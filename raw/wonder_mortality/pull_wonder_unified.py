"""
pull_wonder_unified.py
======================
Pull CDC WONDER mortality data from either:
  • D77  – Multiple Cause of Death, 1999-2020  (final)
  • D176 – Provisional Mortality Statistics, 2018 through Last Week

The script auto-selects the dataset based on PULL_YEARS.
  → All years in 1999-2020  →  D77  (final)
  → All years in 2018-2025  →  D176 (provisional)
  → Mixed ranges that span both datasets are rejected.

USAGE
-----
Edit the CONFIG block below, then run:

    python pull_wonder_unified.py

The script makes one HTTP POST per year (to stay within WONDER's 75,000-row
limit for county-level queries), parses the returned CSV, strips footer rows,
and upserts into a Parquet database.

CAUSES SYNTAX
-------------
    CAUSES = ["all"]                  → all causes
    CAUSES = ["external"]             → external causes only  (V01-Y89)
    CAUSES = ["!external"]            → everything EXCEPT external
    CAUSES = ["mental", "external"]   → mental + external (OR filter)
    CAUSES = ["!mental", "!external"] → everything except mental and external

Keys are from ICD_CHAPTERS.  Raw ICD range strings also work,
e.g. CAUSES = ["V01-Y89"].

OUTPUT FILE
-----------
The Parquet file is named automatically from the config:
    <OUTPUT_DIR>/<LABEL>.parquet
"""

import io
import json
import os
import re
import sys
import subprocess
import textwrap
import time
from pathlib import Path
from urllib.parse import quote_plus

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

# ---------------------------------------------------------------------------
# Import BOTH codebooks under namespaced aliases
# ---------------------------------------------------------------------------
sys.path.insert(0, str(Path(__file__).parent))
import wonder_codebook          as cb_final
import wonder_codebook_provisional as cb_prov


# ============================================================================
# YEAR-RANGE VALIDATION  &  DATASET SELECTION
# ============================================================================
FINAL_YEARS = set(range(1999, 2021))        # D77: 1999-2020
PROVISIONAL_YEARS = set(range(2018, 2026))   # D176: 2018-2025

def _detect_dataset(years: list[int]) -> str:
    """
    Return "D77" or "D176" based on the years requested.
    Raises ValueError if the years span both datasets.
    """
    year_set = set(years)
    in_final = year_set <= FINAL_YEARS
    in_prov  = year_set <= PROVISIONAL_YEARS

    if in_final and not in_prov:
        return "D77"
    if in_prov and not in_final:
        return "D176"
    if in_final and in_prov:
        # Ambiguous (years in the 2018-2020 overlap).  Default to final.
        return "D77"

    # Some years are only in final, some only in provisional → invalid.
    only_final = year_set - PROVISIONAL_YEARS
    only_prov  = year_set - FINAL_YEARS
    raise ValueError(
        f"PULL_YEARS spans both datasets and cannot be served by a single "
        f"WONDER query.\n"
        f"  Years only in D77 (final, 1999-2020): {sorted(only_final)}\n"
        f"  Years only in D176 (provisional, 2018-2025): {sorted(only_prov)}\n"
        f"Split your request into two separate runs."
    )

WORKING_CURL = """
curl 'https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=2C2D2A5339F8748FBC53FEEE1B3E' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -H 'DNT: 1' \
  -H 'Origin: https://wonder.cdc.gov' \
  -H 'Referer: https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=2C2D2A5339F8748FBC53FEEE1B3E' \
  -H 'Upgrade-Insecure-Requests: 1' \
  -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36' \
  -H 'sec-ch-ua: "Google Chrome";v="143", "Chromium";v="143", "Not A(Brand";v="24"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "macOS"' \
  --data-raw 'saved_id=&dataset_code=D77&dataset_label=Multiple+Cause+of+Death%2C+1999-2020&dataset_vintage=2020&stage=request&O_javascript=on&M_1=D77.M1&M_2=D77.M2&M_3=D77.M3&O_aar=aar_none&B_1=D77.V10-level1&B_2=D77.V1-level1&B_3=*None*&B_4=*None*&B_5=*None*&O_title=&O_oc-sect1-request=close&O_rate_per=100000&O_aar_pop=0000&VM_D77.M6_D77.V1_S=*All*&VM_D77.M6_D77.V7=*All*&VM_D77.M6_D77.V17=*All*&VM_D77.M6_D77.V8=*All*&VM_D77.M6_D77.V10=&O_location=D77.V9&finder-stage-D77.V9=codeset&O_V9_fmode=freg&V_D77.V9=&F_D77.V9=*All*&I_D77.V9=*All*+%28The+United+States%29%0D%0A&finder-stage-D77.V10=codeset&O_V10_fmode=freg&V_D77.V10=&F_D77.V10=*All*&I_D77.V10=*All*+%28The+United+States%29%0D%0A&finder-stage-D77.V27=codeset&O_V27_fmode=freg&V_D77.V27=&F_D77.V27=*All*&I_D77.V27=*All*+%28The+United+States%29%0D%0A&O_urban=D77.V19&V_D77.V19=*All*&V_D77.V11=*All*&O_age=D77.V5&V_D77.V5=*All*&V_D77.V51=*All*&V_D77.V52=*All*&V_D77.V6=00&V_D77.V7=*All*&V_D77.V17=*All*&V_D77.V8=*All*&finder-stage-D77.V1=codeset&O_V1_fmode=freg&V_D77.V1=&F_D77.V1=*All*&I_D77.V1=*All*+%28All+Dates%29%0D%0A&V_D77.V24=*All*&V_D77.V20=*All*&V_D77.V21=*All*&O_ucd=D77.V4&finder-stage-D77.V2=codeset&O_V2_fmode=freg&V_D77.V2=&F_D77.V2=*All*&I_D77.V2=*All*+%28All+Causes+of+Death%29%0D%0A&V_D77.V4=GR113-122&V_D77.V4=GR113-124&V_D77.V4=GR113-127&V_D77.V12=*All*&V_D77.V22=*All*&V_D77.V23=*All*&finder-stage-D77.V25=codeset&O_V25_fmode=freg&V_D77.V25=&F_D77.V25=*All*&I_D77.V25=All+Causes+of+Death%0D%0A&O_mcd=D77.V13&finder-stage-D77.V13=codeset&O_V13_fmode=fadv&V_D77.V13=&V_D77.V13_AND=&F_D77.V13=*All*&finder-stage-D77.V15=&O_V15_fmode=fadv&V_D77.V15=&V_D77.V15_AND=&L_D77.V15=*All*&finder-stage-D77.V16=&O_V16_fmode=fadv&V_D77.V16=&V_D77.V16_AND=&L_D77.V16=*All*&finder-stage-D77.V26=codeset&O_V26_fmode=fadv&V_D77.V26=&V_D77.V26_AND=&F_D77.V26=*All*&O_change_action-Send-Export+Results=Export+Results&O_export-format=csv&O_show_totals=true&O_show_zeros=true&O_show_suppressed=true&O_precision=1&O_timeout=600&action-Send=Send'
  """



# Map 5-year labels → enclosing 10-year group (for AAR, which requires V5)
_AGE_5_TO_10 = {
    "<1": "<1", "1-4": "1-4",
    "5-9": "5-14",   "10-14": "5-14",
    "15-19": "15-24", "20-24": "15-24",
    "25-29": "25-34", "30-34": "25-34",
    "35-39": "35-44", "40-44": "35-44",
    "45-49": "45-54", "50-54": "45-54",
    "55-59": "55-64", "60-64": "55-64",
    "65-69": "65-74", "70-74": "65-74",
    "75-79": "75-84", "80-84": "75-84",
    "85+": "85+", "85-89": "85+", "90-94": "85+", "95-99": "85+", "100+": "85+",
}

ucd_options = {
    "all":      
        ["all"],
    "DOD": [   
        # Standard Deaths of Despair preset (Case & Deaton definition).
        # WONDER V2 uses 3-character ICD-10 codes (level 3, e.g. "X40").
        "X40","X41","X42","X43","X44",   # Accidental poisoning by drugs
        "X45",                           # Accidental poisoning by alcohol
        "X60","X61","X62","X63","X64",   # Intentional self-poisoning by drugs
        "X65",                           # Intentional self-poisoning by alcohol
        "X70","X71","X72","X73","X74",   # Intentional self-harm (suicide)
        "X75","X76","X77","X78","X79",
        "X80","X81","X82","X83","X84",
        "Y10","Y11","Y12","Y13","Y14",   # Undetermined intent, drugs
        "Y15",                           # Undetermined intent, alcohol
        "K70",                           # Alcoholic liver disease
        "K73","K74",                     # Chronic hepatitis / cirrhosis of liver
    ]
}

age_options = {
    "all": ["all"],
    "adults":  ["20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"],
    "elderly": ["60-64","65-69","70-74","75-79","80-84","85+"],
}


# ============================================================================
#  ██████╗ ██████╗ ███╗   ██╗███████╗██╗ ██████╗
# ██╔════╝██╔═══██╗████╗  ██║██╔════╝██║██╔════╝
# ██║     ██║   ██║██╔██╗ ██║█████╗  ██║██║  ███╗
# ██║     ██║   ██║██║╚██╗██║██╔══╝  ██║██║   ██║
# ╚██████╗╚██████╔╝██║ ╚████║██║     ██║╚██████╔╝
#  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝ ╚═════╝
# ============================================================================

# --- Output file ---
OUTPUT_DIR = Path(__file__).parent / "data"
LABEL = "alladults"

# --- Geography / grouping ---
REGION   = "county"       # "county" | "state" | "census_region" | "hhs_region"

# --- Demographics to include as GROUP BY columns (leave empty list for totals only) ---
GROUP_DIMS = []
# GROUP_DIMS = ["sex", "race", "hispanic_origin"]

# --- Age-adjusted mortality rate ---
# Age adjusted rates are NOT available in provisional data when REGION="county"
AGE_ADJUSTED     = False          
AAR_STANDARD_POP = "0000"        # "0000" = 2000 U.S. Std. Population

# --- Filters ---
FILTER_SEX       = ["all"]
FILTER_HISPANIC  = ["all"]
FILTER_RACE      = ["all"]
FILTER_AGE       = age_options["adults"]

# --- Underlying Cause of Death (UCD) filter ---
USE_113_LIST = False                # When True, O_ucd switches from V2 (raw ICD-10) to V4 (113 Cause List).
                                    # UCD_CODES should then contain GR113-xxx codes, e.g. ["GR113-124"] for suicide.
UCD_CODES    = ucd_options["all"]

# --- Multiple Cause of death filter (MCD) ---
CAUSES = ["all"]

# --- Years to pull ---
# Set any subset of 1999-2020 (final) OR 2018-2025 (provisional). Do NOT mix years across both ranges.
PULL_YEARS = list(range(1999, 2021))



# ============================================================================
# END CONFIG 
# ============================================================================



## Validate YEARS immediately
DATASET = _detect_dataset(PULL_YEARS)

# Select the correct codebook based on dataset
if DATASET == "D77":
    cb = cb_final
else:
    cb = cb_prov

GROUP_BY           = cb.GROUP_BY
SEX                = cb.SEX
HISPANIC_ORIGIN    = cb.HISPANIC_ORIGIN
RACE               = cb.RACE
AGE_10YR           = cb.AGE_10YR
AGE_5YR            = cb.AGE_5YR
ICD_CHAPTERS       = cb.ICD_CHAPTERS
NON_EXTERNAL_CHAPTERS = cb.NON_EXTERNAL_CHAPTERS
LOCATION_RADIO     = cb.LOCATION_RADIO

print(f"Dataset      : {DATASET}  "
      f"({'Final' if DATASET == 'D77' else 'Provisional'})")
print(f"Years        : {min(PULL_YEARS)}-{max(PULL_YEARS)}")


# ============================================================================
# Helpers (shared)
# ============================================================================

def _resolve_keys(keys: list, mapping: dict, name: str) -> list:
    out = []
    for k in keys:
        if k in mapping:
            out.append(mapping[k])
        elif k.startswith("!"):
            inner = k[1:]
            if inner not in mapping:
                raise ValueError(f"Unknown {name} key: {inner!r}")
            out.append(f"!{mapping[inner]}")
        else:
            out.append(k)
    return out


def _resolve_causes(causes: list) -> list:
    all_chapters = [v for k, v in ICD_CHAPTERS.items() if k != "all"]
    includes, excludes = set(), set()
    for c in causes:
        if c in ("all", "*All*"):
            return ["*All*"]
        if c.startswith("!"):
            excludes.add(ICD_CHAPTERS.get(c[1:], c[1:]))
        else:
            includes.add(ICD_CHAPTERS.get(c, c))
    if excludes and not includes:
        return [ch for ch in all_chapters if ch not in excludes]
    if includes and not excludes:
        return list(includes)
    if includes and excludes:
        return list(includes - excludes)
    return ["*All*"]


def _auto_label() -> str:
    region_str = REGION
    dims_str   = "_".join(GROUP_DIMS) if GROUP_DIMS else "no_dims"
    race_str   = "_".join(FILTER_RACE) if FILTER_RACE != ["all"] else "all_races"
    age_str    = "_".join(FILTER_AGE) if FILTER_AGE != ["all"] else "all_ages"
    cause_str  = "_".join(CAUSES).replace("!", "non_")
    if cause_str == "all":
        cause_str = "all_causes"
    suffix = "" if DATASET == "D77" else "_provisional"
    return f"{region_str}_{dims_str}_{age_str}_{race_str}_{cause_str}{suffix}"


def _build_group_by_params(group_dims: list) -> dict:
    dims = [GROUP_BY[REGION]] + [GROUP_BY[d] for d in group_dims]
    dims += ["*None*"] * (5 - len(dims))
    return {f"B_{i+1}": v for i, v in enumerate(dims[:5])}


def _encode_pairs(pairs: list[tuple]) -> str:
    parts = []
    for k, v in pairs:
        parts.append(f"{quote_plus(k, safe='*')}={quote_plus(str(v), safe='*')}")
    return "&".join(parts)


# ============================================================================
# UCD helper (shared by D77 and D176)
# ============================================================================

def _build_ucd_pairs(D: str) -> list:
    """
    Build the Underlying Cause of Death section of the POST body.
    Reads UCD_CODES, USE_113_LIST from module-level CONFIG.

    - UCD_CODES=["all"]     → no UCD filter (all causes)
    - USE_113_LIST=False    → V2 filter (raw ICD-10 codes, e.g. "X40")
    - USE_113_LIST=True     → V4 filter (ICD-10 113 Cause List, e.g. "GR113-124")

    D77 includes V24 (weekday); D176 does not.
    """
    all_causes = UCD_CODES in (["all"], ["*All*"])
    pairs = []

    if D == "D77":
        pairs.append((f"V_{D}.V24", "*All*"))
    pairs += [
        (f"V_{D}.V20", "*All*"),
        (f"V_{D}.V21", "*All*"),
    ]

    if USE_113_LIST and not all_causes:
        # V4: ICD-10 113 Cause List.
        # V2 finder block is always sent as *All*; codes go in as V_{D}.V4 (NOT F_).
        # No finder-stage-D77.V4 block — WONDER doesn't expect one.
        pairs += [
            ("O_ucd",                f"{D}.V4"),
            (f"finder-stage-{D}.V2", "codeset"),
            ("O_V2_fmode",           "freg"),
            (f"V_{D}.V2",            ""),
            (f"F_{D}.V2",            "*All*"),
            (f"I_{D}.V2",            "*All* (All Causes of Death)\r\n"),
        ]
        for code in UCD_CODES:
            pairs.append((f"V_{D}.V4", code))
    else:
        # V2: raw ICD-10 hierarchy
        pairs += [
            ("O_ucd",                f"{D}.V2"),
            (f"finder-stage-{D}.V2", "codeset"),
            ("O_V2_fmode",           "freg"),
            (f"V_{D}.V2",            ""),
        ]
        if all_causes:
            pairs += [
                (f"F_{D}.V2", "*All*"),
                (f"I_{D}.V2", "*All* (All Causes of Death)\r\n"),
            ]
        else:
            for code in UCD_CODES:
                pairs.append((f"F_{D}.V2", code))
            pairs.append((f"I_{D}.V2", "; ".join(UCD_CODES) + "\r\n"))
        pairs.append((f"V_{D}.V4", "*All*"))

    pairs += [
        (f"V_{D}.V12",             "*All*"),
        (f"V_{D}.V22",             "*All*"),
        (f"V_{D}.V23",             "*All*"),
        (f"finder-stage-{D}.V25", "codeset"),
        ("O_V25_fmode",           "freg"),
        (f"V_{D}.V25",            ""),
        (f"F_{D}.V25",            "*All*"),
        (f"I_{D}.V25",            "All Causes of Death\r\n"),
    ]
    return pairs


# ============================================================================
# D77 (Final) — filter params & post body
# ============================================================================

def _build_filter_params_d77(sex_vals, hispanic_vals, race_vals,
                             age_vals, cause_codes) -> list:
    D = "D77"
    pairs = []

    # Location: all US
    pairs += [
        (f"finder-stage-{D}.V9",  "codeset"),
        ("O_V9_fmode",            "freg"),
        (f"V_{D}.V9",             ""),
        (f"F_{D}.V9",             "*All*"),
        (f"I_{D}.V9",             "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V10", "codeset"),
        ("O_V10_fmode",           "freg"),
        (f"V_{D}.V10",            ""),
        (f"F_{D}.V10",            "*All*"),
        (f"I_{D}.V10",            "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V27", "codeset"),
        ("O_V27_fmode",           "freg"),
        (f"V_{D}.V27",            ""),
        (f"F_{D}.V27",            "*All*"),
        (f"I_{D}.V27",            "*All* (The United States)\r\n"),
    ]

    # Urbanization
    pairs += [
        ("O_urban",       f"{D}.V19"),
        (f"V_{D}.V19",    "*All*"),
        (f"V_{D}.V11",    "*All*"),
    ]

    # Age — V5 (10-year) required for AAR; V51 (5-year) otherwise
    if AGE_ADJUSTED:
        pairs += [("O_age", f"{D}.V5")]
        for v in age_vals:
            pairs.append((f"V_{D}.V5", v))
        pairs += [(f"V_{D}.V51", "*All*"), (f"V_{D}.V52", "*All*"), (f"V_{D}.V6", "00")]
    else:
        pairs += [("O_age", f"{D}.V51")]
        for v in age_vals:
            pairs.append((f"V_{D}.V51", v))
        pairs += [(f"V_{D}.V5", "*All*"), (f"V_{D}.V52", "*All*"), (f"V_{D}.V6", "00")]

    # Sex
    for v in sex_vals:
        pairs.append((f"V_{D}.V7", v))

    # Hispanic Origin
    for v in hispanic_vals:
        pairs.append((f"V_{D}.V17", v))

    # Race (D77 uses V8)
    for v in race_vals:
        pairs.append((f"V_{D}.V8", v))

    return pairs


def _build_post_body_d77(year, group_by_params,
                         sex_vals, hispanic_vals, race_vals,
                         age_vals, cause_codes) -> str:
    D = "D77"
    fixed = [
        ("saved_id",        ""),
        ("dataset_code",    D),
        ("dataset_label",   "Multiple Cause of Death, 1999-2020"),
        ("dataset_vintage", "2020"),
        ("stage",           "request"),
        ("O_javascript",    "on"),
        ("M_1", f"{D}.M1"), ("M_2", f"{D}.M2"), ("M_3", f"{D}.M3"),
        *([(("M_4", f"{D}.M4"))] if AGE_ADJUSTED else []),
        ("O_aar",           "aar_std" if AGE_ADJUSTED else "aar_none"),
    ]
    for k in ("B_1","B_2","B_3","B_4","B_5"):
        fixed.append((k, group_by_params[k]))

    fixed += [
        ("O_title",                 ""),
        ("O_oc-sect1-request",      "close"),
        ("O_rate_per",              "100000"),
        ("O_aar_pop",               AAR_STANDARD_POP),
        (f"VM_{D}.M6_{D}.V1_S",    "*All*"),
        (f"VM_{D}.M6_{D}.V7",      "*All*"),
        (f"VM_{D}.M6_{D}.V17",     "*All*"),
        (f"VM_{D}.M6_{D}.V8",      "*All*"),
        (f"VM_{D}.M6_{D}.V10",     ""),
        ("O_location",             LOCATION_RADIO.get("state", f"{D}.V9")),
    ]

    filter_pairs = _build_filter_params_d77(
        sex_vals, hispanic_vals, race_vals, age_vals, cause_codes)

    year_pairs = [
        (f"finder-stage-{D}.V1", "codeset"),
        ("O_V1_fmode",           "freg"),
        (f"V_{D}.V1",            ""),
        (f"F_{D}.V1",            year),
        (f"I_{D}.V1",            f"{year} ({year})\r\n"),
    ]

    ucd_pairs = _build_ucd_pairs(D)

    mcd_pairs = [
        ("O_mcd",                 f"{D}.V13"),
        (f"finder-stage-{D}.V13", "codeset"),
        ("O_V13_fmode",           "fadv"),
        (f"V_{D}.V13",            ""),
        (f"V_{D}.V13_AND",        ""),
    ]
    for code in cause_codes:
        mcd_pairs.append((f"F_{D}.V13", code))

    mcd_pairs += [
        (f"finder-stage-{D}.V15", ""),
        ("O_V15_fmode",           "fadv"),
        (f"V_{D}.V15",            ""),
        (f"V_{D}.V15_AND",        ""),
        (f"L_{D}.V15",            "*All*"),
        (f"finder-stage-{D}.V16", ""),
        ("O_V16_fmode",           "fadv"),
        (f"V_{D}.V16",            ""),
        (f"V_{D}.V16_AND",        ""),
        (f"L_{D}.V16",            "*All*"),
        (f"finder-stage-{D}.V26", "codeset"),
        ("O_V26_fmode",           "fadv"),
        (f"V_{D}.V26",            ""),
        (f"V_{D}.V26_AND",        ""),
        (f"F_{D}.V26",            "*All*"),
    ]

    export_pairs = [
        ("O_change_action-Send-Export Results", "Export Results"),
        ("O_export-format",    "csv"),
        ("O_show_totals",      "true"),
        ("O_show_zeros",       "true"),
        ("O_show_suppressed",  "true"),
        ("O_precision",        "1"),
        ("O_timeout",          "600"),
        ("action-Send",        "Send"),
    ]

    all_pairs = fixed + filter_pairs + year_pairs + ucd_pairs + mcd_pairs + export_pairs
    return _encode_pairs(all_pairs)


# ============================================================================
# D176 (Provisional) — filter params & post body
# ============================================================================

def _build_filter_params_d176(sex_vals, hispanic_vals, race_vals,
                              age_vals, cause_codes) -> list:
    D = "D176"
    pairs = []

    # Residence location: all US
    pairs += [
        (f"finder-stage-{D}.V9",  "codeset"),
        ("O_V9_fmode",            "freg"),
        (f"V_{D}.V9",             ""),
        (f"F_{D}.V9",             "*All*"),
        (f"I_{D}.V9",             "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V10", "codeset"),
        ("O_V10_fmode",           "freg"),
        (f"V_{D}.V10",            ""),
        (f"F_{D}.V10",            "*All*"),
        (f"I_{D}.V10",            "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V27", "codeset"),
        ("O_V27_fmode",           "freg"),
        (f"V_{D}.V27",            ""),
        (f"F_{D}.V27",            "*All*"),
        (f"I_{D}.V27",            "*All* (The United States)\r\n"),
    ]

    # Urbanization (residence) — D176 adds V18 (2023 classification)
    pairs += [
        ("O_urban",       f"{D}.V19"),
        (f"V_{D}.V19",    "*All*"),
        (f"V_{D}.V11",    "*All*"),
        (f"V_{D}.V18",    "*All*"),
    ]

    # Death (occurrence) location — D176 only
    pairs += [
        ("O_death_location",         f"{D}.V79"),
        (f"finder-stage-{D}.V79",    "codeset"),
        ("O_V79_fmode",              "freg"),
        (f"V_{D}.V79",               ""),
        (f"F_{D}.V79",               "*All*"),
        (f"I_{D}.V79",               "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V80",    "codeset"),
        ("O_V80_fmode",              "freg"),
        (f"V_{D}.V80",               ""),
        (f"F_{D}.V80",               "*All*"),
        (f"I_{D}.V80",               "*All* (The United States)\r\n"),
        (f"finder-stage-{D}.V77",    "codeset"),
        ("O_V77_fmode",              "freg"),
        (f"V_{D}.V77",               ""),
        (f"F_{D}.V77",               "*All*"),
        (f"I_{D}.V77",               "*All* (The United States)\r\n"),
    ]

    # Death urbanization — D176 only
    pairs += [
        ("O_death_urban", f"{D}.V89"),
        (f"V_{D}.V89",    "*All*"),
        (f"V_{D}.V81",    "*All*"),
        (f"V_{D}.V82",    "*All*"),
    ]

    # Age — V5 (10-year) required for AAR; V51 (5-year) otherwise
    if AGE_ADJUSTED:
        pairs += [("O_age", f"{D}.V5")]
        for v in age_vals:
            pairs.append((f"V_{D}.V5", v))
        pairs += [(f"V_{D}.V51", "*All*"), (f"V_{D}.V52", "*All*"), (f"V_{D}.V6", "00")]
    else:
        pairs += [("O_age", f"{D}.V51")]
        for v in age_vals:
            pairs.append((f"V_{D}.V51", v))
        pairs += [(f"V_{D}.V5", "*All*"), (f"V_{D}.V52", "*All*"), (f"V_{D}.V6", "00")]

    # Sex
    for v in sex_vals:
        pairs.append((f"V_{D}.V7", v))

    # Hispanic Origin
    for v in hispanic_vals:
        pairs.append((f"V_{D}.V17", v))

    # Race — D176 uses V42 (Single Race 6), plus V43/V44
    pairs.append(("O_race", f"{D}.V42"))
    for v in race_vals:
        pairs.append((f"V_{D}.V42", v))
    pairs += [
        (f"V_{D}.V43", "*All*"),
        (f"V_{D}.V44", "*All*"),
    ]

    # Date selection
    pairs.append(("O_dates", "YEAR"))

    return pairs


def _build_post_body_d176(year, group_by_params,
                          sex_vals, hispanic_vals, race_vals,
                          age_vals, cause_codes,
                          dataset_vintage: str = "") -> str:
    D = "D176"
    fixed = [
        ("saved_id",        ""),
        ("dataset_code",    D),
        ("dataset_label",   "Provisional Mortality Statistics, 2018 through Last Week"),
        ("O_MMWR",          "false"),
        ("dataset_vintage", dataset_vintage),
        ("stage",           "request"),
        ("O_javascript",    "on"),
        ("M_1", f"{D}.M1"), ("M_2", f"{D}.M2"), ("M_3", f"{D}.M3"),
        *([(("M_4", f"{D}.M4"))] if AGE_ADJUSTED else []),
        ("O_aar",           "aar_std" if AGE_ADJUSTED else "aar_none"),
    ]
    for k in ("B_1","B_2","B_3","B_4","B_5"):
        fixed.append((k, group_by_params[k]))

    fixed += [
        ("O_title",                 ""),
        ("O_oc-sect1-request",      "close"),
        ("O_rate_per",              "100000"),
        ("O_aar_pop",               AAR_STANDARD_POP),
        (f"VM_{D}.M6_{D}.V1_S",    "*All*"),
        (f"VM_{D}.M6_{D}.V7",      "*All*"),
        (f"VM_{D}.M6_{D}.V17",     "*All*"),
        (f"VM_{D}.M6_{D}.V42",     "*All*"),
        (f"VM_{D}.M6_{D}.V10",     ""),
        ("O_location",             LOCATION_RADIO.get("state", f"{D}.V9")),
    ]

    filter_pairs = _build_filter_params_d176(
        sex_vals, hispanic_vals, race_vals, age_vals, cause_codes)

    year_pairs = [
        (f"finder-stage-{D}.V1",  "codeset"),
        ("O_V1_fmode",            "freg"),
        (f"V_{D}.V1",             ""),
        (f"F_{D}.V1",             year),
        (f"I_{D}.V1",             f"{year} ({year})\r\n"),
        # MMWR dates: all
        (f"finder-stage-{D}.V100", "codeset"),
        ("O_V100_fmode",           "freg"),
        (f"V_{D}.V100",            ""),
        (f"F_{D}.V100",            "*All*"),
        (f"I_{D}.V100",            "*All* (All Dates)\r\n"),
    ]

    # UCD — D176 has no V24 (weekday); handled inside _build_ucd_pairs
    ucd_pairs = _build_ucd_pairs(D)

    mcd_pairs = [
        ("O_mcd",                 f"{D}.V13"),
        (f"finder-stage-{D}.V13", "codeset"),
        ("O_V13_fmode",           "fadv"),
        (f"V_{D}.V13",            ""),
        (f"V_{D}.V13_AND",        ""),
    ]
    for code in cause_codes:
        mcd_pairs.append((f"F_{D}.V13", code))

    mcd_pairs += [
        (f"finder-stage-{D}.V15", ""),
        ("O_V15_fmode",           "fadv"),
        (f"V_{D}.V15",            ""),
        (f"V_{D}.V15_AND",        ""),
        (f"L_{D}.V15",            "*All*"),
        (f"finder-stage-{D}.V16", ""),
        ("O_V16_fmode",           "fadv"),
        (f"V_{D}.V16",            ""),
        (f"V_{D}.V16_AND",        ""),
        (f"L_{D}.V16",            "*All*"),
        (f"finder-stage-{D}.V26", "codeset"),
        ("O_V26_fmode",           "fadv"),
        (f"V_{D}.V26",            ""),
        (f"V_{D}.V26_AND",        ""),
        (f"F_{D}.V26",            "*All*"),
    ]

    export_pairs = [
        ("O_change_action-Send-Export Results", "Export Results"),
        ("O_export-format",    "csv"),
        ("O_show_totals",      "true"),
        ("O_show_zeros",       "true"),
        ("O_show_suppressed",  "true"),
        ("O_precision",        "1"),
        ("O_timeout",          "600"),
        ("action-Send",        "Send"),
    ]

    all_pairs = fixed + filter_pairs + year_pairs + ucd_pairs + mcd_pairs + export_pairs
    return _encode_pairs(all_pairs)


# ============================================================================
# CSV Parsing (shared)
# ============================================================================

def _parse_wonder_csv(raw_text: str) -> pd.DataFrame:
    try:
        df_raw = pd.read_csv(
            io.StringIO(raw_text),
            dtype=str,
            keep_default_na=False,
        )
    except Exception as e:
        raise RuntimeError(f"CSV parse error: {e}\nFirst 500 chars:\n{raw_text[:500]}")

    if "Notes" not in df_raw.columns:
        raise RuntimeError(f"Unexpected CSV columns: {df_raw.columns.tolist()}")

    df = df_raw[df_raw["Notes"].str.strip() == ""].copy()
    df = df.drop(columns=["Notes"])
    df.columns = [c.strip() for c in df.columns]

    for col in ["Deaths", "Population", "Crude Rate", "Age Adjusted Rate"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(how="all")
    return df


# ============================================================================
# Session management via Playwright
# ============================================================================

# Dataset-specific config for Playwright session
_SESSION_CFG = {
    "D77": {
        "agreement_url": "https://wonder.cdc.gov/mcd-icd10.html",
        "url_pattern":   "datarequest/D77",
    },
    "D176": {
        "agreement_url": "https://wonder.cdc.gov/mcd-icd10-provisional.html",
        "url_pattern":   "datarequest/D176",
    },
}

def _open_wonder_session(dataset: str) -> tuple[str, str, str]:
    """
    Open a Playwright browser, navigate to the WONDER agreement page for the
    given dataset, click "I Agree", and capture jsessionid + cookies.

    For D176, also captures dataset_vintage from the hidden form field.

    Returns (jsession_id, cookie_string, dataset_vintage).
    dataset_vintage is "" for D77 (which uses a fixed "2020" value).
    """
    from playwright.sync_api import sync_playwright

    cfg = _SESSION_CFG[dataset]
    captured = {}

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context()
        page = context.new_page()

        def handle_request(request):
            if cfg["url_pattern"] in request.url and request.method == "POST":
                m = re.search(r"jsessionid=([A-F0-9]+)", request.url)
                if m:
                    captured["jsession_id"] = m.group(1)

        page.on("request", handle_request)

        print(f"  Opening WONDER agreement page ({dataset}) ...", flush=True)
        page.goto(cfg["agreement_url"])

        page.click('input[value="I Agree"]')
        page.wait_for_selector("#submit-button1", state="visible")
        page.wait_for_timeout(2000)

        # Capture dataset_vintage for D176
        vintage = ""
        if dataset == "D176":
            try:
                vintage = page.input_value('input[name="dataset_vintage"]')
            except Exception:
                vintage = ""
        captured["dataset_vintage"] = vintage

        page.click("#submit-button1")
        page.wait_for_timeout(2000)

        browser_cookies = context.cookies("https://wonder.cdc.gov")
        cookie_str = "; ".join(
            f"{c['name']}={c['value']}" for c in browser_cookies
        )
        captured["cookies"] = cookie_str
        browser.close()

    print(captured)

    if "jsession_id" not in captured:
        raise RuntimeError(
            f"Failed to capture jsessionid from WONDER ({dataset})")

    print(f"  Session ID : {captured['jsession_id']}")
    print(f"  Cookies    : {captured['cookies'][:80]}...")
    return (captured["jsession_id"],
            captured["cookies"],
            captured.get("dataset_vintage", ""))


# ============================================================================
# HTTP call via curl
# ============================================================================

def _build_curl_headers_d77(jsession_id: str) -> list[str]:
    return [
        "accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "accept-language: en-US,en;q=0.9",
        "cache-control: max-age=0",
        "content-type: application/x-www-form-urlencoded",
        "dnt: 1",
        "origin: https://wonder.cdc.gov",
        "priority: u=0, i",
        f"referer: https://wonder.cdc.gov/controller/datarequest/D77;jsessionid={jsession_id}",
        'sec-ch-ua: "Google Chrome";v="143", "Chromium";v="143", "Not A(Brand";v="24"',
        "sec-ch-ua-mobile: ?0",
        'sec-ch-ua-platform: "macOS"',
        "sec-fetch-dest: document",
        "sec-fetch-mode: navigate",
        "sec-fetch-site: same-origin",
        "sec-fetch-user: ?1",
        "upgrade-insecure-requests: 1",
        "user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36",
    ]


def _build_curl_headers_d176(jsession_id: str) -> list[str]:
    return [
        "Content-Type: application/x-www-form-urlencoded",
        "DNT: 1",
        "Origin: https://wonder.cdc.gov",
        "Referer: https://wonder.cdc.gov/controller/datarequest/D176",
        "Upgrade-Insecure-Requests: 1",
        "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36",
        'sec-ch-ua: "Google Chrome";v="143", "Chromium";v="143", "Not A(Brand";v="24"',
        "sec-ch-ua-mobile: ?0",
        'sec-ch-ua-platform: "macOS"',
    ]


def _fetch_year(year: str, post_body: str,
                jsession_id: str, cookies: str,
                dataset: str) -> str:
    """Execute curl and return the raw response text."""
    wonder_url = f"https://wonder.cdc.gov/controller/datarequest/{dataset};jsessionid={jsession_id}"

    if dataset == "D77":
        headers = _build_curl_headers_d77(jsession_id)
    else:
        headers = _build_curl_headers_d176(jsession_id)

    cmd = ["curl", "-s", "-X", "POST", wonder_url]
    for h in headers:
        cmd += ["-H", h]
    cmd += ["-b", cookies]
    cmd += ["--data-raw", post_body]

    result = subprocess.run(cmd, capture_output=True, timeout=300)
    if result.returncode != 0:
        with open(OUTPUT_DIR / "curl_output.html", "w") as f:
            f.write(result.stderr.decode("latin-1", errors="replace"))
        raise RuntimeError(
            f"curl failed (code {result.returncode}): {result.stderr[:400]}"
        )
    return result.stdout.decode("latin-1", errors="replace")


# ============================================================================
# Main
# ============================================================================

# ---------------------------------------------------------------------------
# Credential caching
# ---------------------------------------------------------------------------
_CRED_FILE = Path(__file__).parent / ".wonder_session.json"
_CRED_MAX_AGE = 28 * 60   # seconds

def _load_cached_credentials(dataset: str) -> tuple[str, str, str] | None:
    """Return (jsession_id, cookies, dataset_vintage) if cache is fresh, else None."""
    if not _CRED_FILE.exists():
        return None
    try:
        data = json.loads(_CRED_FILE.read_text())
        if data.get("dataset") != dataset:
            return None
        age = time.time() - data["timestamp"]
        if age > _CRED_MAX_AGE:
            print(f"  Cached credentials expired ({age/60:.1f} min old).")
            return None
        print(f"  Reusing cached credentials ({age/60:.1f} min old).")
        return data["jsession_id"], data["cookies"], data.get("dataset_vintage", "")
    except (json.JSONDecodeError, KeyError):
        return None

def _save_credentials(dataset: str, jsession_id: str, cookies: str,
                      dataset_vintage: str) -> None:
    _CRED_FILE.write_text(json.dumps({
        "dataset": dataset,
        "jsession_id": jsession_id,
        "cookies": cookies,
        "dataset_vintage": dataset_vintage,
        "timestamp": time.time(),
    }, indent=2))


def main():
    label = LABEL or _auto_label()
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    out_path = OUTPUT_DIR / f"{label}.parquet"

    # --- Open (or reuse) a WONDER session ---
    print("Opening WONDER session ...")
    cached = _load_cached_credentials(DATASET)
    if cached:
        jsession_id, cookies, dataset_vintage = cached
    else:
        jsession_id, cookies, dataset_vintage = _open_wonder_session(DATASET)
        _save_credentials(DATASET, jsession_id, cookies, dataset_vintage)
    print()

    # Resolve filter values
    sex_vals      = [SEX[k] for k in FILTER_SEX]
    hispanic_vals = [HISPANIC_ORIGIN[k] for k in FILTER_HISPANIC]
    race_vals     = [RACE[k] for k in FILTER_RACE]
    cause_codes   = _resolve_causes(CAUSES)

    # Age values: 10-year groups for AAR, 5-year otherwise
    if AGE_ADJUSTED:
        labels_10 = list(dict.fromkeys(_AGE_5_TO_10[k] for k in FILTER_AGE))  # unique, order-preserved
        age_vals  = [AGE_10YR.get(k, k) for k in labels_10]
        print(f"  AAR enabled → 10-year age groups: {labels_10}")
    else:
        age_vals = [AGE_5YR.get(k, k) for k in FILTER_AGE]

    group_by_params = _build_group_by_params(GROUP_DIMS)

    print(f"Output file  : {out_path}")
    print(f"Region level : {REGION}  ({GROUP_BY[REGION]})")
    print(f"Group dims   : {GROUP_DIMS}")
    print(f"Causes       : {cause_codes}")
    print(f"Years        : {PULL_YEARS}")
    print()

    frames = []

    for year in PULL_YEARS:
        year_str = str(year)
        print(f"  Pulling year {year_str} ... ", end="", flush=True)

        if DATASET == "D77":
            post_body = _build_post_body_d77(
                year_str, group_by_params,
                sex_vals, hispanic_vals, race_vals, age_vals, cause_codes,
            )
        else:
            post_body = _build_post_body_d176(
                year_str, group_by_params,
                sex_vals, hispanic_vals, race_vals, age_vals, cause_codes,
                dataset_vintage=dataset_vintage,
            )

        raw = _fetch_year(year_str, post_body, jsession_id, cookies, DATASET)

        # Check for WONDER error page
        if "<html" in raw.lower():
            html_path = OUTPUT_DIR / "curl_output.html"
            with open(html_path, "w") as f:
                f.write(raw)
            print(f"ERROR — HTML returned instead of CSV (saved to {html_path})")
            continue

        try:
            df = _parse_wonder_csv(raw)
        except RuntimeError as e:
            print(f"PARSE ERROR: {e}")
            continue

        if df.empty:
            print("empty result — skipping")
            continue

        # Tag with metadata
        df["query_label"]  = label
        df["cause_filter"] = ";".join(cause_codes)
        df["Year"] = year
        df["dataset"] = DATASET

        frames.append(df)
        print(f"{len(df):,} rows")

    if not frames:
        print("No data retrieved.")
        return

    combined = pd.concat(frames, ignore_index=True)
    print(f"\nTotal rows: {combined.shape[0]:,}")

    # --- Write / append to Parquet ---
    table = pa.Table.from_pandas(combined, preserve_index=False)

    if out_path.exists():
        existing = pq.read_table(str(out_path))
        combined_all = pd.concat(
            [existing.to_pandas(), combined], ignore_index=True
        )
        key_cols = [c for c in combined_all.columns
                    if c not in ("query_label", "cause_filter")]
        combined_all = combined_all.drop_duplicates(subset=key_cols, keep="last")
        table = pa.Table.from_pandas(combined_all, preserve_index=False)
        print(f"Merged with existing file → {len(combined_all):,} total rows")

    pq.write_table(table, str(out_path), compression="snappy")
    print(f"Saved → {out_path}")


if __name__ == "__main__":
    main()
