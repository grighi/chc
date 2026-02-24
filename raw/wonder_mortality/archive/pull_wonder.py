"""
pull_wonder.py
==============
Pull CDC WONDER Multiple Cause of Death data (D77, 1999-2020) and store
results in a local Parquet file.

USAGE
-----
Edit the CONFIG block below, then run:

    python pull_wonder.py

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

Keys are from wonder_codebook.ICD_CHAPTERS.  Raw ICD range strings also work,
e.g. CAUSES = ["V01-Y89"].

OUTPUT FILE
-----------
The Parquet file is named automatically from the config:
    <OUTPUT_DIR>/<LABEL>.parquet

e.g.  data/county_year_all_ages_all_races_external_causes.parquet
"""

import io
import os
import re
import sys
import subprocess
import textwrap
from pathlib import Path
from urllib.parse import quote_plus

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

# ---------------------------------------------------------------------------
# Import codebook
# ---------------------------------------------------------------------------
sys.path.insert(0, str(Path(__file__).parent))
from wonder_codebook import (
    GROUP_BY, SEX, HISPANIC_ORIGIN, RACE,
    AGE_10YR, AGE_5YR, ICD_CHAPTERS,
    YEARS, STATES, CENSUS_REGIONS, LOCATION_RADIO,
    NON_EXTERNAL_CHAPTERS,
)


WORKING_CURL = """
curl "https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=C86174CAE0083B15EE32733A00ED" \
  -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7' \
  -H 'accept-language: en-US,en;q=0.9' \
  -H 'cache-control: max-age=0' \
  -H 'content-type: application/x-www-form-urlencoded' \
  -b 'akamai_visit_id=4e5ed6174509437f707Z; _ga=GA1.3.788110559.1771445095; _gid=GA1.3.1813801190.1771445095; _cdc_test_c=value; _cdc_vid=76ac3e24-5246-4a47-a35a-abd1b10b2c44; _ga=GA1.1.788110559.1771445095; s_fid=5B0E5723DFD38535-1CFE6F2E39BE43AC; Akamai-Request-ID=4e5ed61745166389316Z; _cdc_vnum=2; _cdc_sid=992f3111-091c-4cb4-b71c-e461c6a7311a; s_vnum=1772352000496%26vn%3D2; s_invisit=true; s_lv_s=Less%20than%201%20day; s_visit=1; s_ptc=%5B%5BB%5D%5D; _gat_GSA_ENOR0=1; _cdc_ppu=https%3A%2F%2Fwonder.cdc.gov%2Fcontroller%2Fdatarequest%2FD77jsessionid%3D86C679BC0FB6DA406D4CEDC0F8DA; gpv_c54=https%3A%2F%2Fwonder.cdc.gov%2Fcontroller%2Fdatarequest%2FD77%3Bjsessionid%3D86C679BC0FB6DA406D4CEDC0F8DA; s_pvs=%5B%5BB%5D%5D; s_tps=%5B%5BB%5D%5D; _cdc_occ=17; _cdc_ppn=Multiple%2520Cause%2520of%2520Death%252C%25201999-2020%2520Request%2520Form; _ga_CSLL4ZEK4L=GS2.1.s1771515846$o2$g1$t1771517469$j49$l0$h0; s_ppvl=Multiple%2520Cause%2520of%2520Death%252C%25201999-2020%2520Results%2520Form%2C14%2C14%2C804%2C833%2C804%2C2560%2C1440%2C1%2CL; gpv_v45=Multiple%20Cause%20of%20Death%2C%201999-2020%20Request%20Form; s_ppv=Multiple%2520Cause%2520of%2520Death%252C%25201999-2020%2520Request%2520Form%2C100%2C100%2C4150%2C833%2C804%2C2560%2C1440%2C1%2CL; s_lv=1771517475778; s_sq=cdcgov%3D%2526pid%253DMultiple%252520Cause%252520of%252520Death%25252C%2525201999-2020%252520Request%252520Form%2526pidt%253D1%2526oid%253Dfunctiononclick%252528event%252529%25257BsubmitSet%252528this%252529%25253B%25257D%2526oidt%253D2%2526ot%253DSUBMIT' \
  -H 'dnt: 1' \
  -H 'origin: https://wonder.cdc.gov' \
  -H 'priority: u=0, i' \
  -H 'referer: https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=86C679BC0FB6DA406D4CEDC0F8DA' \
  -H 'sec-ch-ua: "Google Chrome";v="143", "Chromium";v="143", "Not A(Brand";v="24"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "macOS"' \
  -H 'sec-fetch-dest: document' \
  -H 'sec-fetch-mode: navigate' \
  -H 'sec-fetch-site: same-origin' \
  -H 'sec-fetch-user: ?1' \
  -H 'upgrade-insecure-requests: 1' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36' \
  --data-raw 'saved_id=&dataset_code=D77&dataset_label=Multiple+Cause+of+Death%2C+1999-2020&dataset_vintage=2020&stage=request&O_javascript=on&M_1=D77.M1&M_2=D77.M2&M_3=D77.M3&O_aar=aar_none&B_1=D77.V10-level1&B_2=D77.V1-level1&B_3=*None*&B_4=*None*&B_5=*None*&O_title=&O_oc-sect1-request=close&O_rate_per=100000&O_aar_pop=0000&VM_D77.M6_D77.V1_S=*All*&VM_D77.M6_D77.V7=*All*&VM_D77.M6_D77.V17=*All*&VM_D77.M6_D77.V8=*All*&VM_D77.M6_D77.V10=&O_location=D77.V9&finder-stage-D77.V9=codeset&O_V9_fmode=freg&V_D77.V9=&F_D77.V9=*All*&I_D77.V9=*All*+%28The+United+States%29%0D%0A&finder-stage-D77.V10=codeset&O_V10_fmode=freg&V_D77.V10=&F_D77.V10=*All*&I_D77.V10=*All*+%28The+United+States%29%0D%0A&finder-stage-D77.V27=codeset&O_V27_fmode=freg&V_D77.V27=&F_D77.V27=*All*&I_D77.V27=*All*+%28The+United+States%29%0D%0A&O_urban=D77.V19&V_D77.V19=*All*&V_D77.V11=*All*&O_age=D77.V5&V_D77.V5=*All*&V_D77.V51=*All*&V_D77.V52=*All*&V_D77.V6=00&V_D77.V7=*All*&V_D77.V17=*All*&V_D77.V8=*All*&finder-stage-D77.V1=codeset&O_V1_fmode=freg&V_D77.V1=&F_D77.V1=*All*&I_D77.V1=*All*+%28All+Dates%29%0D%0A&V_D77.V24=*All*&V_D77.V20=*All*&V_D77.V21=*All*&O_ucd=D77.V2&finder-stage-D77.V2=codeset&O_V2_fmode=freg&V_D77.V2=&F_D77.V2=*All*&I_D77.V2=*All*+%28All+Causes+of+Death%29%0D%0A&V_D77.V4=*All*&V_D77.V12=*All*&V_D77.V22=*All*&V_D77.V23=*All*&finder-stage-D77.V25=codeset&O_V25_fmode=freg&V_D77.V25=&F_D77.V25=*All*&I_D77.V25=All+Causes+of+Death%0D%0A&O_mcd=D77.V13&finder-stage-D77.V13=codeset&O_V13_fmode=fadv&V_D77.V13=&V_D77.V13_AND=&F_D77.V13=*All*&finder-stage-D77.V15=&O_V15_fmode=fadv&V_D77.V15=&V_D77.V15_AND=&L_D77.V15=*All*&finder-stage-D77.V16=&O_V16_fmode=fadv&V_D77.V16=&V_D77.V16_AND=&L_D77.V16=*All*&finder-stage-D77.V26=codeset&O_V26_fmode=fadv&V_D77.V26=&V_D77.V26_AND=&F_D77.V26=*All*&O_change_action-Send-Export+Results=Export+Results&O_export-format=csv&O_show_totals=true&O_precision=1&O_timeout=600&action-Send=Send'
"""


# ============================================================================
#  ██████╗ ██████╗ ███╗   ██╗███████╗██╗ ██████╗
# ██╔════╝██╔═══██╗████╗  ██║██╔════╝██║██╔════╝
# ██║     ██║   ██║██╔██╗ ██║█████╗  ██║██║  ███╗
# ██║     ██║   ██║██║╚██╗██║██╔══╝  ██║██║   ██║
# ╚██████╗╚██████╔╝██║ ╚████║██║     ██║╚██████╔╝
#  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝ ╚═════╝
# ============================================================================

# --- Geography / grouping ---
REGION   = "county"       # "county" | "state" | "census_region" | "hhs_region"
                          # This sets the first Group-By dimension.

# --- Demographics to include as GROUP BY columns (leave empty list for totals only) ---
# Each entry adds a Group-By column.  Max 5 group-bys total (REGION counts as one).
GROUP_DIMS = ["sex", "race", "hispanic_origin"]     # e.g. ["year", "age_10yr", "race", "sex", "hispanic_origin"]

# --- Filters: these restrict WHICH rows are returned (not grouped by) ---
age_options = {
    "adults": ["20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"],
    "elderly": ["60-64","65-69","70-74","75-79","80-84","85+"]
}
# Values are keys from the codebook dicts above, or "*All*" for no filter.
FILTER_AGE_LABEL = 'adults'
FILTER_SEX       = ["all"]          # list of SEX keys
FILTER_HISPANIC  = ["all"]          # list of HISPANIC_ORIGIN keys
FILTER_RACE      = ["all"]          # list of RACE keys
FILTER_AGE       =  age_options[FILTER_AGE_LABEL] # either a selection from age_options OR a list of AGE_5YR keys (when age is not a group dim)

# --- Cause of death filter (MCD — Multiple Cause of Death) ---
# Keys from ICD_CHAPTERS, or raw ICD-10 range strings.
# Prefix with "!" to EXCLUDE that chapter (everything except it).
# Examples:
#   CAUSES = ["all"]               → all causes
#   CAUSES = ["external"]          → V01-Y89
#   CAUSES = ["!external"]         → all non-external
#   CAUSES = ["mental", "nervous"] → F01-F99 and G00-G98
CAUSES = ["all"]

# --- Years to pull ---
# PULL_YEARS = list(range(1999, 2021))   # all years
PULL_YEARS = list(range(1999, 2021))     # quick test: 1999-2000

# --- Output ---
OUTPUT_DIR = Path(__file__).parent / "data"

# Optional short label for the output file (auto-generated if empty string).
LABEL = ""

# JSESSION_ID and COOKIES are obtained automatically via Playwright (see _open_wonder_session)


# ============================================================================
# END CONFIG
# ============================================================================


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _resolve_keys(keys: list, mapping: dict, name: str) -> list:
    """Resolve a list of human-readable keys to API parameter values."""
    out = []
    for k in keys:
        if k in mapping:
            out.append(mapping[k])
        elif k.startswith("!"):
            inner = k[1:]
            if inner not in mapping:
                raise ValueError(f"Unknown {name} key: {inner!r}")
            # caller must handle negation
            out.append(f"!{mapping[inner]}")
        else:
            # treat as a raw value (e.g. "V01-Y89")
            out.append(k)
    return out


def _resolve_causes(causes: list) -> list:
    """
    Returns a list of ICD chapter codes to include.
    "!" prefix means: select all OTHER chapters.
    Mixed include/exclude is not supported by WONDER's single filter box,
    so we expand exclusions into explicit inclusions.
    """
    all_chapters = [v for k, v in ICD_CHAPTERS.items() if k != "all"]
    includes = set()
    excludes = set()

    for c in causes:
        if c == "all" or c == "*All*":
            return ["*All*"]
        if c.startswith("!"):
            code = c[1:]
            resolved = ICD_CHAPTERS.get(code, code)
            excludes.add(resolved)
        else:
            resolved = ICD_CHAPTERS.get(c, c)
            includes.add(resolved)

    if excludes and not includes:
        # select everything except excluded
        return [ch for ch in all_chapters if ch not in excludes]
    elif includes and not excludes:
        return list(includes)
    elif includes and excludes:
        # union of includes minus excludes
        return list(includes - excludes)
    else:
        return ["*All*"]


def _auto_label() -> str:
    region_str = REGION
    dims_str   = "_".join(GROUP_DIMS) if GROUP_DIMS else "no_dims"
    race_str   = "_".join(FILTER_RACE) if FILTER_RACE != ["all"] else "all_races"
    age_str    = FILTER_AGE_LABEL  if FILTER_AGE  != ["all"] else "all_ages"
    cause_str  = "_".join(CAUSES).replace("!", "non_")
    if cause_str == "all":
        cause_str = "all_causes"
    return f"{region_str}_{dims_str}_{age_str}_{race_str}_{cause_str}"


def _build_group_by_params(group_dims: list) -> dict:
    """Build B_1 … B_5 params from REGION + GROUP_DIMS."""
    dims = [GROUP_BY[REGION]] + [GROUP_BY[d] for d in group_dims]
    # pad to 5
    dims += ["*None*"] * (5 - len(dims))
    return {f"B_{i+1}": v for i, v in enumerate(dims[:5])}


def _build_filter_params(sex_vals, hispanic_vals, race_vals, age_vals, cause_codes) -> str:
    """
    Build the variable-length filter portion of the POST body.
    Returns a list of (key, value) tuples for urllib encoding.
    """
    pairs = []

    # Location: all US
    pairs += [
        ("finder-stage-D77.V9",  "codeset"),
        ("O_V9_fmode",           "freg"),
        ("V_D77.V9",             ""),
        ("F_D77.V9",             "*All*"),
        ("I_D77.V9",             "*All* (The United States)\r\n"),
        ("finder-stage-D77.V10", "codeset"),
        ("O_V10_fmode",          "freg"),
        ("V_D77.V10",            ""),
        ("F_D77.V10",            "*All*"),
        ("I_D77.V10",            "*All* (The United States)\r\n"),
        ("finder-stage-D77.V27", "codeset"),
        ("O_V27_fmode",          "freg"),
        ("V_D77.V27",            ""),
        ("F_D77.V27",            "*All*"),
        ("I_D77.V27",            "*All* (The United States)\r\n"),
    ]

    # Urbanization: all
    pairs += [
        ("O_urban", "D77.V19"),
        ("V_D77.V19", "*All*"),
        ("V_D77.V11", "*All*"),
    ]

    # Age (Ten-Year groups)
    pairs += [("O_age", "D77.V51")]
    for v in age_vals:
        pairs.append(("V_D77.V51", v))
    pairs += [("V_D77.V5", "*All*"), ("V_D77.V52", "*All*"), ("V_D77.V6", "00")]

    # Sex
    for v in sex_vals:
        pairs.append(("V_D77.V7", v))

    # Hispanic Origin
    for v in hispanic_vals:
        pairs.append(("V_D77.V17", v))

    # Race
    for v in race_vals:
        pairs.append(("V_D77.V8", v))

    return pairs


def _encode_pairs(pairs: list[tuple]) -> str:
    """URL-encode a list of (key, value) tuples, repeating keys as needed.
    Preserves * literally (not %-encoded) to match WONDER's expected format."""
    parts = []
    for k, v in pairs:
        parts.append(f"{quote_plus(k, safe='*')}={quote_plus(str(v), safe='*')}")
    return "&".join(parts)


def _build_post_body(year: str, group_by_params: dict,
                     sex_vals, hispanic_vals, race_vals,
                     age_vals, cause_codes) -> str:
    # --- fixed prefix ---
    fixed = [
        ("saved_id",        ""),
        ("dataset_code",    "D77"),
        ("dataset_label",   "Multiple Cause of Death, 1999-2020"),
        ("dataset_vintage", "2020"),
        ("stage",           "request"),
        ("O_javascript",    "on"),
        ("M_1",             "D77.M1"),
        ("M_2",             "D77.M2"),
        ("M_3",             "D77.M3"),
        ("O_aar",           "aar_none"),
    ]

    # group-by (right after O_aar, per WORKING_CURL)
    for k in ("B_1","B_2","B_3","B_4","B_5"):
        fixed.append((k, group_by_params[k]))

    # output options (O_title, O_oc-sect1-request before O_rate_per, per WORKING_CURL)
    fixed += [
        ("O_title",                 ""),
        ("O_oc-sect1-request",      "close"),
        ("O_rate_per",              "100000"),
        ("O_aar_pop",               "0000"),
        ("VM_D77.M6_D77.V1_S",     "*All*"),
        ("VM_D77.M6_D77.V7",       "*All*"),
        ("VM_D77.M6_D77.V17",      "*All*"),
        ("VM_D77.M6_D77.V8",       "*All*"),
        ("VM_D77.M6_D77.V10",      ""),
        ("O_location",              LOCATION_RADIO.get("state", "D77.V9")),
    ]

    # variable-length filter params
    filter_pairs = _build_filter_params(
        sex_vals, hispanic_vals, race_vals, age_vals, cause_codes
    )

    # Year
    year_pairs = [
        ("finder-stage-D77.V1", "codeset"),
        ("O_V1_fmode",          "freg"),
        ("V_D77.V1",            ""),
        ("F_D77.V1",            year),
        ("I_D77.V1",            f"{year} ({year})\r\n"),
    ]

    # UCD: all causes
    ucd_pairs = [
        ("V_D77.V24",  "*All*"),
        ("V_D77.V20",  "*All*"),
        ("V_D77.V21",  "*All*"),
        ("O_ucd",      "D77.V2"),
        ("finder-stage-D77.V2", "codeset"),
        ("O_V2_fmode",          "freg"),
        ("V_D77.V2",            ""),
        ("F_D77.V2",            "*All*"),
        ("I_D77.V2",            "*All* (All Causes of Death)\r\n"),
        ("V_D77.V4",   "*All*"),
        ("V_D77.V12",  "*All*"),
        ("V_D77.V22",  "*All*"),
        ("V_D77.V23",  "*All*"),
        ("finder-stage-D77.V25", "codeset"),
        ("O_V25_fmode",          "freg"),
        ("V_D77.V25",            ""),
        ("F_D77.V25",            "*All*"),
        ("I_D77.V25",            "All Causes of Death\r\n"),
    ]

    # MCD cause filter
    mcd_pairs = [
        ("O_mcd", "D77.V13"),
        ("finder-stage-D77.V13", "codeset"),
        ("O_V13_fmode",          "fadv"),
        ("V_D77.V13",            ""),
        ("V_D77.V13_AND",        ""),
    ]
    for code in cause_codes:
        mcd_pairs.append(("F_D77.V13", code))

    mcd_pairs += [
        ("finder-stage-D77.V15", ""),
        ("O_V15_fmode",          "fadv"),
        ("V_D77.V15",            ""),
        ("V_D77.V15_AND",        ""),
        ("L_D77.V15",            "*All*"),
        ("finder-stage-D77.V16", ""),
        ("O_V16_fmode",          "fadv"),
        ("V_D77.V16",            ""),
        ("V_D77.V16_AND",        ""),
        ("L_D77.V16",            "*All*"),
        ("finder-stage-D77.V26", "codeset"),
        ("O_V26_fmode",          "fadv"),
        ("V_D77.V26",            ""),
        ("V_D77.V26_AND",        ""),
        ("F_D77.V26",            "*All*"),
    ]

    # export options (matches WORKING_CURL; add O_show_zeros / O_show_suppressed if needed)
    export_pairs = [
        ("O_change_action-Send-Export Results", "Export Results"),
        ("O_export-format",    "csv"),
        ("O_show_totals",      "true"),
        ("O_precision",        "1"),
        ("O_timeout",          "600"),
        ("action-Send",        "Send"),
    ]

    all_pairs = fixed + filter_pairs + year_pairs + ucd_pairs + mcd_pairs + export_pairs
    return _encode_pairs(all_pairs)


# ---------------------------------------------------------------------------
# CSV Parsing
# ---------------------------------------------------------------------------

def _parse_wonder_csv(raw_text: str) -> pd.DataFrame:
    """
    Parse the raw CSV returned by WONDER.
    - Strips header/footer rows (rows where 'Notes' column is non-empty, and
      the trailing metadata block that starts with a quoted long note).
    - Converts Deaths / Population to numeric (coerces Suppressed/Unreliable/Missing → NaN).
    - Returns a clean DataFrame.
    """
    # WONDER footer starts after a blank data row; it consists of lines where
    # the first column (Notes) is non-empty *and* subsequent columns are empty.
    lines = raw_text.splitlines()

    # Find the first line that looks like the metadata block:
    # it starts a quoted string that is NOT a data row.
    # Strategy: read everything into pandas, then drop rows where the
    # geography column (col index 1 or the column after Notes) is empty
    # but Notes is non-empty.
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

    # Drop footer rows: these are rows where 'Notes' is non-empty AND
    # the Deaths column (or any numeric column) is missing / empty.
    # The simplest heuristic: keep only rows where Notes is empty (data rows).
    df = df_raw[df_raw["Notes"].str.strip() == ""].copy()
    df = df.drop(columns=["Notes"])

    # Standardise column names
    df.columns = [c.strip() for c in df.columns]

    # Coerce numeric columns
    for col in ["Deaths", "Population", "Crude Rate"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    # Drop rows where all key fields are empty (blank separator lines)
    df = df.dropna(how="all")

    return df


# ---------------------------------------------------------------------------
# Session management via Playwright
# ---------------------------------------------------------------------------

def _open_wonder_session() -> tuple[str, str]:
    """
    Open a Playwright browser, navigate to the WONDER agreement page,
    click "I Agree", and capture the jsessionid + cookies from the
    resulting POST request.

    Returns (jsession_id, cookie_string).
    """
    from playwright.sync_api import sync_playwright

    captured = {}  # will hold {"jsession_id": ..., "cookies": ...}

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context()
        page = context.new_page()

        def handle_request(request):
            if "datarequest/D77" in request.url and request.method == "POST":
                # Extract jsessionid from URL
                m = re.search(r"jsessionid=([A-F0-9]+)", request.url)
                if m:
                    captured["jsession_id"] = m.group(1)

        page.on("request", handle_request)

        # Step 1: open agreement page
        print("  Opening WONDER agreement page ...", flush=True)
        page.goto("https://wonder.cdc.gov/mcd-icd10.html")

        # Step 2: click "I Agree"
        page.click('input[value="I Agree"]')

        # Wait for the request form to load
        page.wait_for_selector("#submit-button1", state="visible")
        page.wait_for_timeout(2000)

        # Step 3: click submit-button1 to trigger the POST request that captures jsessionid
        page.click("#submit-button1")
        page.wait_for_timeout(2000)

        # Grab all cookies from the browser context
        browser_cookies = context.cookies("https://wonder.cdc.gov")
        cookie_str = "; ".join(
            f"{c['name']}={c['value']}" for c in browser_cookies
        )
        captured["cookies"] = cookie_str

        browser.close()

    print(captured)

    if "jsession_id" not in captured:
        raise RuntimeError("Failed to capture jsessionid from WONDER")

    print(f"  Session ID : {captured['jsession_id']}")
    print(f"  Cookies    : {captured['cookies'][:80]}...")
    return captured["jsession_id"], captured["cookies"]


# ---------------------------------------------------------------------------
# HTTP call via curl
# ---------------------------------------------------------------------------

def _build_curl_headers(jsession_id: str) -> list[str]:
    """Build the header list for curl, matching WORKING_CURL exactly."""
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


def _fetch_year(year: str, post_body: str,
                jsession_id: str, cookies: str) -> str:
    """Execute curl and return the raw response text."""
    wonder_url = f"https://wonder.cdc.gov/controller/datarequest/D77;jsessionid={jsession_id}"
    headers = _build_curl_headers(jsession_id)

    cmd = ["curl", "-s", "-X", "POST", wonder_url]
    for h in headers:
        cmd += ["-H", h]
    cmd += ["-b", cookies]
    cmd += ["--data-raw", post_body]

    result = subprocess.run(cmd, capture_output=True, timeout=300)
    if result.returncode != 0:
        with open(OUTPUT_DIR / "curl_output.html", "w") as f:
            f.write(result.stderr.decode('latin-1', errors='replace'))
        raise RuntimeError(
            f"curl failed (code {result.returncode}): {result.stderr[:400]}"
        )
    return result.stdout.decode("latin-1", errors="replace")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    label = LABEL or _auto_label()
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    out_path = OUTPUT_DIR / f"{label}.parquet"

    # --- Open a WONDER session (Playwright) ---
    print("Opening WONDER session ...")
    jsession_id, cookies = _open_wonder_session()
    print()

    # Resolve filter values
    sex_vals      = [SEX[k] for k in FILTER_SEX]
    hispanic_vals = [HISPANIC_ORIGIN[k] for k in FILTER_HISPANIC]
    race_vals     = [RACE[k] for k in FILTER_RACE]
    age_vals      = [AGE_5YR.get(k, k) for k in FILTER_AGE]
    cause_codes   = _resolve_causes(CAUSES)

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

        post_body = _build_post_body(
            year_str, group_by_params,
            sex_vals, hispanic_vals, race_vals, age_vals, cause_codes,
        )

        raw = _fetch_year(year_str, post_body, jsession_id, cookies)

        # Check for WONDER error page (save HTML to curl_output.html for debugging)
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
        
        frames.append(df)
        print(f"{len(df):,} rows")

    if not frames:
        print("No data retrieved.")
        return

    combined = pd.concat(frames, ignore_index=True)
    print(f"\nTotal rows: {combined.shape[0]:,}")

    # ---------------------------------------------------------------------------
    # Write / append to Parquet
    # ---------------------------------------------------------------------------
    table = pa.Table.from_pandas(combined, preserve_index=False)

    if out_path.exists():
        existing = pq.read_table(str(out_path))
        # Deduplicate on all non-metadata columns if re-pulling same years
        combined_all = pd.concat(
            [existing.to_pandas(), combined], ignore_index=True
        )
        # Identify key columns (all except query metadata)
        key_cols = [c for c in combined_all.columns
                    if c not in ("query_label", "cause_filter")]
        combined_all = combined_all.drop_duplicates(subset=key_cols, keep="last")
        table = pa.Table.from_pandas(combined_all, preserve_index=False)
        print(f"Merged with existing file → {len(combined_all):,} total rows")

    pq.write_table(table, str(out_path), compression="snappy")
    print(f"Saved → {out_path}")


if __name__ == "__main__":
    main()
