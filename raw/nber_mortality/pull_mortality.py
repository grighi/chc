#!/usr/bin/env python3
"""
pull_mortality.py
=================
Download NBER Vital Statistics Multiple Cause of Death micro-data (1959–2021),
aggregate to (year, staters, countyrs, race, age_bin, ucod [, hispanic]) death
counts, and write to a single Parquet file.

Usage:
    python3 pull_mortality.py

Output:
    nber_mortality_counts.parquet

Variable notes
--------------
  year      : calendar year of death
  staters   : state of residence FIPS (2-digit string)
  countyrs  : county of residence code (3-digit string within state)
              → combine with staters for 5-digit FIPS: staters.zfill(2) + countyrs.zfill(3)
  race      : NCHS race code (1=White, 2=Black, 3=Other, …)
  age_bin   : 11-level age group (0, 1-4, 5-14, 15-24, 25-34, 35-44,
              45-54, 55-64, 65-74, 75-84, 85+)
  ucod      : underlying cause of death (ICD-9 1979-1998; ICD-10 1999+)
  hispanic  : Hispanic origin code (present 1989+; missing for earlier years)
  deaths    : count of micro-data records matching the cell

ICD reference (for downstream use)
-----------------------------------
  Suicide       ICD-9  E950-E959       ICD-10  X60-X84, Y87.0
  Homicide      ICD-9  E960-E969,E979  ICD-10  X85-Y09, Y87.1
  Unint. poison ICD-9  E850-E869       ICD-10  X40-X49
  Self poison   ICD-9  E950-E952       ICD-10  X60-X69
  All external  ICD-9  E800-E999       ICD-10  V01-Y98
"""


import warnings
warnings.warn(
    "Year 1969 appears to be incorrect. Check raw mortality file coding (datayear vs calendar year).",
    UserWarning
)
    
import argparse
import concurrent.futures
import io
import sys
import threading
import zipfile
from pathlib import Path

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import requests

# ── Configuration ──────────────────────────────────────────────────────────────

OUT_PARQUET  = Path("nber_mortality_counts_2.parquet")

YEARS        = range(1959, 2022)   # inclusive on both ends
TIMEOUT_HEAD = 15                  # seconds for HEAD requests
TIMEOUT_GET  = 600                 # seconds for full file downloads
MAX_WORKERS  = 8                   # parallel download threads

# ── Age bins ───────────────────────────────────────────────────────────────────

AGE_BINS   = [0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 9999]
AGE_LABELS = ["0", "1-4", "5-14", "15-24", "25-34", "35-44",
              "45-54", "55-64", "65-74", "75-84", "85+"]

# ── NBER URL scheme ────────────────────────────────────────────────────────────
# 1959-1967: all files live under the 1959/ directory.
# 2019-2021: different subdirectory and filename convention.

NBER_URL = "https://data.nber.org/mortality/"

YEAR_TO_DIR = {yr: 1959 for yr in range(1959, 1968)}
YEAR_TO_DIR.update({yr: yr for yr in range(1968, 2005)})

SPECIAL_URLS = {
    # 2019: f"{NBER_URL}2019/nber_output/mortality_2019_nber_us.csv",
    # 2020: f"{NBER_URL}2020/nber_output/mortality_2020_nber_us.csv",
    # 2021: f"{NBER_URL}2021/nber_output/mortality_2021_nber_us.csv",
}

# ── Fixed output schema ────────────────────────────────────────────────────────
# Defined upfront so every year — including pre-1989 years that lack hispanic —
# is written with a consistent set of columns.  Missing columns are filled with
# null before writing.

OUTPUT_SCHEMA = pa.schema([
    pa.field("year",     pa.string()),
    pa.field("staters",  pa.string()),
    pa.field("countyrs", pa.string()),
    pa.field("race",     pa.string()),
    pa.field("age_bin",  pa.string()),
    pa.field("ucod",     pa.string()),
    pa.field("hispanic", pa.string()),   # null for years < 1989
    pa.field("deaths",   pa.int64()),
])

# ── Thread-safe logging ────────────────────────────────────────────────────────

_print_lock = threading.Lock()

def _log(msg: str) -> None:
    with _print_lock:
        print(msg, flush=True)


# ── URL discovery ──────────────────────────────────────────────────────────────

def find_mort_url(year: int) -> str | None:
    """Return the first reachable URL for a given year, or None."""
    if year in SPECIAL_URLS:
        url = SPECIAL_URLS[year]
        try:
            r = requests.head(url, allow_redirects=True, timeout=TIMEOUT_HEAD)
            return url if r.status_code == 200 else None
        except requests.RequestException:
            return None

    dir_year = YEAR_TO_DIR.get(year)
    if dir_year is None:
        return None

    base = f"{NBER_URL}{dir_year}/"
    for filename in [f"mort{year}.csv.zip", f"mort{year}.csv"]:
        url = base + filename
        try:
            r = requests.head(url, allow_redirects=True, timeout=TIMEOUT_HEAD)
            if r.status_code == 200:
                return url
        except requests.RequestException:
            continue
    return None


# ── Variable name map ─────────────────────────────────────────────────────────
# Maps canonical name → actual CSV column name for a given year.
#
# "year"     : switched from "datayear" to "year" in 1996
# "staters"  : 2-digit state of residence FIPS
# "countyrs" : 3-digit county of residence code (within state)
# "hispanic" : available 1989+

def get_varnames(year: int) -> dict[str, str]:
    """Return {canonical_name: actual_csv_column} for the given year."""
    d = {
        "year":     "datayear" if year <= 1995 else "year",
        "race":     "race",
        "age":      "age",
        "ucod":     "ucod",
        "staters":  "staters",
        "countyrs": "countyrs",
    }
    if year >= 1989:
        d["hispanic"] = "hispanic"
    return d


# ── Download and aggregate one year ───────────────────────────────────────────

def _download_to_buffer(url: str) -> io.BytesIO:
    """Download url; extract first .csv if zip. Return BytesIO."""
    r = requests.get(url, stream=True, timeout=TIMEOUT_GET)
    r.raise_for_status()
    raw = r.content

    if url.lower().endswith(".zip"):
        with zipfile.ZipFile(io.BytesIO(raw)) as zf:
            csv_name = next(n for n in zf.namelist() if n.lower().endswith(".csv"))
            raw = zf.read(csv_name)

    return io.BytesIO(raw)


def _read_csv_robust(buf: io.BytesIO, **kwargs) -> pd.DataFrame:
    """Read CSV from a BytesIO buffer, falling back to latin-1 on UnicodeDecodeError."""
    pos = buf.tell()
    try:
        return pd.read_csv(buf, encoding="utf-8", **kwargs)
    except UnicodeDecodeError:
        _log("  NOTE: utf-8 decode failed — retrying with latin-1")
        buf.seek(pos)
        return pd.read_csv(buf, encoding="latin-1", **kwargs)


def aggregate_year(year: int, url: str) -> pd.DataFrame | None:
    """
    Download one year of NBER mortality data, keep needed columns, bin age,
    and aggregate to death counts.

    Returns a tidy DataFrame:
      [year, staters, countyrs, race, age_bin, ucod, (hispanic,) deaths]

    staters / countyrs are strings. If absent from the source file (typically
    some pre-1972 years), those columns will be null.
    """
    varnames = get_varnames(year)

    # Download
    _log(f"  {year}: downloading …")
    buf = _download_to_buffer(url)

    # Peek at available columns (case-insensitive match)
    header = _read_csv_robust(buf, nrows=0, dtype=str)
    available_lower = {c.lower(): c for c in header.columns}
    buf.seek(0)

    # Map canonical → actual column name, skipping absent optional columns
    present: dict[str, str] = {}
    for canonical, actual in varnames.items():
        if actual.lower() in available_lower:
            present[canonical] = available_lower[actual.lower()]
        elif canonical in ("staters", "countyrs", "hispanic"):
            _log(f"  {year}: NOTE '{actual}' absent — column will be null")
        else:
            # Required column missing — skip year
            _log(f"  {year}: SKIP — required column '{actual}' not found")
            return None

    # Read only the columns we need
    df = _read_csv_robust(buf, usecols=list(present.values()), dtype=str, low_memory=False)
    _log(f"  {year}: done  rows={len(df):,}")

    # Rename to canonical names
    df = df.rename(columns={v: k for k, v in present.items()})

    # Ensure staters / countyrs columns exist (null if absent in source)
    for col in ("staters", "countyrs"):
        if col not in df.columns:
            df[col] = pd.NA

    # Bin age
    df["age_num"] = pd.to_numeric(df["age"], errors="coerce")
    df["age_bin"] = pd.cut(
        df["age_num"], bins=AGE_BINS, labels=AGE_LABELS, right=False
    ).astype(str)
    df = df.drop(columns=["age", "age_num"])

    # Aggregate
    group_cols = ["year", "staters", "countyrs", "race", "age_bin", "ucod"]
    if "hispanic" in df.columns:
        group_cols.append("hispanic")

    agg = (
        df.groupby(group_cols, dropna=False)
        .size()
        .reset_index(name="deaths")
    )
    return agg


# ── Main ───────────────────────────────────────────────────────────────────────

def main() -> None:
    parser = argparse.ArgumentParser(description="Download and aggregate NBER mortality data.")
    parser.add_argument("start_year", nargs="?", type=int, default=1959,
                        help="First year to download (default: 1959)")
    parser.add_argument("end_year",   nargs="?", type=int, default=2021,
                        help="Last year to download, inclusive (default: 2021)")
    args = parser.parse_args()
    years = range(args.start_year, args.end_year + 1)

    # ── Phase 1: parallel HEAD scan ───────────────────────────────────────────
    print(f"Scanning NBER for years {args.start_year}–{args.end_year} ({MAX_WORKERS} parallel HEAD requests) …")
    urls: dict[int, str] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        future_to_year = {pool.submit(find_mort_url, yr): yr for yr in years}
        for fut in concurrent.futures.as_completed(future_to_year):
            yr = future_to_year[fut]
            try:
                url = fut.result()
            except Exception as exc:
                _log(f"  {yr}: HEAD error — {exc}")
                continue
            if url:
                urls[yr] = url
                _log(f"  {yr}: {url}")
            else:
                _log(f"  {yr}: not found — skipping")

    print(f"\nFound {len(urls)} years. Downloading with {MAX_WORKERS} workers …\n")

    # ── Phase 2: parallel downloads + aggregation ─────────────────────────────
    # Results are collected into a dict so we can write in year order after all
    # threads finish.  ParquetWriter is not thread-safe so writes stay serial.
    results: dict[int, pd.DataFrame | None] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        future_to_year = {
            pool.submit(aggregate_year, yr, url): yr
            for yr, url in urls.items()
        }
        for fut in concurrent.futures.as_completed(future_to_year):
            yr = future_to_year[fut]
            try:
                agg = fut.result()
            except Exception as exc:
                _log(f"  {yr}: ERROR — {exc}")
                agg = None
            results[yr] = agg

    # ── Phase 3: write in year order ──────────────────────────────────────────
    print(f"\nWriting {OUT_PARQUET} …")
    writer = pq.ParquetWriter(OUT_PARQUET, OUTPUT_SCHEMA, compression="snappy")
    n_written = 0
    for yr in sorted(results):
        agg = results[yr]
        if agg is None or agg.empty:
            continue
        # Pad any columns absent in this year's data (e.g. hispanic pre-1989)
        for field in OUTPUT_SCHEMA:
            if field.name not in agg.columns:
                agg[field.name] = pd.NA
        table = pa.Table.from_pandas(
            agg[[f.name for f in OUTPUT_SCHEMA]], preserve_index=False
        ).cast(OUTPUT_SCHEMA)
        writer.write_table(table)
        n_written += 1
    writer.close()
    print(f"\n✓  Done → {OUT_PARQUET.resolve()}  ({n_written} years written)")


if __name__ == "__main__":
    main()


## Mortality Codes
## 
# {
#   "note": "You’re thinking of ICD-9 (and ICD-9-CM) E-codes, not ICD-1. The E800–E999 block is the classic ICD-9 external-cause range for injuries/poisonings. In ICD-10, external causes move to V01–Y98.",
#   "icd_sets": [
#     {
#       "icd_id": "ICD-9 (mortality, NCHS/US)",
#       "years_relevant": {
#         "united_states_cause_of_death": "1979-1998"
#       },
#       "codes_that_match": {
#         "external_causes_overall": ["E800-E999"],
#         "accidental_death_unintentional_injury_all": ["E800-E848", "E850-E869", "E880-E929"],
#         "suicide_self_inflicted": ["E950-E959"],
#         "homicide_assault": ["E960-E969", "E979"],
#         "poisoning_unintentional": ["E850-E869"],
#         "poisoning_suicide_self_poisoning": ["E950-E952"],
#         "poisoning_homicide_assault_by_poisoning": ["E962"]
#       }
#     },
#     {
#       "icd_id": "ICD-10 (mortality, NCHS/US)",
#       "years_relevant": {
#         "united_states_cause_of_death": "1999-present"
#       },
#       "codes_that_match": {
#         "external_causes_overall": ["V01-Y98"],
#         "accidental_death_unintentional_injury_all": ["V01-X59", "Y85-Y86"],
#         "suicide_intentional_self_harm": ["X60-X84", "Y87.0"],
#         "homicide_assault": ["X85-Y09", "Y87.1"],
#         "poisoning_unintentional": ["X40-X49"],
#         "poisoning_suicide_intentional_self_poisoning": ["X60-X69"],
#         "poisoning_homicide_assault_by_poisoning": ["X85-X90"]
#       }
#     },
#     {
#       "icd_id": "ICD-9-CM (US clinical modification; morbidity/claims)",
#       "years_relevant": {
#         "united_states_healthcare_claims": "1979-2015-09-30"
#       },
#       "codes_that_match": {
#         "external_causes_overall": ["E800-E999"],
#         "accidental_death_unintentional_injury_all": ["E800-E848", "E850-E869", "E880-E929"],
#         "suicide_self_inflicted": ["E950-E959"],
#         "homicide_assault": ["E960-E969", "E979"],
#         "poisoning_unintentional": ["E850-E869"],
#         "poisoning_suicide_self_poisoning": ["E950-E952"],
#         "poisoning_homicide_assault_by_poisoning": ["E962"]
#       }
#     },
#     {
#       "icd_id": "ICD-10-CM (US clinical modification; morbidity/claims)",
#       "years_relevant": {
#         "united_states_healthcare_claims": "2015-10-01-present"
#       },
#       "codes_that_match": {
#         "external_causes_overall": ["V00-Y99"],
#         "accidental_death_unintentional_injury_all": ["V01-X59", "Y85-Y86"],
#         "suicide_intentional_self_harm": ["X60-X84", "Y87.0"],
#         "homicide_assault": ["X85-Y09", "Y87.1"],
#         "poisoning_unintentional": ["X40-X49"],
#         "poisoning_suicide_intentional_self_poisoning": ["X60-X69"],
#         "poisoning_homicide_assault_by_poisoning": ["X85-X90"]
#       }
#     }
#   ],
#   "source_notes": {
#     "icd9_us_mortality_years_by_revision": "NCHS lists ICD-9 for US cause-of-death 1979–1998 and ICD-10 from 1999 onward.",
#     "icd10_global_context": "WHO describes ICD-10 Chapter XX external causes as V01–Y98; CDC notes ICD-10 came into use in WHO member states in 1994.",
#     "icd9_icd10_external_cause_groupings": "State public-health injury matrices commonly use: ICD-9 unintentional E800–E848,E850–E869,E880–E929; suicide E950–E959; homicide E960–E969,E979 and ICD-10 unintentional V01–X59,Y85–Y86; suicide X60–X84,Y87.0; homicide X85–Y09,Y87.1; unintentional poisoning X40–X49."
#   }
# }






## ====================== OTHER STUFF ======================
##
## Find dcts of variable names
## =========================================================

import re
from collections import defaultdict

# ── 2. Parse the combined .dct and build a variable-name mapping ─────────────
#
# Key concepts across all years:
#   year      : "year" (ICD-10 era, ~2003+) or "datayear" (older files)
#   race      : "race" (all years; detail race code)
#   age       : "age"  (all years; detail age)
#   cause     : "ucod" (all years; underlying cause of death, ICD code string)
#
# Additional race/age recodes present in most years:
#   racer3, racer5  – race recodes
#   ager52, ager27, ager12, ager22  – age recodes
#   hispanic / hspanicr  – hispanic origin (ICD-10 era)

# Patterns we care about (regex on variable name part of each _column line)
TARGET_PATTERNS = {
    "year":    re.compile(r'\b(year|datayear)\b'),
    "race":    re.compile(r'\brace\b'),
    "race_recode": re.compile(r'\bracer\d+\b'),
    "age":     re.compile(r'\bage\b'),
    "age_recode": re.compile(r'\bager\d+\b'),
    "cause_ucod": re.compile(r'\bucod\b'),
    "hispanic": re.compile(r'\bhispanic\b|\bhspanicr\b'),
}

# Regex to extract a _column line: _column(N) <type> <varname> <format> ["label"]
COL_RE = re.compile(
    r'_column\(\s*(\d+)\s*\)\s+\S+\s+(\w+)\s+(%\S+)(?:\s+"([^"]*)")?'
)

# Parse by year block
year_var_map = {}   # year -> {concept -> [(col, varname, label), ...]}

current_year = None
current_vars = defaultdict(list)

dct_text = dct_path.read_text(errors="replace")

for line in dct_text.splitlines():
    # Detect year header lines we wrote
    m_hdr = re.match(r'^=== YEAR (\d{4})', line)
    if m_hdr:
        if current_year is not None:
            year_var_map[current_year] = dict(current_vars)
        current_year = int(m_hdr.group(1))
        current_vars = defaultdict(list)
        continue

    if current_year is None:
        continue

    m_col = COL_RE.search(line)
    if not m_col:
        continue

    col_num, varname, fmt, label = m_col.groups()
    label = label or ""

    for concept, pat in TARGET_PATTERNS.items():
        if pat.search(varname):
            current_vars[concept].append((int(col_num), varname, label))

# Save last block
if current_year is not None:
    year_var_map[current_year] = dict(current_vars)

# ── 3. Print a summary table ──────────────────────────────────────────────────
print(f"{'Year':>6}  {'year_var':12}  {'race_var':10}  {'age_var':8}  {'ucod_var':10}  {'hispanic':10}")
print("-" * 70)

for yr in sorted(year_var_map):
    v = year_var_map[yr]
    yr_var   = v.get("year",     [("","","")])[0][1] if v.get("year")     else "—"
    race_var = v.get("race",     [("","","")])[0][1] if v.get("race")     else "—"
    age_var  = v.get("age",      [("","","")])[0][1] if v.get("age")      else "—"
    ucod_var = v.get("cause_ucod",[("","","")])[0][1] if v.get("cause_ucod") else "—"
    hisp_var = v.get("hispanic", [("","","")])[0][1] if v.get("hispanic") else "—"
    print(f"{yr:>6}  {yr_var:12}  {race_var:10}  {age_var:8}  {ucod_var:10}  {hisp_var:10}")


## ====================== OTHER STUFF ======================
##
## Find datasets
## =========================================================

import requests

NBER_URL = "https://data.nber.org/mortality/"

# Years 1959-1967 live under the 1959/ directory
YEAR_TO_DIR = {yr: 1959 for yr in range(1959, 1968)}
YEAR_TO_DIR.update({yr: yr for yr in range(1968, 2005)})

# 2019+ use a different naming convention and subdirectory
SPECIAL_YEARS = {
    # 2019: f"{NBER_URL}2019/nber_output/mortality_2019_nber_us.csv",
    # 2020: f"{NBER_URL}2020/nber_output/mortality_2020_nber_us.csv",
    # 2021: f"{NBER_URL}2021/nber_output/mortality_2021_nber_us.csv",
}

def find_mort_url(year):
    """
    For a given year, check for:
      1. mort{year}.csv.zip  (preferred)
      2. mort{year}.csv
    in the appropriate directory. Returns the first URL that exists, or None.
    Special-cases 2019–2021 which use a different path/naming scheme.
    """
    if year in SPECIAL_YEARS:
        url = SPECIAL_YEARS[year]
        r = requests.head(url, allow_redirects=True)
        return url if r.status_code == 200 else None

    dir_year = YEAR_TO_DIR[year]
    base = f"{NBER_URL}{dir_year}/"

    for filename in [f"mort{year}.csv.zip", f"mort{year}.csv"]:
        url = base + filename
        r = requests.head(url, allow_redirects=True)
        if r.status_code == 200:
            return url
    return None

results = {}
for year in range(1959, 2005):
    url = find_mort_url(year)
    results[year] = url
    status = url if url else "NOT FOUND"
    print(f"{year}: {status}")
