
import io
import zipfile
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
from pathlib import Path

## Pull mortality data

# ── Variable name map ─────────────────────────────────────────────────────────
# Derived from .dct parse above.
# The year field switched from "datayear" to "year" in 1996.
# All other target variables ("race", "age", "ucod") are stable across all years.
# hispanic/hspanicr is available 1989+.

def get_varnames(year: int) -> dict:
    """Return the column names to read for a given year."""
    return {
        "year":  "datayear" if year <= 1995 else "year",
        "race":  "race",
        "age":   "age",
        "ucod":  "ucod",
        # optional – include only if year >= 1989
        **( {"hispanic": "hispanic"} if year >= 1989 else {} ),
    }

# ── Age bins ──────────────────────────────────────────────────────────────────
AGE_BINS   = [0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 9999]
AGE_LABELS = ["0","1-4","5-14","15-24","25-34","35-44",
              "45-54","55-64","65-74","75-84","85+"]

# ── Main download-and-aggregate function ──────────────────────────────────────

def aggregate_year(year: int, csv_url: str) -> pd.DataFrame | None:
    """
    Download mort{year}.csv.zip (or .csv), read only the needed columns,
    bin age, group by (year, race, age_bin, ucod[+hispanic]), return counts.
    Deletes the downloaded file when done.
    """
    varnames = get_varnames(year)
    cols_needed = list(varnames.values())   # actual column names in the file

    tmp = Path(f"_mort_{year}_tmp")

    # ── download ──
    print(f"  downloading {csv_url} …", end=" ", flush=True)
    r = requests.get(csv_url, stream=True)
    r.raise_for_status()

    if csv_url.endswith(".zip"):
        tmp.write_bytes(r.content)
        zf = zipfile.ZipFile(tmp)
        csv_name = next(n for n in zf.namelist() if n.lower().endswith(".csv"))
        raw = zf.read(csv_name)
        zf.close()
        tmp.unlink(missing_ok=True)
        buf = io.BytesIO(raw)
    else:
        tmp.write_bytes(r.content)
        buf = tmp.open("rb")

    # ── read only needed columns ──
    df = pd.read_csv(buf, usecols=cols_needed, dtype=str, low_memory=False)
    if hasattr(buf, "close"):
        buf.close()
    tmp.unlink(missing_ok=True)   # clean up raw file
    print(f"rows={len(df):,}")

    # ── coerce types ──
    df["age_num"] = pd.to_numeric(df[varnames["age"]], errors="coerce")
    df["age_bin"] = pd.cut(df["age_num"], bins=AGE_BINS, labels=AGE_LABELS,
                           right=False).astype(str)

    # standardise column names to canonical names
    df = df.rename(columns={v: k for k, v in varnames.items()})

    # ── aggregate ──
    group_cols = ["year", "race", "age_bin", "ucod"]
    if "hispanic" in df.columns:
        group_cols.append("hispanic")

    agg = (df.groupby(group_cols, dropna=False)
             .size()
             .reset_index(name="deaths"))
    return agg


# ── Run for all years, append to a single Parquet file ───────────────────────

OUT_PARQUET = Path("nber_mortality_counts.parquet")
writer = None
schema = None

for yr, csv_url in sorted(results.items()):
    if csv_url is None:
        print(f"{yr}: skipped (no URL)")
        continue
    print(f"{yr}:", end=" ")
    try:
        agg = aggregate_year(yr, csv_url)
        table = pa.Table.from_pandas(agg, preserve_index=False)
        if writer is None:
            schema = table.schema
            writer = pq.ParquetWriter(OUT_PARQUET, schema)
        writer.write_table(table.cast(schema))
    except Exception as e:
        print(f"  ERROR: {e}")

if writer:
    writer.close()
    print(f"\nDone → {OUT_PARQUET.resolve()}")
else:
    print("No data written.")


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
YEAR_TO_DIR.update({yr: yr for yr in range(1968, 2022)})

# 2019+ use a different naming convention and subdirectory
SPECIAL_YEARS = {
    2019: f"{NBER_URL}2019/nber_output/mortality_2019_nber_us.csv",
    2020: f"{NBER_URL}2020/nber_output/mortality_2020_nber_us.csv",
    2021: f"{NBER_URL}2021/nber_output/mortality_2021_nber_us.csv",
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
for year in range(1959, 2022):
    url = find_mort_url(year)
    results[year] = url
    status = url if url else "NOT FOUND"
    print(f"{year}: {status}")
