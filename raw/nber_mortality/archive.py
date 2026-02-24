

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
YEAR_TO_DIR.update({yr: yr for yr in range(1968, 2006)})

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
