#!/usr/bin/env python3
"""
combine_raw.py
==============
Combine SEER (1969-2009) and Census (2010-2023) county-level population data
into a single Parquet file.

Output schema: year, fips, race, hispanic, sex, age_bin, pop

Sources
-------
  SEER   1969–2009  population_seer/uswbo19agesadj.csv
  Census 2010–2019  population_cen/CC-EST2020-ALLDATA.csv  (YEAR 2-11)
  Census 2020–2023  population_cen/cc-est2023-alldata.csv  (YEAR 2-5)

Race codes (harmonised)
-----------------------
  1 = White
  2 = Black
  3 = Other  (SEER race 3 & 4 collapsed here; Census residual NH/H)

Hispanic codes
--------------
  0  = Non-Hispanic
  1  = Hispanic
  pd.NA = unknown (SEER pre-2005, where hispanic=9 in source)

Sex codes
---------
  1 = Male
  2 = Female

Age bins
--------
  <20    SEER age codes  0-4   / Census AGEGRP  1-4
  20-59  SEER age codes  5-12  / Census AGEGRP  5-12
  60+    SEER age codes 13-18  / Census AGEGRP 13-18

Census YEAR mapping
-------------------
  CC-EST2020: YEAR=1 → 4/1/2010 census (skip)
              YEAR=2-11 → 7/1/2010 – 7/1/2019  (calendar_year = YEAR + 2008)
              YEAR=12 → 4/1/2020 census (skip)
              YEAR=13 → 7/1/2020  (skipped; use cc-est2023 for 2020 instead)
  cc-est2023: YEAR=1 → 4/1/2020 census (skip)
              YEAR=2-5 → 7/1/2020 – 7/1/2023  (calendar_year = YEAR + 2018)
"""

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
from pathlib import Path

ROOT               = Path(__file__).parent
OUT_PARQUET        = ROOT / "county_pop_demo.parquet"
OUT_PARQUET_TOTALS = ROOT / "county_pop.parquet"

WONDER_DIR = ROOT / "wonder_mortality/data"
WONDER_FINAL       = WONDER_DIR / "county_sex_race_hispanic_origin_adults_all_races_all_causes.parquet"
WONDER_PROVISIONAL = WONDER_DIR / "county_sex_race_hispanic_origin_adults_all_races_all_causes_provisional.parquet"
WONDER_COD_EXTERNAL = WONDER_DIR / "cod_external_injury_alladults.parquet"

NBER_PARQUET = ROOT / "nber_mortality/nber_mortality_counts.parquet"


# ── Age bin helpers ────────────────────────────────────────────────────────────

def _age_bin_seer(code: int) -> str:
    """SEER age code 0-18 → '<20' / '20-59' / '60+'."""
    if code <= 4:
        return "<20"
    if code <= 12:
        return "20-59"
    return "60+"


def _age_bin_census(agegrp: int) -> str:
    """Census AGEGRP 1-18 → '<20' / '20-59' / '60+'."""
    if agegrp <= 4:
        return "<20"
    if agegrp <= 12:
        return "20-59"
    return "60+"


# ── SEER 1969–2009 ─────────────────────────────────────────────────────────────

def load_seer() -> pd.DataFrame:
    df = pd.read_csv(
        ROOT / "population_seer/uswbo19agesadj.csv",
        dtype={"county": str},
        low_memory=False,
    )

    # Keep 1969-2009 only (Census takes over from 2010)
    df = df[df["year"] <= 2009].copy()

    # Race: collapse Asian/PI (4) into Other (3)
    df["race"] = df["race"].replace(4, 3)

    # Hispanic: 9 = "not applicable" → NA; keep 0/1 where present
    df["hispanic"] = df["hispanic"].replace(9, pd.NA).astype("Int8")

    # Age bin
    df["age_bin"] = df["age"].map(_age_bin_seer)

    result = (
        df.rename(columns={"county": "fips"})
        .groupby(["year", "fips", "race", "hispanic", "sex", "age_bin"], dropna=False)["pop"]
        .sum()
        .reset_index()
    )
    return result


# ── Census 2010–2023 ───────────────────────────────────────────────────────────
#
# The Census files are wide: one row = county × year × AGEGRP, with separate
# columns for each race × hispanic × sex combination.
# We construct:
#   (race=1, hisp=0): NHWA_MALE / NHWA_FEMALE   (non-Hispanic White alone)
#   (race=1, hisp=1): HWA_MALE  / HWA_FEMALE    (Hispanic White alone)
#   (race=2, hisp=0): NHBA_MALE / NHBA_FEMALE   (non-Hispanic Black alone)
#   (race=2, hisp=1): HBA_MALE  / HBA_FEMALE    (Hispanic Black alone)
#   (race=3, hisp=0): NH_MALE - NHWA_MALE - NHBA_MALE  (other non-Hispanic)
#   (race=3, hisp=1): H_MALE  - HWA_MALE  - HBA_MALE   (other Hispanic)

_RACE_HISP_COLS = {
    (1, 0): ("NHWA_MALE", "NHWA_FEMALE"),
    (1, 1): ("HWA_MALE",  "HWA_FEMALE"),
    (2, 0): ("NHBA_MALE", "NHBA_FEMALE"),
    (2, 1): ("HBA_MALE",  "HBA_FEMALE"),
}

_CENSUS_COLS = [
    "SUMLEV", "STATE", "COUNTY", "YEAR", "AGEGRP",
    "NHWA_MALE", "NHWA_FEMALE", "HWA_MALE",  "HWA_FEMALE",
    "NHBA_MALE", "NHBA_FEMALE", "HBA_MALE",  "HBA_FEMALE",
    "NH_MALE",   "NH_FEMALE",   "H_MALE",    "H_FEMALE",
]


def _load_census_file(filepath: Path, year_map: dict) -> pd.DataFrame:
    df = pd.read_csv(filepath, usecols=_CENSUS_COLS, encoding="latin1", low_memory=False)

    # Population columns may read as mixed types due to header/note rows — coerce
    pop_cols = [c for c in _CENSUS_COLS if c not in ("SUMLEV","STATE","COUNTY","YEAR","AGEGRP")]
    for col in pop_cols:
        df[col] = pd.to_numeric(df[col], errors="coerce")

    # County-level rows only; drop AGEGRP=0 (all-age total)
    df = df[(df["SUMLEV"] == 50) & (df["AGEGRP"] > 0)].copy()

    # Keep only YEAR values that map to a calendar year
    df = df[df["YEAR"].isin(year_map)].copy()
    df["year"] = df["YEAR"].map(year_map)

    # 5-digit FIPS
    df["fips"] = df["STATE"].astype(str).str.zfill(2) + df["COUNTY"].astype(str).str.zfill(3)

    # Age bin
    df["age_bin"] = df["AGEGRP"].map(_age_bin_census)

    records = []
    base_cols = ["year", "fips", "age_bin"]

    # Named race × hispanic pairs
    for (race, hisp), (m_col, f_col) in _RACE_HISP_COLS.items():
        for sex_code, pop_col in ((1, m_col), (2, f_col)):
            tmp = df[base_cols + [pop_col]].copy()
            tmp["race"]     = race
            tmp["hispanic"] = hisp
            tmp["sex"]      = sex_code
            tmp = tmp.rename(columns={pop_col: "pop"})
            records.append(tmp)

    # Residual "Other" = NH or H total minus White-alone minus Black-alone
    for hisp, (tot_m, tot_f, wa_m, wa_f, ba_m, ba_f) in [
        (0, ("NH_MALE", "NH_FEMALE", "NHWA_MALE", "NHWA_FEMALE", "NHBA_MALE", "NHBA_FEMALE")),
        (1, ("H_MALE",  "H_FEMALE",  "HWA_MALE",  "HWA_FEMALE",  "HBA_MALE",  "HBA_FEMALE")),
    ]:
        for sex_code, t, wa, ba in ((1, tot_m, wa_m, ba_m), (2, tot_f, wa_f, ba_f)):
            tmp = df[base_cols].copy()
            tmp["race"]     = 3
            tmp["hispanic"] = hisp
            tmp["sex"]      = sex_code
            tmp["pop"]      = (df[t] - df[wa] - df[ba]).values
            records.append(tmp)

    long = pd.concat(records, ignore_index=True)

    return (
        long.groupby(["year", "fips", "race", "hispanic", "sex", "age_bin"])["pop"]
        .sum()
        .reset_index()
    )


# ── Totals (year × county only) ──────────────────────────────────────────────

def load_seer_totals() -> pd.DataFrame:
    """Sum SEER population across all demographic categories → year, fips, pop."""
    df = pd.read_csv(
        ROOT / "population_seer/uswbo19agesadj.csv",
        usecols=["year", "county", "pop"],
        dtype={"county": str},
        low_memory=False,
    )
    df = df[df["year"] <= 2009]
    return (
        df.rename(columns={"county": "fips"})
        .groupby(["year", "fips"])["pop"]
        .sum()
        .reset_index()
    )


def load_census_totals() -> pd.DataFrame:
    """Pull TOT_POP from AGEGRP=0 (all-age total row) → year, fips, pop."""
    usecols = ["SUMLEV", "STATE", "COUNTY", "YEAR", "AGEGRP", "TOT_POP"]

    def _read(filepath: Path, year_map: dict) -> pd.DataFrame:
        df = pd.read_csv(filepath, usecols=usecols, encoding="latin1", low_memory=False)
        df["TOT_POP"] = pd.to_numeric(df["TOT_POP"], errors="coerce")
        df = df[(df["SUMLEV"] == 50) & (df["AGEGRP"] == 0) & (df["YEAR"].isin(year_map))].copy()
        df["year"] = df["YEAR"].map(year_map)
        df["fips"] = df["STATE"].astype(str).str.zfill(2) + df["COUNTY"].astype(str).str.zfill(3)
        return df[["year", "fips", "TOT_POP"]].rename(columns={"TOT_POP": "pop"})

    year_map_2020 = {yr: yr + 2006 for yr in range(4, 14)}
    year_map_2023 = {yr: yr + 2018 for yr in range(2, 6)}
    return pd.concat([
        _read(ROOT / "population_cen/CC-EST2020-ALLDATA.csv", year_map_2020),
        _read(ROOT / "population_cen/cc-est2023-alldata.csv", year_map_2023),
    ], ignore_index=True)


def load_census() -> pd.DataFrame:
    # CC-EST2020: YEAR 2-11 → calendar years 2010-2019
    year_map_2020 = {yr: yr + 2006 for yr in range(4, 14)}   # 2→2010, 11→2019

    # cc-est2023: YEAR 2-5 → calendar years 2020-2023
    year_map_2023 = {yr: yr + 2018 for yr in range(2, 6)}    # 2→2020, 5→2023

    cc2020 = _load_census_file(ROOT / "population_cen/CC-EST2020-ALLDATA.csv", year_map_2020)
    cc2023 = _load_census_file(ROOT / "population_cen/cc-est2023-alldata.csv", year_map_2023)
    return pd.concat([cc2020, cc2023], ignore_index=True)


# ── WONDER mortality deaths ────────────────────────────────────────────────────
#
# Sources
#   Final      1999-2020  county_sex_race_hispanic_origin_adults_all_races_all_causes.parquet
#   Provisional 2021-2025  county_sex_race_hispanic_origin_adults_all_races_all_causes_provisional.parquet
#
# We use final data for 1999-2020 and provisional for 2021+ to prefer
# certified over provisional wherever both exist.
#
# Race codes used here (identical in both files):
#   White = 2106-3   Black = 2054-5
#
# Suppressed cells (<10 deaths) were excluded at query time; summing without
# them understates true totals for sparse counties — treat as a known limitation.

_WHITE_CODE = "2106-3"
_BLACK_CODE = "2054-5"


def _agg_wonder(df: pd.DataFrame, fips_col: str, race_col: str) -> pd.DataFrame:
    """
    Aggregate a WONDER long frame to one row per (year, fips) with columns:
      deaths, white_deaths, black_deaths, male_deaths, female_deaths
    All counts treat suppressed (excluded) cells as 0 — conservative lower bound.
    """
    df = df.copy()
    df["fips"] = df[fips_col].astype(str).str.zfill(5)
    df["Deaths"] = pd.to_numeric(df["Deaths"], errors="coerce").fillna(0).astype(int)

    base = ["Year", "fips"]
    out = df.groupby(base)["Deaths"].sum().rename("deaths").reset_index()

    for label, mask in [
        ("white_deaths", df[race_col] == _WHITE_CODE),
        ("black_deaths", df[race_col] == _BLACK_CODE),
        ("male_deaths",  df["Sex Code"] == "M"),
        ("female_deaths",df["Sex Code"] == "F"),
    ]:
        sub = df[mask].groupby(base)["Deaths"].sum().rename(label).reset_index()
        out = out.merge(sub, on=base, how="left")

    out = out.rename(columns={"Year": "year"})
    int_cols = ["deaths", "white_deaths", "black_deaths", "male_deaths", "female_deaths"]
    out[int_cols] = out[int_cols].fillna(0).astype("Int64")
    return out


def load_wonder_deaths() -> pd.DataFrame:
    """Return a (year, fips) deaths frame spanning 1999–2025."""
    # Final: 1999-2020
    df_f = pd.read_parquet(WONDER_FINAL)
    df_f = df_f[df_f["Year"] <= 2020]
    final = _agg_wonder(df_f, fips_col="County Code", race_col="Race Code")

    # Provisional: 2021+
    df_p = pd.read_parquet(WONDER_PROVISIONAL)
    df_p = df_p[df_p["Year"] >= 2021]
    prov  = _agg_wonder(df_p, fips_col="Residence County Code", race_col="Single Race 6 Code")

    return pd.concat([final, prov], ignore_index=True).sort_values(["year", "fips"]).reset_index(drop=True)


def load_cod_external_deaths() -> pd.DataFrame:
    """Return (year, fips, deaths_cod_external) from cod_external_injury_alladults.parquet."""
    df = pd.read_parquet(WONDER_COD_EXTERNAL)
    df["fips"] = df["County Code"].astype(str).str.zfill(5)
    df["Deaths"] = pd.to_numeric(df["Deaths"], errors="coerce").fillna(0).astype(int)
    return (
        df.groupby(["Year", "fips"])["Deaths"]
        .sum()
        .reset_index()
        .rename(columns={"Year": "year", "Deaths": "deaths_cod_external"})
        .sort_values(["year", "fips"])
        .reset_index(drop=True)
    )


def load_nber_deaths() -> pd.DataFrame:
    """
    Aggregate NBER micro-data to (year, fips) total deaths.

    Year normalization: NBER files store year as 4-digit string ('1959') or
    2-digit string ('91' → 1991, '01' → 2001).

    FIPS construction: early NBER files embed the state code as a prefix in
    countyrs (e.g. staters='01', countyrs='0101' → strip '01' → county '01'
    → fips '01001').  Rows where the resulting fips is not exactly 5 chars
    are dropped.
    """
    df = pd.read_parquet(NBER_PARQUET, columns=["year", "staters", "countyrs", "deaths"])

    # Normalize year → 4-digit int
    df["year"] = pd.to_numeric(df["year"], errors="coerce")
    df = df[df["year"].notna()].copy()
    df["year"] = df["year"].astype(int)
    df.loc[df["year"].between(40, 99), "year"] += 1900   # '91' → 1991
    df.loc[df["year"].between(0,  39), "year"] += 2000   # '01' → 2001

    # Build FIPS; strip embedded state prefix from countyrs where present
    # e.g. staters='01', countyrs='0101' → strip leading '01' → county '01'
    #      → zfill(3) → '001' → fips '01001'
    s = df["staters"].astype(str).str.zfill(2)
    c = df["countyrs"].astype(str)
    has_prefix = c.str[:2] == s
    county_part = c.where(~has_prefix, c.str[2:])
    df["fips"] = s + county_part.str.zfill(3)
    df = df[df["fips"].str.len() == 5].copy()

    return (
        df.groupby(["year", "fips"])["deaths"]
        .sum()
        .reset_index()
        .rename(columns={"deaths": "deaths_nber"})
        .sort_values(["year", "fips"])
        .reset_index(drop=True)
    )


# ── Main ───────────────────────────────────────────────────────────────────────

def main() -> None:
    print("Loading SEER (1969-2009) …")
    seer = load_seer()
    print(f"  {len(seer):,} rows, {seer['year'].nunique()} years")

    print("Loading Census (2010-2023) …")
    cen = load_census()
    print(f"  {len(cen):,} rows, {cen['year'].nunique()} years")

    combined = (
        pd.concat([seer, cen], ignore_index=True)
        .sort_values(["year", "fips", "race", "hispanic", "sex", "age_bin"])
        .reset_index(drop=True)
    )
    combined["pop"] = combined["pop"].astype("Int64")

    pq.write_table(
        pa.Table.from_pandas(combined, preserve_index=False),
        OUT_PARQUET,
        compression="snappy",
    )
    print(f"\n✓  Written → {OUT_PARQUET.resolve()}")
    print(f"   {len(combined):,} rows  |  {combined['year'].nunique()} years  "
          f"|  {combined['fips'].nunique()} counties")

    # ── Tests ──────────────────────────────────────────────────────────────────
    print("\n── Tests ─────────────────────────────────────────────────────────────")

    sample_years = [1969, 1980, 1990, 2000, 2009, 2010, 2015, 2020, 2023]

    # Test 1: Total US population by year (sum across all demographics)
    pop_yr = combined.groupby("year")["pop"].sum().reset_index()
    print("\nTest 1: Total population by year")
    print(pop_yr[pop_yr["year"].isin(sample_years)].to_string(index=False))

    # Test 2: Number of distinct counties by year
    cty_yr = combined.groupby("year")["fips"].nunique().reset_index()
    cty_yr.columns = ["year", "n_counties"]
    print("\nTest 2: County count by year")
    print(cty_yr[cty_yr["year"].isin(sample_years)].to_string(index=False))

    # Test 3: Negative population cells (residual "Other" can go negative for small cells)
    neg = combined[combined["pop"] < 0]
    print(f"\nTest 3: Cells with negative pop: {len(neg):,}  "
          f"(sum = {int(neg['pop'].sum()):,})")
    if len(neg) > 0:
        print(neg.groupby(["race", "hispanic"])["pop"].sum().reset_index().to_string(index=False))

    # Test 4: Population by age bin (select years)
    pop_age = combined.groupby(["year", "age_bin"])["pop"].sum().unstack("age_bin")
    print("\nTest 4: Population by age bin")
    print(pop_age.loc[pop_age.index.isin(sample_years)].to_string())

    # ── county_pop.parquet (totals) ────────────────────────────────────────────
    print("\nBuilding county_pop.parquet (totals) …")
    totals = (
        pd.concat([load_seer_totals(), load_census_totals()], ignore_index=True)
        .sort_values(["year", "fips"])
        .reset_index(drop=True)
    )
    totals["pop"] = totals["pop"].astype("Int64")

    pq.write_table(
        pa.Table.from_pandas(totals, preserve_index=False),
        OUT_PARQUET_TOTALS,
        compression="snappy",
    )
    print(f"✓  Written → {OUT_PARQUET_TOTALS.resolve()}")
    print(f"   {len(totals):,} rows  |  {totals['year'].nunique()} years  "
          f"|  {totals['fips'].nunique()} counties")

    # Quick sanity: totals should match sum of demographic file
    demo_sum = combined.groupby(["year"])["pop"].sum()
    tot_sum  = totals.groupby("year")["pop"].sum()
    diff = (demo_sum - tot_sum).abs()
    print(f"\nSanity: max |demo_sum − totals| across years = {diff.max():,}  "
          f"({'OK' if diff.max() < 1000 else 'CHECK'})")
    # ── Merge WONDER deaths onto county_pop.parquet ────────────────────────────
    print("\nLoading WONDER mortality deaths …")
    deaths = load_wonder_deaths()
    print(f"  {len(deaths):,} county-year rows  |  "
          f"years {deaths['year'].min()}–{deaths['year'].max()}  |  "
          f"{deaths['fips'].nunique()} counties")

    totals["year"] = totals["year"].astype(int)
    deaths["year"] = deaths["year"].astype(int)
    merged = totals.merge(deaths, on=["year", "fips"], how="left")

    death_cols = ["deaths", "white_deaths", "black_deaths", "male_deaths", "female_deaths"]
    for c in death_cols:
        merged[c] = merged[c].astype("Int64")

    # Merge external/injury COD
    cod_ext = load_cod_external_deaths()
    cod_ext["year"] = cod_ext["year"].astype(int)
    merged = merged.merge(cod_ext, on=["year", "fips"], how="left")
    merged["deaths_cod_external"] = merged["deaths_cod_external"].astype("Int64")

    all_death_cols = death_cols + ["deaths_cod_external"]

    # Fill deaths with NBER totals for years not covered by WONDER (pre-1999)
    print("\nLoading NBER mortality (filling pre-WONDER gaps) …")
    nber = load_nber_deaths()
    nber["year"] = nber["year"].astype(int)
    print(f"  {len(nber):,} county-year rows  |  "
          f"years {nber['year'].min()}–{nber['year'].max()}  |  "
          f"{nber['fips'].nunique()} counties")
    merged = merged.merge(nber, on=["year", "fips"], how="left")
    # Coalesce: keep WONDER deaths where present; fill from NBER otherwise
    merged["deaths"] = merged["deaths"].combine_first(
        pd.to_numeric(merged["deaths_nber"], errors="coerce")
    ).astype("Int64")
    merged = merged.drop(columns=["deaths_nber"])

    nber_filled = merged["deaths"].notna().sum()
    print(f"  Coverage after NBER fill: {nber_filled:,} / {len(merged):,} "
          f"({100*nber_filled/len(merged):.1f}%)", flush=True)

    pq.write_table(
        pa.Table.from_pandas(merged, preserve_index=False),
        OUT_PARQUET_TOTALS,
        compression="snappy",
    )
    print(f"✓  Updated → {OUT_PARQUET_TOTALS.resolve()}")
    print(f"   {len(merged):,} rows  |  {merged['year'].nunique()} years  "
          f"|  {merged['fips'].nunique()} counties")

    # Coverage check
    with_deaths = merged["deaths"].notna().sum()
    print(f"   Rows with deaths data: {with_deaths:,} / {len(merged):,} "
          f"({100*with_deaths/len(merged):.1f}%)")
    print("\nSample (Autauga County AL, 2010-2012):")
    print(merged[(merged["fips"]=="01001") & merged["year"].between(2010,2012)]
          [["year","fips","pop"] + all_death_cols].to_string(index=False))

if __name__ == "__main__":
    main()

