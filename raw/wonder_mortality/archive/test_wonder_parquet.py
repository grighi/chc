"""
test_wonder_parquet.py
======================
Sanity checks for Parquet files produced by pull_wonder.py.

Run with pytest:

    pytest wonder_mortality/test_wonder_parquet.py -v

Or directly:

    python wonder_mortality/test_wonder_parquet.py
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np
import pytest

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

DATA_DIR = Path(__file__).parent / "data"

def _load(filename: str) -> pd.DataFrame:
    """Load a parquet file from DATA_DIR."""
    path = DATA_DIR / filename
    if not path.exists():
        pytest.skip(f"File not found: {path}")
    return pd.read_parquet(str(path))


def _list_parquets():
    """Return all parquet files in DATA_DIR."""
    return list(DATA_DIR.glob("*.parquet"))


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(params=[p.name for p in _list_parquets()], scope="module")
def df(request):
    """Parametrize over every parquet file in data/."""
    return _load(request.param)


# ---------------------------------------------------------------------------
# Tests that apply to EVERY parquet file
# ---------------------------------------------------------------------------

class TestStructure:
    def test_not_empty(self, df):
        """File contains at least one row."""
        assert len(df) > 0, "Parquet file is empty"

    def test_required_columns_present(self, df):
        """At minimum these columns must be present."""
        required = {"Deaths", "Population", "Crude Rate"}
        missing = required - set(df.columns)
        assert not missing, f"Missing columns: {missing}"

    def test_notes_column_absent_or_empty(self, df):
        """
        The 'Notes' column is a WONDER footer artefact; it should have been
        stripped.  If it survived, every value must be blank.
        """
        if "Notes" in df.columns:
            non_empty = df["Notes"].dropna().astype(str).str.strip()
            assert (non_empty == "").all(), \
                f"Non-empty Notes values found:\n{non_empty[non_empty != ''].head()}"

    def test_no_duplicate_key_rows(self, df):
        """
        Key columns (geography + time) should not have duplicates.
        We detect key cols heuristically.
        """
        key_candidates = [
            c for c in df.columns
            if c.lower() in {
                "county code", "state code", "county", "state",
                "year", "year code",
                "age group", "race", "sex", "hispanic origin",
            }
        ]
        if not key_candidates:
            pytest.skip("No key columns detected for dedup check")
        dups = df.duplicated(subset=key_candidates)
        assert not dups.any(), \
            f"{dups.sum()} duplicate rows on {key_candidates}"


class TestGeography:
    def test_county_code_length(self, df):
        """County FIPS codes must be exactly 5 characters (zero-padded)."""
        col = next((c for c in df.columns if "county code" in c.lower()), None)
        if col is None:
            pytest.skip("No 'County Code' column")
        codes = df[col].dropna().astype(str).str.strip()
        bad = codes[codes.str.len() != 5]
        assert bad.empty, \
            f"{len(bad)} county codes with unexpected length:\n{bad.value_counts().head()}"

    def test_county_code_numeric(self, df):
        """County FIPS codes must be all-digits after stripping."""
        col = next((c for c in df.columns if "county code" in c.lower()), None)
        if col is None:
            pytest.skip("No 'County Code' column")
        codes = df[col].dropna().astype(str).str.strip()
        bad = codes[~codes.str.match(r"^\d{5}$")]
        assert bad.empty, \
            f"Non-numeric county codes: {bad.value_counts().head().to_dict()}"

    def test_state_code_range(self, df):
        """State FIPS codes (if present) must be 01–56 (no territory outliers)."""
        col = next(
            (c for c in df.columns
             if c.lower() in {"state code", "hhs region code", "census region code"}),
            None,
        )
        if col is None:
            pytest.skip("No state/region code column")
        codes = pd.to_numeric(df[col], errors="coerce").dropna()
        assert codes.between(1, 56).all(), \
            f"State codes outside 01-56: {codes[~codes.between(1, 56)].unique()}"


class TestYears:
    def test_year_range_valid(self, df):
        """All years must be within WONDER's valid range (1999–2020)."""
        col = next((c for c in df.columns if c.lower() == "year"), None)
        if col is None:
            pytest.skip("No 'Year' column")
        years = pd.to_numeric(df[col], errors="coerce").dropna()
        assert years.between(1999, 2020).all(), \
            f"Years outside 1999-2020: {years[~years.between(1999, 2020)].unique()}"

    def test_years_are_integers(self, df):
        col = next((c for c in df.columns if c.lower() == "year"), None)
        if col is None:
            pytest.skip("No 'Year' column")
        frac = pd.to_numeric(df[col], errors="coerce") % 1
        assert (frac == 0).all(), "Non-integer values in Year column"


class TestDeaths:
    def test_deaths_numeric_or_nan(self, df):
        """Deaths column must be numeric (non-suppressed rows)."""
        if "Deaths" not in df.columns:
            pytest.skip("No Deaths column")
        non_null = df["Deaths"].dropna()
        assert pd.api.types.is_numeric_dtype(non_null), \
            "Deaths column contains non-numeric values after parsing"

    def test_deaths_non_negative(self, df):
        """Observed death counts must be ≥ 0."""
        if "Deaths" not in df.columns:
            pytest.skip("No Deaths column")
        neg = df["Deaths"].dropna()
        assert (neg >= 0).all(), \
            f"Negative death counts found: {neg[neg < 0].tolist()}"

    def test_deaths_sum_positive(self, df):
        """At least one year should have a positive death total."""
        if "Deaths" not in df.columns:
            pytest.skip("No Deaths column")
        total = df["Deaths"].sum(min_count=1)
        assert total > 0, "Sum of Deaths is zero or NaN — no data retrieved?"

    def test_suppression_rate_not_extreme(self, df):
        """
        Suppression is expected for small counties, but if >50% of rows are
        suppressed the query may be mis-configured.
        """
        if "Deaths" not in df.columns:
            pytest.skip("No Deaths column")
        total_rows  = len(df)
        suppressed  = df["Deaths"].isna().sum()
        rate        = suppressed / total_rows if total_rows else 0
        assert rate < 0.50, \
            f"High suppression rate: {rate:.1%} ({suppressed}/{total_rows} rows)"


class TestCrudeRate:
    def test_crude_rate_plausible(self, df):
        """
        Crude rates (deaths per 100,000) should be between 0 and ~5000.
        Values outside this range suggest a parse error.
        """
        col = "Crude Rate"
        if col not in df.columns:
            pytest.skip("No 'Crude Rate' column")
        rates = df[col].dropna()
        assert (rates >= 0).all(),    "Negative crude rates found"
        assert (rates <= 5000).all(), \
            f"Crude rate > 5000 found: {rates[rates > 5000].tolist()}"

    def test_crude_rate_consistent_with_deaths(self, df):
        """
        Where Deaths and Population are both observed,
        crude rate ≈ Deaths / Population × 100,000 (within 5 %).
        """
        if not {"Deaths", "Population", "Crude Rate"}.issubset(df.columns):
            pytest.skip("Missing required columns")
        sub = df[["Deaths", "Population", "Crude Rate"]].dropna()
        if sub.empty:
            pytest.skip("No fully-observed rows")
        computed = sub["Deaths"] / sub["Population"] * 100_000
        rel_err  = (computed - sub["Crude Rate"]).abs() / sub["Crude Rate"].clip(lower=1)
        bad = rel_err[rel_err > 0.05]
        assert bad.empty, \
            f"{len(bad)} rows where crude rate deviates >5% from Deaths/Pop×100k"


class TestPopulation:
    def test_population_positive(self, df):
        """Population must be strictly positive where observed."""
        if "Population" not in df.columns:
            pytest.skip("No Population column")
        pop = df["Population"].dropna()
        assert (pop > 0).all(), "Zero or negative population values found"

    def test_population_reasonable_range(self, df):
        """
        US county populations range from ~100 (Loving County TX) to ~10M.
        Values outside 50 to 12_000_000 are suspicious.
        """
        if "Population" not in df.columns:
            pytest.skip("No Population column")
        pop = df["Population"].dropna()
        assert (pop >= 50).all(),           f"Population < 50: {pop[pop < 50].tolist()}"
        assert (pop <= 12_000_000).all(),   \
            f"Population > 12M: {pop[pop > 12_000_000].tolist()}"


# ---------------------------------------------------------------------------
# Stand-alone runner (no pytest required)
# ---------------------------------------------------------------------------

def _run_standalone():
    files = _list_parquets()
    if not files:
        print(f"No parquet files found in {DATA_DIR}")
        return

    all_pass = True
    for f in files:
        print(f"\n{'='*60}")
        print(f"  File: {f.name}")
        print(f"{'='*60}")
        try:
            df = _load(f.name)
        except Exception as e:
            print(f"  LOAD ERROR: {e}")
            all_pass = False
            continue

        checks = {
            "not_empty":           lambda d: len(d) > 0,
            "Deaths column":       lambda d: "Deaths" in d.columns,
            "Population column":   lambda d: "Population" in d.columns,
            "Crude Rate column":   lambda d: "Crude Rate" in d.columns,
            "deaths non-negative": lambda d: "Deaths" not in d.columns or (d["Deaths"].dropna() >= 0).all(),
            "deaths sum > 0":      lambda d: "Deaths" not in d.columns or d["Deaths"].sum(min_count=1) > 0,
            "years in 1999-2020":  lambda d: "Year" not in d.columns or
                                             pd.to_numeric(d["Year"], errors="coerce").dropna().between(1999,2020).all(),
            "county FIPS 5-digit": lambda d: next(
                (c for c in d.columns if "county code" in c.lower()), None
            ) is None or d[next(c for c in d.columns if "county code" in c.lower())]
                .dropna().astype(str).str.strip().str.match(r"^\d{5}$").all(),
            "crude rate 0-5000":   lambda d: "Crude Rate" not in d.columns or
                                             (d["Crude Rate"].dropna().between(0,5000).all()),
        }

        print(f"  Rows: {len(df):,}  |  Columns: {list(df.columns)}")
        for name, fn in checks.items():
            try:
                ok = fn(df)
                print(f"  {'✓' if ok else '✗'}  {name}")
                if not ok:
                    all_pass = False
            except Exception as e:
                print(f"  ?  {name}  [error: {e}]")

    print()
    if all_pass:
        print("All checks passed.")
    else:
        print("Some checks FAILED — review output above.")
        sys.exit(1)


if __name__ == "__main__":
    _run_standalone()
