#!/usr/bin/env python3
"""
figure_deaths_of_despair.py
===========================
National deaths-of-despair rate trend (all adults, ages 20–59) using
CDC WONDER final data (1999–2020).

Source: wonder_mortality/data/dod_alladults.parquet
  – DoD definition: suicide + drug overdose + alcohol liver disease
    (UCD filter: X40-X45, X60-X65, X70-X84, Y10-Y15, K70, K73-K74)
  – Age range: 20–59 (configured in pull_wonder_unified.py)
  – Geography: county × year

Suppression diagnostic is printed to stdout before the figure is made.

Output:
    figure_deaths_of_despair.png
"""

from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker

# ── Paths ─────────────────────────────────────────────────────────────────────

ROOT    = Path(__file__).parent
DOD_PQ  = ROOT / "wonder_mortality" / "data" / "dod_alladults.parquet"
OUT_FIG = ROOT / "figure_deaths_of_despair.png"


# ── Suppression diagnostic ────────────────────────────────────────────────────

def print_suppression_summary(df: pd.DataFrame) -> None:
    """
    Print the share of deaths that fall in county-years where AAR is suppressed
    (i.e. Age Adjusted Rate is NaN).  Helps quantify the impact of WONDER's
    <20-death suppression rule on national aggregates.
    """
    total_deaths   = df["Deaths"].sum()
    suppressed     = df[df["Age Adjusted Rate"].isna()]
    supp_deaths    = suppressed["Deaths"].sum()       # NaN deaths count as 0 in sum
    supp_rows      = len(suppressed)
    total_rows     = len(df)

    # County-years where Death count itself is suppressed (NaN) but row exists
    both_nan       = df[df["Deaths"].isna() & df["Age Adjusted Rate"].isna()]

    print("── Suppression diagnostic (dod_alladults.parquet) ──────────────────")
    print(f"  Total county-year rows        : {total_rows:,}")
    print(f"  Rows where AAR is NaN         : {supp_rows:,}  ({100*supp_rows/total_rows:.1f}%)")
    print(f"  Rows where Deaths AND AAR NaN : {len(both_nan):,}")
    print(f"  Known deaths in NaN-AAR rows  : {supp_deaths:,.0f}")
    print(f"  Total known deaths (all rows) : {total_deaths:,.0f}")
    pct = 100 * supp_deaths / total_deaths if total_deaths > 0 else float("nan")
    print(f"  Suppressed share of deaths    : {pct:.1f}%")
    print()
    print("  By year (known deaths in AAR-suppressed county-years):")
    by_year = (
        suppressed.groupby("Year")["Deaths"]
        .sum()
        .reset_index()
        .rename(columns={"Deaths": "supp_deaths"})
    )
    total_by_year = df.groupby("Year")["Deaths"].sum().reset_index().rename(
        columns={"Deaths": "total_deaths"})
    merged = by_year.merge(total_by_year, on="Year")
    merged["pct"] = 100 * merged["supp_deaths"] / merged["total_deaths"]
    print(merged.to_string(index=False))
    print("────────────────────────────────────────────────────────────────────")
    print()


# ── Aggregate to national ─────────────────────────────────────────────────────

def national_dod(df: pd.DataFrame) -> pd.DataFrame:
    """
    Sum Deaths and Population across all county-year rows, then compute
    a crude rate per 100,000.  NaN deaths (fully suppressed cells) are
    treated as missing and excluded from the numerator sum; their population
    is still included in the denominator, so the rate is a lower bound.
    """
    agg = (
        df.groupby("Year")
        .agg(
            deaths=("Deaths", "sum"),        # NaN excluded by default
            pop=("Population", "sum"),
        )
        .reset_index()
        .rename(columns={"Year": "year"})
    )
    agg["rate"] = agg["deaths"] / agg["pop"] * 100_000
    return agg.sort_values("year")


# ── Figure ────────────────────────────────────────────────────────────────────

def make_figure(agg: pd.DataFrame) -> None:
    fig, ax = plt.subplots(figsize=(8, 4.5))

    ax.plot(agg["year"], agg["rate"], color="#1f4e79", linewidth=2.2,
            marker="o", markersize=3.5, zorder=3)

    ax.set_xlabel("Year", fontsize=11)
    ax.set_ylabel("Deaths per 100,000", fontsize=11)
    ax.set_title("Deaths of Despair, all adults (ages 20–59)\n"
                 "All races, United States, 1999–2020",
                 fontsize=12, pad=10)

    ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f"{x:,.1f}"))
    ax.set_xlim(agg["year"].min() - 0.3, agg["year"].max() + 0.3)
    ax.tick_params(axis="both", labelsize=9.5)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.text(0.02, 0.04,
            "Suicide, drug overdose, alcohol liver disease\n"
            "(ICD-10 X40–X45, X60–X65, X70–X84, Y10–Y15, K70, K73–K74)",
            transform=ax.transAxes, fontsize=7.5, color="grey", va="bottom")

    fig.tight_layout()
    fig.savefig(OUT_FIG, dpi=200, bbox_inches="tight")
    print(f"✓  Saved → {OUT_FIG.resolve()}")


# ── Main ─────────────────────────────────────────────────────────────────────

def main() -> None:
    print(f"Loading {DOD_PQ} …\n")
    df = pd.read_parquet(DOD_PQ)

    print_suppression_summary(df)

    agg = national_dod(df)
    print("National DoD rate (per 100,000):")
    print(agg[["year", "deaths", "pop", "rate"]].round(1).to_string(index=False))
    print()

    make_figure(agg)


if __name__ == "__main__":
    main()
