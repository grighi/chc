#!/usr/bin/env python3
"""
figure_ccbhc_dod.py
===================
Differential change in Deaths of Despair (DoD) by CCBHC treatment status,
county × year, 2010–2020.

Data sources:
  - wonder_mortality/data/dod_alladults.parquet   (WONDER final, 1999–2020)
      DoD = suicide + drug overdose + alcohol liver disease (ages 20–59)
  - build/data/ccbhc_panel.parquet               (CCBHC treatment panel)

Strategy:
  - Merge WONDER DoD deaths/pop onto CCBHC panel for 2010–2020.
  - Compute aggregate (sum deaths / sum pop × 100k) by year × group to
    sidestep WONDER suppression at the county level.
  - Panel A: raw aggregate DoD trends, treated vs. control.
  - Panel B: county-demeaned differences (treated minus control), year-by-year,
    with 95% CI from county-level variation.

Treatment groups:
  - "Treated 2018" : counties whose CCBHC first grant was 2018 (N=57)
  - "Never treated": counties with treated == 0 throughout (N=2,744)
  We focus on the 2018-cohort because it is the only cohort with multiple
  post-period years in the 1999–2020 WONDER data.

Output:
  raw/figure_ccbhc_dod.png
"""

from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.patches as mpatches

# ── Paths ──────────────────────────────────────────────────────────────────────
ROOT      = Path(__file__).parent
DOD_PQ    = ROOT / "wonder_mortality" / "data" / "dod_alladults.parquet"
PANEL_PQ  = ROOT.parent / "build" / "data" / "ccbhc_panel.parquet"
OUT_FIG   = ROOT / "figure_ccbhc_dod.png"

TREAT_YEAR = 2018          # cohort to highlight


# ── Load & merge ───────────────────────────────────────────────────────────────

def load_panel() -> pd.DataFrame:
    ccbhc = pd.read_parquet(PANEL_PQ)
    dod   = pd.read_parquet(DOD_PQ)

    # Restrict to overlapping years
    ccbhc = ccbhc[ccbhc["year"].between(2010, 2020)].copy()

    # Rename WONDER columns to avoid collision with ccbhc panel's own Deaths col
    dod_slim = (
        dod[["County Code", "Year", "Deaths", "Population"]]
        .rename(columns={"Deaths": "dod_deaths", "Population": "dod_pop",
                         "County Code": "fips", "Year": "year"})
    )
    dod_slim["year"] = dod_slim["year"].astype(int)

    merged = ccbhc.merge(dod_slim, on=["fips", "year"], how="left")

    # Assign comparison groups
    # "cohort_2018": first treated in 2018
    # "never":       never treated (treated == 0)
    merged["group"] = np.where(
        merged["treat_year"] == TREAT_YEAR, "Treated (2018 cohort)",
        np.where(merged["treated"] == 0, "Never treated", "Other treated")
    )

    return merged


# ── Panel A: aggregate rates ────────────────────────────────────────────────────

def aggregate_rates(df: pd.DataFrame) -> pd.DataFrame:
    """
    Sum dod_deaths and dod_pop by year × group, then compute rate.
    Aggregating before dividing avoids bias from suppressed small-county rates.
    """
    agg = (
        df[df["group"].isin(["Treated (2018 cohort)", "Never treated"])]
        .groupby(["year", "group"])
        .agg(deaths=("dod_deaths", "sum"), pop=("dod_pop", "sum"))
        .reset_index()
    )
    agg["rate"] = agg["deaths"] / agg["pop"] * 100_000
    return agg


# ── Panel B: county-demeaned year effects ──────────────────────────────────────

def demeaned_effects(df: pd.DataFrame) -> pd.DataFrame:
    """
    For each county, compute dod_rate = dod_deaths / dod_pop × 100k.
    Demean by county mean rate (2010–2017 baseline), then take the
    population-weighted mean residual by year × group.
    Returns rows with year, group, mean_resid, se, ci_lo, ci_hi.
    """
    sub = df[df["group"].isin(["Treated (2018 cohort)", "Never treated"])].copy()
    sub = sub[sub["dod_deaths"].notna() & sub["dod_pop"].notna() & (sub["dod_pop"] > 0)]
    sub["rate"] = sub["dod_deaths"] / sub["dod_pop"] * 100_000

    # County mean over 2010-2017 pre-period
    pre = sub[sub["year"] <= 2017].groupby("fips")["rate"].mean().rename("rate_pre")
    sub = sub.merge(pre, on="fips", how="left")
    sub["resid"] = sub["rate"] - sub["rate_pre"]

    def _wagg(g):
        w  = g["dod_pop"]
        wm = np.average(g["resid"], weights=w)
        # Weighted SE via weighted variance
        n  = len(g)
        if n < 3:
            return pd.Series({"mean_resid": wm, "se": np.nan, "n": n})
        wvar = np.average((g["resid"] - wm)**2, weights=w)
        se   = np.sqrt(wvar / n)
        return pd.Series({"mean_resid": wm, "se": se, "n": n})

    out = (
        sub.groupby(["year", "group"])
        .apply(_wagg, include_groups=False)
        .reset_index()
    )
    out["ci_lo"] = out["mean_resid"] - 1.96 * out["se"]
    out["ci_hi"] = out["mean_resid"] + 1.96 * out["se"]
    return out


# ── Figure ─────────────────────────────────────────────────────────────────────

COLORS = {
    "Treated (2018 cohort)": "#c0392b",
    "Never treated":         "#2c3e7a",
}
TREAT_LINE = TREAT_YEAR - 0.5   # vertical line between 2017 and 2018


def _shade_post(ax, x_start, x_end):
    ax.axvspan(x_start, x_end, color="#f5c6c6", alpha=0.35, zorder=0)


def make_figure(agg: pd.DataFrame, dem: pd.DataFrame, df_merged: pd.DataFrame) -> None:
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.8))
    years = sorted(agg["year"].unique())

    # ── Panel A: Raw aggregate rates ──────────────────────────────────────────
    ax = axes[0]
    for grp, color in COLORS.items():
        g = agg[agg["group"] == grp].sort_values("year")
        label = grp
        ax.plot(g["year"], g["rate"], color=color, linewidth=2.0,
                marker="o", markersize=4, label=label, zorder=3)

    _shade_post(ax, TREAT_LINE, max(years) + 0.5)
    ax.axvline(TREAT_LINE, color="grey", linewidth=0.8, linestyle="--", zorder=2)
    ax.set_xlabel("Year", fontsize=10)
    ax.set_ylabel("Deaths per 100,000 (ages 20–59)", fontsize=10)
    ax.set_title("A. Aggregate DoD mortality rate\nby CCBHC treatment status",
                 fontsize=10.5, loc="left")
    ax.set_xlim(min(years) - 0.5, max(years) + 0.5)
    ax.tick_params(labelsize=9)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.legend(fontsize=8.5, frameon=False)

    # N-county annotation
    n_treat   = df_merged[df_merged["group"] == "Treated (2018 cohort)"]["fips"].nunique()
    n_control = df_merged[df_merged["group"] == "Never treated"]["fips"].nunique()
    ax.text(0.02, 0.04,
            f"N treated = {n_treat:,}  |  N control = {n_control:,}\n"
            "Rate = sum(deaths) / sum(pop) × 100k",
            transform=ax.transAxes, fontsize=7, color="grey", va="bottom")

    # ── Panel B: County-demeaned residuals ────────────────────────────────────
    ax = axes[1]
    for grp, color in COLORS.items():
        g = dem[dem["group"] == grp].sort_values("year")
        ax.plot(g["year"], g["mean_resid"], color=color, linewidth=2.0,
                marker="o", markersize=4, label=grp, zorder=3)
        # 95% CI band
        ci_ok = g["ci_lo"].notna() & g["ci_hi"].notna()
        if ci_ok.any():
            ax.fill_between(g.loc[ci_ok, "year"],
                            g.loc[ci_ok, "ci_lo"],
                            g.loc[ci_ok, "ci_hi"],
                            color=color, alpha=0.15)

    _shade_post(ax, TREAT_LINE, max(years) + 0.5)
    ax.axvline(TREAT_LINE, color="grey", linewidth=0.8, linestyle="--", zorder=2)
    ax.axhline(0, color="black", linewidth=0.6, linestyle=":", zorder=2)
    ax.set_xlabel("Year", fontsize=10)
    ax.set_ylabel("Change vs. county pre-period mean (deaths / 100k)", fontsize=10)
    ax.set_title("B. County-demeaned DoD rate\n(deviation from 2010–2017 mean)",
                 fontsize=10.5, loc="left")
    ax.set_xlim(min(years) - 0.5, max(years) + 0.5)
    ax.tick_params(labelsize=9)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.legend(fontsize=8.5, frameon=False)

    ax.text(0.02, 0.04,
            "Shading = ±1.96 SE (weighted). Pre-period baseline = 2010–2017.",
            transform=ax.transAxes, fontsize=7, color="grey", va="bottom")

    # ── Shared footer ─────────────────────────────────────────────────────────
    fig.suptitle(
        "Deaths of Despair (suicide, drug overdose, alcohol liver disease) — "
        "CCBHC-treated vs. never-treated counties, 2010–2020\n"
        "CDC WONDER final data (D77), adults ages 20–59. "
        "Crude rates aggregate over counties to avoid WONDER suppression.",
        fontsize=8.5, y=0.01, va="bottom", color="dimgrey"
    )

    fig.tight_layout(rect=[0, 0.06, 1, 1])
    fig.savefig(OUT_FIG, dpi=200, bbox_inches="tight")
    print(f"✓  Saved → {OUT_FIG.resolve()}")


# ── Main ───────────────────────────────────────────────────────────────────────

def main() -> None:
    print("Loading data …")
    df = load_panel()

    print(f"  Merged rows: {len(df):,}")
    for grp in ["Treated (2018 cohort)", "Never treated", "Other treated"]:
        n = df[df["group"] == grp]["fips"].nunique()
        print(f"  {grp}: {n} counties")
    print()

    agg = aggregate_rates(df)
    print("Aggregate DoD rates (per 100,000):")
    print(agg.pivot(index="year", columns="group", values="rate").round(1).to_string())
    print()

    dem = demeaned_effects(df)

    make_figure(agg, dem, df)


if __name__ == "__main__":
    main()
