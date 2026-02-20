#!/usr/bin/env python3
"""
fill_tables.py — Generate publication-quality LaTeX tables from CSV outputs.

Reads CSV files from build/output/data/ and fills table shells in
build/output/tables/, producing final .tex files ready for inclusion.

Usage:
    python3 build/fill_tables.py
"""

import csv
import os
from pathlib import Path

BUILD_DIR = Path(__file__).parent
CSV_DIR = BUILD_DIR / "output" / "data"
TABLE_DIR = BUILD_DIR / "output" / "tables"


def read_csv_rows(filename):
    """Read a CSV file and return list of dicts."""
    path = CSV_DIR / filename
    if not path.exists():
        print(f"  WARNING: {filename} not found. Skipping.")
        return None
    with open(path) as f:
        return list(csv.DictReader(f))


def fmt(val, decimals=3):
    """Format a numeric value for LaTeX."""
    try:
        v = float(val)
        return f"{v:.{decimals}f}"
    except (ValueError, TypeError):
        return "--"


def fmt_stars(coef, se):
    """Add significance stars."""
    try:
        t = abs(float(coef) / float(se))
        if t > 2.576:
            return "***"
        elif t > 1.96:
            return "**"
        elif t > 1.645:
            return "*"
    except (ValueError, TypeError, ZeroDivisionError):
        pass
    return ""


def write_tex(filename, content):
    """Write a LaTeX file."""
    path = TABLE_DIR / filename
    with open(path, "w") as f:
        f.write(content)
    print(f"  Wrote: {path}")


# ============================================================================
# Table 1: Balance Table
# ============================================================================
def fill_table01():
    rows = read_csv_rows("table01_balance.csv")
    if rows is None:
        return

    # Read the shell
    shell_path = TABLE_DIR / "table01_balance_shell.tex"
    if not shell_path.exists():
        print("  WARNING: table01_balance_shell.tex not found.")
        return

    body_lines = []
    for r in rows:
        stars = fmt_stars(r.get("diff", 0), r.get("se", 1))
        body_lines.append(
            f"{r['variable']} & {fmt(r['mean_treat'])} & {fmt(r['mean_ctrl'])} "
            f"& {fmt(r['diff'])}{stars} & ({fmt(r['se'])}) & {fmt(r['pval'])} \\\\"
        )

    body = "\n".join(body_lines)

    with open(shell_path) as f:
        shell = f.read()
    tex = shell.replace("%%TABLE_BODY%%", body)
    write_tex("table01_balance.tex", tex)


# ============================================================================
# Table 2: DD Robustness
# ============================================================================
def fill_table02():
    rows = read_csv_rows("table02_dd_robustness_full.csv")
    if rows is None:
        return

    shell_path = TABLE_DIR / "table02_dd_robustness_shell.tex"
    if not shell_path.exists():
        # Generate standalone table
        pass

    lines = []
    for r in rows:
        stars = fmt_stars(r.get("coef", 0), r.get("se", 1))
        lines.append(
            f"{r['spec']} & {fmt(r['coef'])}{stars} & ({fmt(r['se'])}) "
            f"& {r.get('n', '--')} & {fmt(r.get('pval', ''))} \\\\"
        )

    body = "\n".join(lines)

    tex = (
        "\\begin{table}[htbp]\n"
        "\\centering\n"
        "\\caption{Difference-in-Differences Estimates: CMHC Effect on AMR (Ages 20-49)}\n"
        "\\label{tab:dd_robust}\n"
        "\\small\n"
        "\\begin{tabular}{lcccc}\n"
        "\\toprule\n"
        "Specification & Coefficient & SE & $N$ & $p$-value \\\\\n"
        "\\midrule\n"
        f"{body}\n"
        "\\bottomrule\n"
        "\\end{tabular}\n"
        "\\end{table}\n"
    )
    write_tex("table02_dd_robustness.tex", tex)


# ============================================================================
# Table 3: Cause Decomposition
# ============================================================================
def fill_table03():
    rows = read_csv_rows("table03_cause_decomp_cmhc.csv")
    if rows is None:
        return

    lines = []
    for r in rows:
        stars = fmt_stars(r.get("post_mean", 0), r.get("post_se", 1))
        pre_p = fmt(r.get("pre_trend_p", ""), 3) if r.get("pre_trend_p") else "--"
        lines.append(
            f"{r['cause']} & {fmt(r['post_mean'])}{stars} "
            f"& ({fmt(r['post_se'])}) & {pre_p} \\\\"
        )

    body = "\n".join(lines)

    tex = (
        "\\begin{table}[htbp]\n"
        "\\centering\n"
        "\\caption{Cause-Specific Mortality Decomposition: CMHC Effects (Ages 20-49)}\n"
        "\\label{tab:cause_decomp}\n"
        "\\small\n"
        "\\begin{tabular}{lccc}\n"
        "\\toprule\n"
        "Cause of Death & Post-Treatment Mean & SE & Pre-Trend $p$ \\\\\n"
        "\\midrule\n"
        f"{body}\n"
        "\\bottomrule\n"
        "\\end{tabular}\n"
        "\\end{table}\n"
    )
    write_tex("table03_cause_decomp_cmhc.tex", tex)


# ============================================================================
# Table: CMHC Incarceration Long-Difference
# ============================================================================
def fill_table_incarceration():
    rows = read_csv_rows("table_cmhc_incarceration_ld.csv")
    if rows is None:
        return

    # Rows are by end_year
    cols = []
    for r in rows:
        stars = fmt_stars(r.get("coef", 0), r.get("se", 1))
        cols.append(f"{fmt(r['coef'])}{stars}")

    coef_line = "CMHC & " + " & ".join(cols) + " \\\\"
    se_line = " & " + " & ".join([f"({fmt(r['se'])})" for r in rows]) + " \\\\"
    n_line = "$N$ & " + " & ".join([str(r.get("n", "--")) for r in rows]) + " \\\\"

    tex = (
        "\\begin{table}[htbp]\n"
        "\\centering\n"
        "\\caption{Long-Difference: CMHC Effect on $\\Delta\\log$(Jail Rate)}\n"
        "\\label{tab:incarceration_ld}\n"
        "\\small\n"
        "\\begin{tabular}{lccccc}\n"
        "\\toprule\n"
        " & 1970-78 & 1970-83 & 1970-88 & 1970-93 & 1970-00 \\\\\n"
        "\\midrule\n"
        f"{coef_line}\n"
        f"{se_line}\n"
        "\\midrule\n"
        f"{n_line}\n"
        "\\bottomrule\n"
        "\\end{tabular}\n"
        "\\end{table}\n"
    )
    write_tex("table_cmhc_incarceration_ld.tex", tex)


# ============================================================================
# Table: CCBHC Heterogeneity
# ============================================================================
def fill_table_ccbhc_het():
    rows = read_csv_rows("table_ccbhc_heterogeneity.csv")
    if rows is None:
        return

    lines = []
    for r in rows:
        stars = fmt_stars(r.get("post_mean", 0), r.get("post_se", 1))
        lines.append(
            f"{r['group']} & {fmt(r['post_mean'])}{stars} "
            f"& ({fmt(r['post_se'])}) & {r.get('n_treated', '--')} "
            f"& {r.get('n_control', '--')} \\\\"
        )

    body = "\n".join(lines)

    tex = (
        "\\begin{table}[htbp]\n"
        "\\centering\n"
        "\\caption{CCBHC Mortality Effects: Heterogeneity by Pre-Treatment Mortality}\n"
        "\\label{tab:ccbhc_het}\n"
        "\\small\n"
        "\\begin{tabular}{lcccc}\n"
        "\\toprule\n"
        "Group & Post-Treatment Mean & SE & $N$ Treated & $N$ Control \\\\\n"
        "\\midrule\n"
        f"{body}\n"
        "\\bottomrule\n"
        "\\end{tabular}\n"
        "\\end{table}\n"
    )
    write_tex("table_ccbhc_heterogeneity.tex", tex)


# ============================================================================
# Table: Poisson Appendix
# ============================================================================
def fill_table_poisson():
    rows = read_csv_rows("appendix_poisson_comparison.csv")
    if rows is None:
        return

    lines = []
    for r in rows:
        lines.append(f"{r['spec']} & {fmt(r['post_mean'])} \\\\")

    body = "\n".join(lines)

    tex = (
        "\\begin{table}[htbp]\n"
        "\\centering\n"
        "\\caption{CMHC Event Study: Poisson vs.\\ Linear Specification}\n"
        "\\label{tab:poisson}\n"
        "\\small\n"
        "\\begin{tabular}{lc}\n"
        "\\toprule\n"
        "Specification & Post-Treatment Mean \\\\\n"
        "\\midrule\n"
        f"{body}\n"
        "\\bottomrule\n"
        "\\end{tabular}\n"
        "\\end{table}\n"
    )
    write_tex("table_appendix_poisson.tex", tex)


# ============================================================================
# Main
# ============================================================================
if __name__ == "__main__":
    print("=" * 60)
    print("fill_tables.py: Generating LaTeX tables from CSV outputs")
    print("=" * 60)

    fill_table01()
    fill_table02()
    fill_table03()
    fill_table_incarceration()
    fill_table_ccbhc_het()
    fill_table_poisson()

    print("\nDone.")
