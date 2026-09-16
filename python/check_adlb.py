"""
Independent cross-language check of ADLB, reading the SAS Transport file.

Why this exists: the target role migrates SAS -> R and Python. The .xpt files
are language-neutral, so a second language can re-verify the R derivations by a
different door -- pandas here, nothing from the R side is imported.

Run from the project root:  python3 python/check_adlb.py
Requires only pandas (pd.read_sas handles XPT v5 natively).
"""
from pathlib import Path
import sys
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
adlb = pd.read_sas(ROOT / "data/adam/adlb.xpt", format="xport", encoding="latin-1")

failures = []
def check(name, ok, detail=""):
    print(f"{'OK  ' if ok else 'FAIL'} {name}{(' -- ' + detail) if detail else ''}")
    if not ok:
        failures.append(name)

# 1. Exactly one baseline flag per subject x parameter
n_flags = adlb[adlb["ABLFL"] == "Y"].groupby(["USUBJID", "PARAMCD"]).size()
n_groups = adlb.groupby(["USUBJID", "PARAMCD"]).ngroups
check("one ABLFL per subject/parameter", (n_flags == 1).all() and len(n_flags) == n_groups,
      f"{len(n_flags)} flagged of {n_groups} groups")

# 2. Baseline rows are on or before first dose and have a value
base_rows = adlb[adlb["ABLFL"] == "Y"]
check("ABLFL rows have ADT <= TRTSDT", (base_rows["ADT"] <= base_rows["TRTSDT"]).all())
check("ABLFL rows have non-missing AVAL", base_rows["AVAL"].notna().all())

# 3. CHG == AVAL - BASE where both present
both = adlb.dropna(subset=["CHG", "AVAL", "BASE"])
check("CHG == AVAL - BASE", ((both["CHG"] - (both["AVAL"] - both["BASE"])).abs() < 1e-9).all(),
      f"{len(both)} rows compared")

# 4. No day zero
check("ADY never 0", (adlb["ADY"].dropna() != 0).all())

# 5. Reproduce one table cell from outputs/ with plain pandas
sub = adlb[(adlb["SAFFL"] == "Y") & (adlb["PARAMCD"] == "ALT") &
           (adlb["AVISIT"] == "WEEK 8") & (adlb["TRT01P"] == "Xanomeline High Dose")]
mean_chg = round(sub["CHG"].dropna().mean(), 1)
t2 = pd.read_csv(ROOT / "outputs/t2_alt_change_by_visit.csv")
cell = t2[(t2["group1_level"] == "WEEK 8") & (t2["row_name"] == "Mean CHG")]["Xanomeline High Dose"]
r_value = float(cell.iloc[0])
check("Table 2 cell WEEK 8 / High Dose / Mean CHG reproduced by pandas",
      abs(mean_chg - r_value) < 1e-9, f"pandas {mean_chg} vs rtables {r_value} (n = {sub['CHG'].notna().sum()})")

if failures:
    print(f"\n{len(failures)} check(s) failed: {failures}")
    sys.exit(1)
print("\nAll pandas checks passed.")
