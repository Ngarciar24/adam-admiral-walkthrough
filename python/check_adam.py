"""
Independent cross-language check of the ADaM exports, reading the SAS
Transport files with pandas.

Why this exists: work in this area is migrating from SAS to R and Python. The
.xpt files are language-neutral, so a second language can re-verify the R
derivations by a different door -- pandas here, nothing from the R side is
imported.

Two kinds of check: structural invariants of ADLB, and an independent
re-derivation of the ADAE treatment-emergent flag from the dates carried in the
file. The second is the double-programming idea: the rule is written again in
another language, from its SAP wording, and the two implementations must agree
row for row.

Run from the project root:  python3 python/check_adam.py
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

# 6. ADAE: re-derive TRTEMFL from the dates in the file and compare
#    Rule (README "Choices made here", programs/03_adae.R section 6): an event
#    is treatment-emergent when its onset is on or after first dose and no more
#    than 30 days after last dose. admiral returns "Y" or missing, never "N";
#    an event that ENDED before first dose is not emergent even if its onset is
#    unknown, and an event with unknown onset that did not end before first dose
#    is counted. Both branches are written out here so that a reader can compare
#    this function with the admiral call directly.
#    pd.read_sas() leaves XPT dates as plain SAS day counts (days since
#    1960-01-01), so the 30-day window is an addition of 30, not a Timedelta.
adae = pd.read_sas(ROOT / "data/adam/adae.xpt", format="xport", encoding="latin-1")

def trtemfl(row):
    st, en, ts, te = row["ASTDT"], row["AENDT"], row["TRTSDT"], row["TRTEDT"]
    if pd.isna(ts):
        return None
    if pd.notna(en) and en < ts:
        return None
    if pd.isna(st):
        return "Y"
    if st < ts:
        return None
    if pd.notna(te) and st > te + 30:
        return None
    return "Y"

py_flag = adae.apply(trtemfl, axis=1)
r_flag = adae["TRTEMFL"].replace("", None)
agree = (py_flag.fillna("NA") == r_flag.fillna("NA"))
check("ADAE TRTEMFL re-derived in pandas matches admiral on every row", agree.all(),
      f"{agree.sum()} of {len(adae)} rows agree; {int((r_flag == 'Y').sum())} flagged Y")

# 7. ADAE: occurrence flags count subjects, not events
n_subj_te = adae.loc[adae["TRTEMFL"] == "Y", "USUBJID"].nunique()
check("AOCCFL == 'Y' once per subject with a treatment-emergent AE",
      int((adae["AOCCFL"] == "Y").sum()) == n_subj_te,
      f"{n_subj_te} subjects")
check("AOCCFL rows are treatment-emergent",
      (adae.loc[adae["AOCCFL"] == "Y", "TRTEMFL"] == "Y").all())

if failures:
    print(f"\n{len(failures)} check(s) failed: {failures}")
    sys.exit(1)
print("\nAll pandas checks passed.")
