"""
Program    : check_adam.py
Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
Purpose    : Independent re-check of the ADaM transport files in Python:
             structural checks on ADLB, one Table 2 cell reproduced, the ADAE
             treatment-emergent flag and the ADTTE event/censoring times
             re-derived from the files and compared with the R results
Inputs     : data/adam/adsl.xpt, adlb.xpt, adae.xpt, adtte.xpt,
             outputs/t2_alt_change_by_visit.csv
Outputs    : Console report; exit status 1 if any check fails
Usage      : python3 python/check_adam.py   (from the project root; needs pandas)
Author     : Ignacio G. Ribelles
Created    : 2026-09-17
Change log : 2026-09-17  IGR  Initial version
             2026-09-25  IGR  Standard header
             2026-09-25  IGR  Exact zeros restored on read; ADTTE re-derived
"""
from pathlib import Path
import sys
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]


def read_xpt(name):
    df = pd.read_sas(ROOT / f"data/adam/{name}.xpt", format="xport", encoding="latin-1")
    # pandas decodes an IBM-float zero as ~5.4e-79; restore exact zeros so that
    # checks such as "ADY never 0" test something.
    num = df.select_dtypes("number").columns
    df[num] = df[num].mask(df[num].abs() < 1e-70, 0.0)
    return df


adlb = read_xpt("adlb")

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
#    Rule (docs/sap.md section 7, programs/03_adae.R section 6): an event
#    is treatment-emergent when its onset is on or after first dose and no more
#    than 30 days after last dose. admiral returns "Y" or missing, never "N";
#    an event that ENDED before first dose is not emergent even if its onset is
#    unknown, and an event with unknown onset that did not end before first dose
#    is counted. Both branches mirror derive_var_trtemfl().
#    pd.read_sas() leaves XPT dates as plain SAS day counts (days since
#    1960-01-01), so the 30-day window is an addition of 30, not a Timedelta.
adae = read_xpt("adae")

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

# 8. ADTTE: re-derive time to first dermatologic event from ADAE and ADSL
#    Event: first ADAE record with TRTEMFL = "Y" and CQ01NAM populated (by
#    ASTDT, then AESEQ); otherwise censored at RFENDT. AVAL = ADT - TRTSDT + 1.
adsl = read_xpt("adsl")
adtte = read_xpt("adtte")
saf = adsl[adsl["SAFFL"] == "Y"][["USUBJID", "TRTSDT", "RFENDT"]]
ev = (adae[(adae["TRTEMFL"] == "Y") & (adae["CQ01NAM"] != "")]
      .sort_values(["USUBJID", "ASTDT", "AESEQ"]).drop_duplicates("USUBJID")[["USUBJID", "ASTDT"]])
exp = saf.merge(ev, on="USUBJID", how="left")
exp["ADT_EXP"] = exp["ASTDT"].fillna(exp["RFENDT"])
exp["CNSR_EXP"] = exp["ASTDT"].isna().astype(int)
exp["AVAL_EXP"] = exp["ADT_EXP"] - exp["TRTSDT"] + 1
m = adtte.merge(exp, on="USUBJID", how="outer", indicator=True)
check("ADTTE has one record per safety subject", (m["_merge"] == "both").all(), f"{len(adtte)} records")
check("ADTTE ADT, CNSR and AVAL re-derived in pandas match R on every subject",
      ((m["ADT"] == m["ADT_EXP"]) & (m["CNSR"] == m["CNSR_EXP"]) & (m["AVAL"] == m["AVAL_EXP"])).all(),
      f"{int((m['CNSR'] == 0).sum())} events, {int((m['CNSR'] == 1).sum())} censored")

if failures:
    print(f"\n{len(failures)} check(s) failed: {failures}")
    sys.exit(1)
print("\nAll pandas checks passed.")
