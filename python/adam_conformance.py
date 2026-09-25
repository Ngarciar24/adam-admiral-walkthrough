"""
Program    : adam_conformance.py
Study      : CDISCPILOT01 (public CDISC pilot test data)
Purpose    : ADaM conformance checks on the transport files. CDISC CORE has no
             ADaM rules yet, so the checks below implement ADaMIG requirements
             explicitly: identifiers, flag values, study days, ADSL variables
             copied into other datasets, BDS parameter/baseline/change
             consistency, ADTTE and OCCDS date logic
Inputs     : data/adam/*.xpt
Outputs    : outputs/qc/adam_conformance.txt; exit status 1 on any failure
Usage      : python3 python/adam_conformance.py   (needs pandas)
Author     : Ignacio G. Ribelles
Created    : 2026-09-25
Change log : 2026-09-25  IGR  Initial version
"""
from pathlib import Path
import re
import sys

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
DATASETS = ["adsl", "adlb", "adae", "adtte", "adqsadas"]
BDS = ["adlb", "adtte", "adqsadas"]
POP_FLAGS = ["SAFFL", "ITTFL", "EFFFL", "COMP24FL"]
REC_FLAGS = re.compile(r"^(ABLFL|ANL\d\dFL|TRTEMFL|AOCC\w*FL)$")


def read(name):
    df = pd.read_sas(ROOT / f"data/adam/{name}.xpt", format="xport", encoding="latin-1")
    for c in df.columns:
        if pd.api.types.is_numeric_dtype(df[c]):
            # pandas decodes an IBM-float zero as ~5.4e-79; restore exact zeros
            # so that checks such as "never 0" are not vacuous.
            df.loc[df[c].abs() < 1e-70, c] = 0.0
        else:
            # SAS stores missing character values as blanks.
            df[c] = df[c].replace("", None)
    return df


data = {d: read(d) for d in DATASETS}
adsl = data["adsl"].set_index("USUBJID")
results = []


def check(cid, scope, text, bad):
    """bad: number of offending records (0 = pass)."""
    results.append((cid, scope, text, int(bad)))


for d, df in data.items():
    D = d.upper()
    check("GEN-01", D, "STUDYID and USUBJID present and never missing",
          0 if {"STUDYID", "USUBJID"} <= set(df) and df[["STUDYID", "USUBJID"]].notna().all().all() else 1)
    check("GEN-02", D, "variable names: at most 8 characters, uppercase letters and digits, first a letter",
          sum(not re.fullmatch(r"[A-Z][A-Z0-9]{0,7}", c) for c in df.columns))
    check("GEN-03", D, "every USUBJID exists in ADSL", (~df["USUBJID"].isin(adsl.index)).sum())
    pops = [c for c in POP_FLAGS if c in df]
    check("GEN-04", D, "population flags are Y or N, never missing",
          sum((~df[c].isin(["Y", "N"])).sum() for c in pops))
    recs = [c for c in df.columns if REC_FLAGS.match(c)]
    check("GEN-05", D, "record-level flags (ABLFL, ANLxxFL, TRTEMFL, AOCCxxFL) are Y or null",
          sum((df[c].notna() & (df[c] != "Y")).sum() for c in recs))
    dys = [c for c in df.columns if c.endswith("DY") and c not in ("VISITDY",)]
    check("GEN-06", D, "study-day variables (*DY) are never 0", sum((df[c] == 0).sum() for c in dys))
    dts = [c for c in df.columns if re.fullmatch(r"\w*DT", c)]
    check("GEN-07", D, "date variables (*DT) are numeric", sum(not pd.api.types.is_numeric_dtype(df[c]) for c in dts))
    if d != "adsl":
        shared = [c for c in df.columns if c in adsl.columns and c not in ("STUDYID",)]
        ref = adsl.loc[df["USUBJID"], shared].reset_index(drop=True)
        mine = df[shared].reset_index(drop=True)
        diff = ~((mine == ref) | (mine.isna() & ref.isna()))
        check("GEN-08", D, f"ADSL variables carried over match ADSL ({len(shared)} variables)",
              diff.any(axis=1).sum())

# ADSL
a = data["adsl"]
check("ADSL-01", "ADSL", "one record per subject", a["USUBJID"].duplicated().sum())
itt = a[a["ITTFL"] == "Y"]
check("ADSL-02", "ADSL", "TRT01P and TRT01PN populated for every ITT subject",
      (itt["TRT01P"].isna() | itt["TRT01PN"].isna()).sum())
for v in ("TRT01P", "TRT01A"):
    pairs = a[[v, v + "N"]].dropna().drop_duplicates()
    check("ADSL-03", "ADSL", f"{v} and {v}N map one to one",
          pairs[v].duplicated().sum() + pairs[v + "N"].duplicated().sum())
both = a.dropna(subset=["TRTSDT", "TRTEDT"])
check("ADSL-04", "ADSL", "TRTSDT is not after TRTEDT", (both["TRTSDT"] > both["TRTEDT"]).sum())

# BDS
for d in BDS:
    df, D = data[d], d.upper()
    check("BDS-01", D, "PARAMCD at most 8 characters, letters/digits, first a letter",
          (~df["PARAMCD"].str.fullmatch(r"[A-Z][A-Z0-9]{0,7}")).sum())
    pp = df[["PARAMCD", "PARAM"]].drop_duplicates()
    check("BDS-02", D, "PARAMCD and PARAM map one to one",
          pp["PARAMCD"].duplicated().sum() + pp["PARAM"].duplicated().sum())
    check("BDS-03", D, "every record has AVAL or AVALC",
          (df["AVAL"].isna() & (df["AVALC"].isna() if "AVALC" in df else True)).sum())
    if "ABLFL" in df:
        keys = ["USUBJID", "PARAMCD"]
        nbl = df[df["ABLFL"] == "Y"].groupby(keys).size()
        check("BDS-04", D, "at most one ABLFL = 'Y' per subject and parameter", (nbl > 1).sum())
        bl = df[df["ABLFL"] == "Y"][keys + ["AVAL"]].rename(columns={"AVAL": "BLVAL"})
        m = df.merge(bl, on=keys, how="left")
        check("BDS-05", D, "BASE equals AVAL of the baseline record",
              (~((m["BASE"] == m["BLVAL"]) | (m["BASE"].isna() & m["BLVAL"].isna()))).sum())
        c = df.dropna(subset=["CHG"])
        check("BDS-06", D, "CHG = AVAL - BASE wherever CHG is populated",
              ((c["CHG"] - (c["AVAL"] - c["BASE"])).abs() > 1e-8).sum())
        check("BDS-07", D, "CHG is not populated on the baseline record",
              df.loc[df["ABLFL"] == "Y", "CHG"].notna().sum())
    if "AVISIT" in df:
        av = df[["AVISIT", "AVISITN"]].dropna().drop_duplicates()
        check("BDS-08", D, "AVISIT and AVISITN map one to one",
              av["AVISIT"].duplicated().sum() + av["AVISITN"].duplicated().sum())
    if "DTYPE" in df:
        check("BDS-09", D, "derived records (DTYPE populated) are analysis records",
              (df["DTYPE"].notna() & (df["ANL01FL"] != "Y")).sum())

# ADTTE
t = data["adtte"]
check("TTE-01", "ADTTE", "CNSR is 0 (event) or a positive integer (censored)",
      (~t["CNSR"].isin([0, 1])).sum())
check("TTE-02", "ADTTE", "STARTDT is not after ADT", (t["STARTDT"] > t["ADT"]).sum())
check("TTE-03", "ADTTE", "AVAL is non-negative", (t["AVAL"] < 0).sum())
check("TTE-04", "ADTTE", "EVNTDESC populated on every record", t["EVNTDESC"].isna().sum())

# OCCDS
e = data["adae"]
be = e.dropna(subset=["ASTDT", "AENDT"])
check("OCC-01", "ADAE", "ASTDT is not after AENDT", (be["ASTDT"] > be["AENDT"]).sum())
check("OCC-02", "ADAE", "ASTDTF populated only where ASTDT exists",
      (e["ASTDTF"].notna() & e["ASTDT"].isna()).sum())
check("OCC-03", "ADAE", "no BDS variables (PARAMCD, AVAL, BASE, CHG) in an OCCDS dataset",
      len({"PARAMCD", "AVAL", "BASE", "CHG"} & set(e.columns)))

# Report
lines = ["ADaM CONFORMANCE CHECKS (python/adam_conformance.py)",
         "Checks follow ADaMIG requirements; CDISC CORE publishes no ADaM rules yet.", ""]
lines += [f"{'PASS' if bad == 0 else 'FAIL'}  {cid:8} {scope:9} {text}" + (f"  [{bad}]" if bad else "")
          for cid, scope, text, bad in results]
n_fail = sum(1 for r in results if r[3])
lines += ["", f"{len(results)} checks, {n_fail} failed"]
out = ROOT / "outputs/qc/adam_conformance.txt"
out.parent.mkdir(parents=True, exist_ok=True)
out.write_text("\n".join(lines) + "\n")
print("\n".join(lines))
sys.exit(1 if n_fail else 0)
