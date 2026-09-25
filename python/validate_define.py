"""
Program    : validate_define.py
Study      : CDISCPILOT01 (public CDISC pilot test data)
Purpose    : Validate data/adam/define.xml against the Define-XML 2.1 schema
             and check it against the transport files: every dataset and
             variable in define.xml exists in the .xpt files with the same
             label, type and length, and vice versa
Inputs     : data/adam/define.xml, data/adam/*.xpt
Outputs    : Console report; exit status 1 on any failure
Usage      : python3 python/validate_define.py   (needs lxml, pandas and
             odmlib==0.2.1, which ships the CDISC Define-XML 2.1 XSD)
Author     : Ignacio G. Ribelles
Created    : 2026-09-25
Change log : 2026-09-25  IGR  Initial version
"""
from pathlib import Path
import sys

import odmlib
import pandas as pd
from lxml import etree

ROOT = Path(__file__).resolve().parents[1]
DEFINE = ROOT / "data/adam/define.xml"
XSD = Path(odmlib.__file__).parent / "schemas/define/2.1/define2-1-0.xsd"
NS = {"odm": "http://www.cdisc.org/ns/odm/v1.3", "def": "http://www.cdisc.org/ns/def/v2.1"}

failures = []
def check(name, ok, detail=""):
    print(f"{'OK  ' if ok else 'FAIL'} {name}{(' -- ' + detail) if detail else ''}")
    if not ok:
        failures.append(name)

# 1. Schema
schema = etree.XMLSchema(etree.parse(str(XSD)))
doc = etree.parse(str(DEFINE))
valid = schema.validate(doc)
check("define.xml is valid against the Define-XML 2.1 schema", valid,
      "; ".join(f"line {e.line}: {e.message}" for e in list(schema.error_log)[:5]))

# 2. Internal references resolve
oids = set(doc.xpath("//@OID"))
refs = {
    "ItemOID": doc.xpath("//odm:ItemRef/@ItemOID", namespaces=NS),
    "MethodOID": doc.xpath("//odm:ItemRef/@MethodOID", namespaces=NS),
    "CodeListOID": doc.xpath("//odm:CodeListRef/@CodeListOID", namespaces=NS),
    "ValueListOID": doc.xpath("//def:ValueListRef/@ValueListOID", namespaces=NS),
    "WhereClauseOID": doc.xpath("//def:WhereClauseRef/@WhereClauseOID", namespaces=NS),
    "CommentOID": doc.xpath("//@def:CommentOID", namespaces=NS),
}
for kind, values in refs.items():
    missing = sorted(set(values) - oids)
    check(f"every {kind} reference resolves", not missing, ", ".join(missing[:5]))

# Dataset-level elements the Define-XML 2.1 specification requires for ADaM
# but the schema leaves optional.
for ig in doc.xpath("//odm:ItemGroupDef", namespaces=NS):
    missing = [what for what, ok in [
        ("def:Class", ig.xpath("def:Class/@Name", namespaces=NS)),
        ("def:leaf", ig.xpath("def:leaf/@xlink:href", namespaces={**NS, "xlink": "http://www.w3.org/1999/xlink"})),
        ("def:StandardOID", ig.get("{http://www.cdisc.org/ns/def/v2.1}StandardOID")),
        ("def:Structure", ig.get("{http://www.cdisc.org/ns/def/v2.1}Structure")),
        ("KeySequence", ig.xpath("odm:ItemRef/@KeySequence", namespaces=NS)),
    ] if not ok]
    check(f"{ig.get('Name')}: class, location, standard, structure and keys given", not missing,
          ", ".join(missing))

# Every Derived ItemDef is referenced with a MethodOID.
derived = {i.get("OID") for i in doc.xpath("//odm:ItemDef[def:Origin/@Type='Derived']", namespaces=NS)}
with_method = set(doc.xpath("//odm:ItemRef[@MethodOID]/@ItemOID", namespaces=NS))
vlm_items = set(doc.xpath("//def:ValueListDef/odm:ItemRef/@ItemOID", namespaces=NS))
check("every derived variable has a method", not (derived - with_method - vlm_items),
      ", ".join(sorted(derived - with_method - vlm_items)[:5]))

# 3. define.xml agrees with the transport files
items = {i.get("OID"): i for i in doc.xpath("//odm:ItemDef", namespaces=NS)}
for ig in doc.xpath("//odm:ItemGroupDef", namespaces=NS):
    name = ig.get("Name")
    xpt = ROOT / "data/adam" / f"{name.lower()}.xpt"
    with pd.read_sas(xpt, format="xport", encoding="latin-1", iterator=True) as reader:
        fields = {f["name"].decode() if isinstance(f["name"], bytes) else f["name"]: f
                  for f in reader.fields}
    def_vars = []
    problems = []
    for ref in ig.xpath("odm:ItemRef", namespaces=NS):
        it = items[ref.get("ItemOID")]
        var = it.get("Name")
        def_vars.append(var)
        f = fields.get(var)
        if f is None:
            problems.append(f"{var} not in xpt")
            continue
        label = it.xpath("odm:Description/odm:TranslatedText/text()", namespaces=NS)[0]
        xlabel = f["label"].decode("latin-1").strip() if isinstance(f["label"], bytes) else f["label"].strip()
        if label != xlabel:
            problems.append(f"{var} label")
        is_char = it.get("DataType") == "text"
        if is_char != (f["ntype"] == "char"):
            problems.append(f"{var} type")
        if is_char and int(it.get("Length")) != f["field_length"]:
            problems.append(f"{var} length {it.get('Length')} vs {f['field_length']}")
    extra = sorted(set(fields) - set(def_vars))
    check(f"{name}: define.xml matches {xpt.name} (names, labels, types, lengths)",
          not problems and not extra, ", ".join(problems[:5] + [f"{e} not in define" for e in extra[:5]]))

if failures:
    print(f"\n{len(failures)} check(s) failed")
    sys.exit(1)
print("\nAll define.xml checks passed.")
