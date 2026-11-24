---
title: "ADaM walkthrough: the CDISC pilot study in R"
subtitle: "ADSL, ADLB, ADAE, ADTTE and ADQSADAS with {admiral}, define.xml, and the primary efficacy analysis"
author: "Ignacio Garcia Ribelles"
---

ADaM datasets, define.xml and analysis outputs for the public CDISC pilot study (CDISCPILOT01), built in R with [{admiral}](https://pharmaverse.github.io/admiral/) and checked variable by variable against the published pilot ADaM datasets. Source code, tests and CI: [github.com/Ngarciar24/adam-admiral-walkthrough](https://github.com/Ngarciar24/adam-admiral-walkthrough).

## Read

- **[Primary efficacy report](report.html).** ADAS-Cog(11) change from baseline to Week 24 (LOCF), ANCOVA, read from the ADaM transport file. Model checks and sensitivity analyses: observed cases, Week 24 completers and an MMRM. The report stops if any cell differs from Table 4.
- **[From ADaM to an HTA dossier](hta/index.html).** Self-study extension: time-to-event and safety analyses as an HTA dossier asks for them, meta-analysis and indirect treatment comparison, each hand calculation checked against an R package.
- **[define.xml, rendered](data/adam/define.html).** Define-XML 2.1 for all five datasets, generated from one specification and validated against the CDISC schema ([XML](data/adam/define.xml)).
- **[Analysis data reviewer's guide](docs/adrg.html)**, following the PHUSE ADRG template.
- **[Analysis conventions (SAP extract)](docs/sap.html)**: populations, treatment, dates, baseline, analysis windows, adverse events.
- **[Implementation notes](docs/implementation-notes.html)**: technical decisions and checked facts.
- **[Comparison with the published pilot](outputs/qc/pilot_comparison.txt)**: COMPARISON_SUMMARY.

## What it checks

- 329 `testthat` expectations: 225 on the derivations, including unit tests of the baseline rule on hand-built data, and 104 on the hand calculations of the HTA part.
- 71 ADaM conformance checks.
- An independent Python re-derivation of the treatment-emergent flag and the time-to-event data.
- GitHub Actions rebuilds every dataset, transport file, table and this site on every push, in a pinned `{renv}` environment.

## Traceability

One value, followed from SDTM to a table cell:

```
LB    01-701-1028  LBSEQ 135  ALT  WEEK 8  LBSTRESN 33
ADLB  01-701-1028  ASEQ 5     ALT  WEEK 8  AVAL 33  BASE 26  CHG 7
      BASE comes from the same subject's baseline row (ASEQ 1, ABLFL = Y, AVAL 26)
Table 2 / WEEK 8 / Mean CHG / Xanomeline High Dose
```

## Time to first dermatologic adverse event

![Kaplan–Meier plot of time to first treatment-emergent dermatologic adverse event](outputs/f2_ttde_km.png)

*Portfolio project on public test data, not a regulatory submission.*
