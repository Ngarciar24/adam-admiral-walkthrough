#!/usr/bin/env bash
# Assemble the GitHub Pages site in _site/ from files the pipeline already
# produced: the rendered Quarto report, define.xml/define.html, the docs/
# guides, the QC outputs and the HTA part. Run from the repository root after
# `quarto render reports/adascog_ancova.qmd` and `quarto render hta`.
set -euo pipefail

out=_site
rm -rf "$out"
mkdir -p "$out/data/adam" "$out/docs" "$out/outputs"

# Report (self-contained HTML: embed-resources is set in the .qmd).
cp reports/adascog_ancova.html "$out/report.html"

# HTA part: a Quarto website of its own, rendered to hta/_site/.
cp -r hta/_site "$out/hta"

# define.xml and its rendered view keep their relative link to each other.
cp data/adam/define.xml data/adam/define.html "$out/data/adam/"

# Tables, figures and QC reports linked from the pages.
cp -r outputs/. "$out/outputs/"

# Markdown guides -> standalone HTML; links between them point to .html.
tmp=$(mktemp -d)
for f in docs/adrg.md docs/sap.md docs/implementation-notes.md; do
  name=$(basename "$f" .md)
  cp "$f" "$tmp/$name.md"
  quarto render "$tmp/$name.md" --to html -M embed-resources:true -M toc:true --quiet
  sed -E 's#href="([^":#]+)\.md(\#[^"]*)?"#href="\1.html\2"#g' "$tmp/$name.html" > "$out/docs/$name.html"
done

# Landing page, with the current comparison summary filled in.
summary=$(grep -m1 '^Items compared:' outputs/qc/pilot_comparison.txt)
sed "s|COMPARISON_SUMMARY|${summary}|" site/index.md > "$tmp/index.md"
mkdir -p "$tmp/outputs" && cp outputs/f2_ttde_km.png "$tmp/outputs/"
quarto render "$tmp/index.md" --to html -M embed-resources:true --quiet
cp "$tmp/index.html" "$out/index.html"
rm -rf "$tmp"

touch "$out/.nojekyll"
echo "Site assembled in $out/:"
find "$out" -maxdepth 2 -type f | sort
