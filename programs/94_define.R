# -----------------------------------------------------------------------------
# Program    : 94_define.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Generate define.xml (Define-XML 2.1) and a readable define.html
#              for the ADaM datasets from the specification metadata
# Inputs     : metadata/adam_datasets.csv, metadata/adam_spec.csv,
#              metadata/codelists.csv, metadata/valuelevel.csv
# Outputs    : data/adam/define.xml, data/adam/define.html
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : The same metadata drives the XPT attributes (90_export_xpt.R),
#              so define.xml and the transport files cannot disagree. Schema
#              validation: python/validate_define.py (Define-XML 2.1 XSD).
# -----------------------------------------------------------------------------

library(dplyr)
library(readr)
library(xml2)

out_dir <- "data/adam"

ds    <- read_csv("metadata/adam_datasets.csv", col_types = cols(.default = "c"))
spec  <- read_csv("metadata/adam_spec.csv", col_types = cols(.default = "c"), na = "") %>%
  mutate(order = as.integer(order))
cl    <- read_csv("metadata/codelists.csv", col_types = cols(.default = "c"), na = "")
vlm   <- read_csv("metadata/valuelevel.csv", col_types = cols(.default = "c"), na = "")

study      <- "CDISCPILOT01"
adamig_oid <- "STD.ADAMIG.1.3"
ns <- c(
  "xmlns"       = "http://www.cdisc.org/ns/odm/v1.3",
  "xmlns:xlink" = "http://www.w3.org/1999/xlink",
  "xmlns:def"   = "http://www.cdisc.org/ns/def/v2.1"
)

# Description element with one English TranslatedText.
add_desc <- function(node, text, tag = "Description") {
  d <- xml_add_child(node, tag)
  xml_add_child(d, "TranslatedText", text, "xml:lang" = "en")
  invisible(node)
}

key_seq <- function(dataset, variable) {
  keys <- trimws(strsplit(ds$keys[ds$dataset == dataset], ",")[[1]])
  k <- match(variable, keys)
  if (is.na(k)) NULL else k
}

# =============================================================================
# 1. Document root and study
# =============================================================================
doc  <- xml_new_root("ODM", .version = "1.0", .encoding = "UTF-8")
root <- xml_root(doc)
xml_attrs(root) <- c(
  ns,
  ODMVersion        = "1.3.2",
  FileType          = "Snapshot",
  FileOID           = paste0("DEF.", study, ".ADAM"),
  CreationDateTime  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  Originator        = "cdisc-pilot-adam-admiral",
  SourceSystem      = "R",
  SourceSystemVersion = paste(R.version$major, R.version$minor, sep = "."),
  "def:Context"     = "Submission"
)

study_node <- xml_add_child(root, "Study", OID = paste0("STDY.", study))
gv <- xml_add_child(study_node, "GlobalVariables")
xml_add_child(gv, "StudyName", study)
xml_add_child(gv, "StudyDescription", "CDISC pilot study: xanomeline in mild to moderate Alzheimer's disease")
xml_add_child(gv, "ProtocolName", study)

mdv <- xml_add_child(
  study_node, "MetaDataVersion",
  OID                 = paste0("MDV.", study, ".ADAM"),
  Name                = paste(study, "ADaM Data Definitions"),
  Description         = paste(study, "ADaM Data Definitions"),
  "def:DefineVersion" = "2.1.0"
)

stds <- xml_add_child(mdv, "def:Standards")
xml_add_child(stds, "def:Standard", OID = adamig_oid, Name = "ADaMIG", Type = "IG",
              Version = "1.3", Status = "Final")

# =============================================================================
# 2. Value-level metadata and where clauses (ADLB AVAL by PARAMCD)
# =============================================================================
for (vl_key in unique(paste(vlm$dataset, vlm$variable, sep = "."))) {
  v <- vlm[paste(vlm$dataset, vlm$variable, sep = ".") == vl_key, ]
  vl <- xml_add_child(mdv, "def:ValueListDef", OID = paste0("VL.", vl_key))
  for (i in seq_len(nrow(v))) {
    ir <- xml_add_child(vl, "ItemRef", ItemOID = paste0("IT.", vl_key, ".", v$paramcd[i]),
                        OrderNumber = i, Mandatory = "No")
    xml_add_child(ir, "def:WhereClauseRef",
                  WhereClauseOID = paste0("WC.", v$dataset[i], ".PARAMCD.", v$paramcd[i]))
  }
}
for (i in seq_len(nrow(vlm))) {
  wc <- xml_add_child(mdv, "def:WhereClauseDef",
                      OID = paste0("WC.", vlm$dataset[i], ".PARAMCD.", vlm$paramcd[i]))
  rc <- xml_add_child(wc, "RangeCheck", SoftHard = "Soft", Comparator = "EQ",
                      "def:ItemOID" = paste0("IT.", vlm$dataset[i], ".PARAMCD"))
  xml_add_child(rc, "CheckValue", vlm$paramcd[i])
}

# =============================================================================
# 3. Datasets (ItemGroupDef)
# =============================================================================
for (i in seq_len(nrow(ds))) {
  d  <- ds$dataset[i]
  vs <- spec %>% filter(dataset == d) %>% arrange(order)
  ig <- xml_add_child(
    mdv, "ItemGroupDef",
    OID                    = paste0("IG.", d),
    Name                   = d,
    SASDatasetName         = d,
    Repeating              = if (d == "ADSL") "No" else "Yes",
    IsReferenceData        = "No",
    Purpose                = "Analysis",
    "def:Structure"        = ds$structure[i],
    "def:StandardOID"      = adamig_oid,
    "def:ArchiveLocationID" = paste0("LF.", d)
  )
  add_desc(ig, ds$label[i])
  for (j in seq_len(nrow(vs))) {
    attrs <- list(ItemOID = paste0("IT.", d, ".", vs$variable[j]), OrderNumber = vs$order[j],
                  Mandatory = if (!is.null(key_seq(d, vs$variable[j]))) "Yes" else "No")
    ks <- key_seq(d, vs$variable[j])
    if (!is.null(ks)) attrs$KeySequence <- ks
    if (vs$origin[j] == "Derived") attrs$MethodOID <- paste0("MT.", d, ".", vs$variable[j])
    do.call(xml_add_child, c(list(ig, "ItemRef"), attrs))
  }
  xml_add_child(ig, "def:Class", Name = ds$class[i])
  lf <- xml_add_child(ig, "def:leaf", ID = paste0("LF.", d),
                      "xlink:href" = paste0(tolower(d), ".xpt"))
  xml_add_child(lf, "def:title", paste0(tolower(d), ".xpt"))
}

# =============================================================================
# 4. Variables (ItemDef), including value-level items
# =============================================================================
add_item <- function(oid, name, datatype, length, label, format, codelist, origin, source,
                     comment_oid = NA, valuelist_oid = NA) {
  attrs <- list(OID = oid, Name = name, SASFieldName = name, DataType = datatype)
  if (datatype %in% c("text", "integer", "float")) attrs$Length <- length
  if (!is.na(format) && nzchar(format)) attrs[["def:DisplayFormat"]] <- format
  if (!is.na(comment_oid)) attrs[["def:CommentOID"]] <- comment_oid
  it <- do.call(xml_add_child, c(list(mdv, "ItemDef"), attrs))
  add_desc(it, label)
  if (!is.na(codelist) && nzchar(codelist)) xml_add_child(it, "CodeListRef", CodeListOID = codelist)
  og <- xml_add_child(it, "def:Origin", Type = origin)
  if (origin == "Predecessor") add_desc(og, source)
  if (!is.na(valuelist_oid)) xml_add_child(it, "def:ValueListRef", ValueListOID = valuelist_oid)
}

vl_keys <- unique(paste(vlm$dataset, vlm$variable, sep = "."))
for (j in seq_len(nrow(spec))) {
  key <- paste(spec$dataset[j], spec$variable[j], sep = ".")
  add_item(
    oid      = paste0("IT.", key), name = spec$variable[j], datatype = spec$datatype[j],
    length   = spec$length[j], label = spec$label[j], format = spec$format[j],
    codelist = spec$codelist[j], origin = spec$origin[j], source = spec$source[j],
    comment_oid   = if (spec$origin[j] == "Assigned") paste0("COM.", key) else NA,
    valuelist_oid = if (key %in% vl_keys) paste0("VL.", key) else NA
  )
}
for (i in seq_len(nrow(vlm))) {
  add_item(
    oid = paste0("IT.", vlm$dataset[i], ".", vlm$variable[i], ".", vlm$paramcd[i]),
    name = vlm$variable[i], datatype = vlm$datatype[i], length = "8",
    label = vlm$description[i], format = NA, codelist = NA,
    origin = vlm$origin[i], source = vlm$source[i]
  )
}

# =============================================================================
# 5. Codelists, methods, comments
# =============================================================================
for (oid in unique(cl$codelist)) {
  c1 <- cl %>% filter(codelist == oid) %>% arrange(as.integer(order))
  cn <- xml_add_child(mdv, "CodeList", OID = oid, Name = c1$name[1], DataType = c1$datatype[1])
  decoded <- any(!is.na(c1$decode) & nzchar(c1$decode))
  for (k in seq_len(nrow(c1))) {
    if (decoded) {
      item <- xml_add_child(cn, "CodeListItem", CodedValue = c1$value[k], OrderNumber = k)
      add_desc(item, c1$decode[k], tag = "Decode")
    } else {
      xml_add_child(cn, "EnumeratedItem", CodedValue = c1$value[k], OrderNumber = k)
    }
  }
}

derived <- spec %>% filter(origin == "Derived")
for (j in seq_len(nrow(derived))) {
  key <- paste(derived$dataset[j], derived$variable[j], sep = ".")
  m <- xml_add_child(mdv, "MethodDef", OID = paste0("MT.", key),
                     Name = paste("Algorithm for", key), Type = "Computation")
  add_desc(m, derived$method[j])
}

assigned <- spec %>% filter(origin == "Assigned")
for (j in seq_len(nrow(assigned))) {
  key <- paste(assigned$dataset[j], assigned$variable[j], sep = ".")
  cm <- xml_add_child(mdv, "def:CommentDef", OID = paste0("COM.", key))
  add_desc(cm, assigned$method[j])
}

write_xml(doc, file.path(out_dir, "define.xml"))

# =============================================================================
# 6. define.html (readable view of the same metadata)
# =============================================================================
esc <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
row_html <- function(cells, tag = "td") {
  paste0("<tr>", paste0("<", tag, ">", cells, "</", tag, ">", collapse = ""), "</tr>")
}

html <- c(
  "<!DOCTYPE html>", "<html lang=\"en\"><head><meta charset=\"utf-8\">",
  paste0("<title>", study, " ADaM define</title>"),
  "<style>body{font-family:system-ui,sans-serif;margin:2rem;max-width:1200px;color:#1f2328}",
  "table{border-collapse:collapse;width:100%;margin:0 0 2rem;font-size:.85rem}",
  "th,td{border:1px solid #d0d7de;padding:4px 6px;text-align:left;vertical-align:top}",
  "th{background:#f6f8fa}h2{margin-top:2.5rem}code{font-size:.85rem}</style></head><body>",
  paste0("<h1>", study, ": ADaM data definitions</h1>"),
  "<p>Define-XML 2.1 (<a href=\"define.xml\">define.xml</a>), ADaMIG 1.3. Generated by programs/94_define.R from metadata/.</p>",
  "<h2>Datasets</h2><table>",
  row_html(c("Dataset", "Label", "Class", "Structure", "Keys", "Location"), "th"),
  vapply(seq_len(nrow(ds)), function(i) row_html(c(
    paste0("<a href=\"#", ds$dataset[i], "\">", ds$dataset[i], "</a>"), esc(ds$label[i]),
    esc(ds$class[i]), esc(ds$structure[i]), esc(ds$keys[i]),
    paste0("<a href=\"", tolower(ds$dataset[i]), ".xpt\">", tolower(ds$dataset[i]), ".xpt</a>")
  )), ""),
  "</table>"
)
for (i in seq_len(nrow(ds))) {
  vs <- spec %>% filter(dataset == ds$dataset[i]) %>% arrange(order)
  html <- c(html,
    paste0("<h2 id=\"", ds$dataset[i], "\">", ds$dataset[i], ": ", esc(ds$label[i]), "</h2><table>"),
    row_html(c("Variable", "Label", "Type", "Length", "Format", "Codelist", "Origin", "Source / derivation"), "th"),
    vapply(seq_len(nrow(vs)), function(j) row_html(c(
      paste0("<code>", vs$variable[j], "</code>"), esc(vs$label[j]), vs$datatype[j], vs$length[j],
      esc(vs$format[j]),
      if (is.na(vs$codelist[j])) "" else paste0("<a href=\"#", vs$codelist[j], "\">", vs$codelist[j], "</a>"),
      vs$origin[j],
      esc(if (vs$origin[j] == "Predecessor") vs$source[j] else vs$method[j])
    )), ""),
    "</table>"
  )
}
html <- c(html, "<h2>Value-level metadata</h2><table>",
  row_html(c("Dataset", "Variable", "Where", "Type", "Origin", "Description"), "th"),
  vapply(seq_len(nrow(vlm)), function(i) row_html(c(
    vlm$dataset[i], vlm$variable[i], paste0("PARAMCD = '", vlm$paramcd[i], "'"),
    vlm$datatype[i], vlm$origin[i], esc(vlm$description[i])
  )), ""),
  "</table><h2>Codelists</h2>")
for (oid in unique(cl$codelist)) {
  c1 <- cl %>% filter(codelist == oid) %>% arrange(as.integer(order))
  html <- c(html,
    paste0("<h3 id=\"", oid, "\">", oid, ": ", esc(c1$name[1]), " (", c1$datatype[1], ")</h3><table>"),
    row_html(c("Value", "Decode"), "th"),
    vapply(seq_len(nrow(c1)), function(k) row_html(c(esc(c1$value[k]), esc(c1$decode[k]))), ""),
    "</table>")
}
html <- c(html, "</body></html>")
writeLines(html, file.path(out_dir, "define.html"))

message("Wrote data/adam/define.xml (", nrow(ds), " datasets, ", nrow(spec), " variables, ",
        length(unique(cl$codelist)), " codelists, ", nrow(derived), " methods) and data/adam/define.html")
