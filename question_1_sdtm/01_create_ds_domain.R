# =============================================================================
# Question 1: SDTM DS (Disposition) Domain Creation using {sdtm.oak}
# =============================================================================
# Purpose  : Create the SDTM DS domain from raw clinical trial data
# Input    : pharmaverseraw::ds_raw, study_ct.csv
# Output   : DS domain dataset with variables:
#            STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
#            DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
# Reference: SDTM IG v3.4 DS Domain; pharmaverse AE example (similar pattern)
# =============================================================================

# ---- 0. Load Required Packages ----------------------------------------------
library(sdtm.oak)         # SDTM data transformation engine
library(pharmaverseraw)   # Raw test data (ds_raw)
library(pharmaversesdtm)  # SDTM test data (dm, for RFSTDTC → DSSTDY)
library(dplyr)            # Data manipulation
library(lubridate)        # Date handling

# ---- 1. Load Input Data -----------------------------------------------------

# Raw disposition eCRF data
# NOTE: pharmaverseraw::ds_raw already contains the oak ID variables
# (oak_id, raw_source, patient_number) — generate_oak_id_vars() is NOT needed.
# Confirmed column names from names(ds_raw):
#   STUDY, PATNUM, SITENM, INSTANCE, FORM, FORML,
#   IT.DSTERM, IT.DSDECOD, OTHERSP,
#   DSDTCOL, DSTMCOL, IT.DSSTDAT, DEATHDT
ds_raw <- pharmaverseraw::ds_raw

# DM domain — needed to derive VISITNUM, VISIT, and DSSTDY via RFSTDTC
dm <- pharmaversesdtm::dm

# Study controlled terminology
# C66727 = Disposition From Study Reason (→ DSDECOD)
# C74558 = Disposition Category          (→ DSCAT)
study_ct <- read.csv("study_ct.csv", stringsAsFactors = FALSE)

# Quick exploration
cat("=== ds_raw: Column Names ===\n")
print(names(ds_raw))
cat("\n=== ds_raw: First 3 Rows ===\n")
print(head(ds_raw, 3))

# ---- 2. Pre-process: Combine Date + Time into a single datetime column ------
# DSDTCOL = collection date (YYYY-MM-DD), DSTMCOL = collection time (HH:MM)
# assign_datetime() expects a single column, so we combine them first.
# Where time is missing, we keep just the date for ISO 8601 compliance.
ds_raw <- ds_raw |>
  mutate(
    DSDTC_RAW = case_when(
      !is.na(DSDTCOL) & !is.na(DSTMCOL) & DSTMCOL != "" ~
        paste0(DSDTCOL, "T", DSTMCOL),
      !is.na(DSDTCOL) ~ as.character(DSDTCOL),
      TRUE ~ NA_character_
    )
  )

# ---- 3. Derive DS Domain using {sdtm.oak} -----------------------------------
# Each assign_*/hardcode_* call maps one raw column → one SDTM variable.
# The pipe passes the growing dataset through tgt_dat on each step.
# oak_id_vars() = c("oak_id", "raw_source", "patient_number") — already in ds_raw.

ds <- assign_no_ct(
  tgt_dat = NULL,          # NULL on first call — starts a fresh target dataset
  tgt_var  = "STUDYID",
  raw_dat  = ds_raw,
  raw_var  = "STUDY",       # Raw column: STUDY → SDTM: STUDYID (confirmed from names(ds_raw))
  id_vars  = oak_id_vars()
) |>

  # DOMAIN: hardcoded to "DS" for all records
  hardcode_no_ct(
    tgt_var      = "DOMAIN",
    raw_dat      = ds_raw,
    hardcode_val = "DS"     # correct param name (not target_hard_coded_value)
  ) |>

  # USUBJID: unique subject identifier
  # PATNUM in this dataset is the full unique subject ID (e.g., "01-701-1015")
  assign_no_ct(
    tgt_var = "USUBJID",
    raw_dat = ds_raw,
    raw_var = "PATNUM"
  ) |>

  # DSTERM: verbatim reported disposition term — no CT lookup
  # IT.DSTERM is the raw text collected on the eCRF (e.g., "Complete")
  assign_no_ct(
    tgt_var = "DSTERM",
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM"
  ) |>

  # DSDECOD: standardized term via C66727 controlled terminology
  # Maps collected values (e.g., "Complete") → CDISC term (e.g., "COMPLETED")
  assign_ct(
    tgt_var  = "DSDECOD",
    raw_dat  = ds_raw,
    raw_var  = "IT.DSDECOD",  # Raw collected value before CT standardisation
    ct_spec  = study_ct,
    ct_clst  = "C66727"        # Codelist: Disposition From Study Reason
  ) |>

  # DSCAT: disposition category via C74558 CT
  # C74558 values: PROTOCOL MILESTONE, OTHER EVENT, DISPOSITION EVENT
  # FORM identifies which eCRF form was used, which maps to the category
  assign_ct(
    tgt_var  = "DSCAT",
    raw_dat  = ds_raw,
    raw_var  = "FORM",        # Form name indicates category context
    ct_spec  = study_ct,
    ct_clst  = "C74558"       # Codelist: Disposition Category
  ) |>

  # DSDTC: collection date-time in ISO 8601 format
  # Combined DSDTCOL + DSTMCOL into DSDTC_RAW in Step 2
  assign_datetime(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = "DSDTC_RAW",
    raw_fmt = "y-m-dTH:M"    # ISO 8601 with time; date-only rows still parse
  ) |>

  # DSSTDTC: start date of disposition event in ISO 8601 format
  assign_datetime(
    tgt_var = "DSSTDTC",
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    raw_fmt = "y-m-d"
  )

# ---- 4. Derive DSSEQ --------------------------------------------------------
# Sequence number: unique integer per subject, ordered chronologically
ds <- derive_seq(
  sdtm_in  = ds,
  tgt_var  = "DSSEQ",
  rec_vars = c("USUBJID", "DSSTDTC")
)

# ---- 5. Add VISITNUM and VISIT ----------------------------------------------
# VISITNUM and VISIT are not collected on the disposition eCRF (not in ds_raw).
# We join from the DM domain using the closest reference, or leave as NA.
# Per SDTM IG, these are "expected" but may be blank when not collected.
# If DM has visit information tied to subjects, we bring it in here.
# For now, set to NA — update this logic if a visit lookup source is available.
ds <- ds |>
  mutate(
    VISITNUM = NA_real_,
    VISIT    = NA_character_
  )

# ---- 6. Derive DSSTDY -------------------------------------------------------
# DSSTDY: study day of disposition relative to first treatment date (RFSTDTC)
# Rule: if DSSTDTC >= RFSTDTC → day = diff + 1 (no day 0)
#       if DSSTDTC <  RFSTDTC → day = diff      (negative days)

rfstdtc_lookup <- dm |>
  select(USUBJID, RFSTDTC) |>
  filter(!is.na(RFSTDTC))

ds <- ds |>
  left_join(rfstdtc_lookup, by = "USUBJID") |>
  mutate(
    DSSTDY = case_when(
      !is.na(DSSTDTC) & !is.na(RFSTDTC) ~ {
        diff <- as.integer(as.Date(DSSTDTC) - as.Date(RFSTDTC))
        if_else(diff >= 0L, diff + 1L, diff)
      },
      TRUE ~ NA_integer_
    )
  ) |>
  select(-RFSTDTC)

# ---- 7. Select and Order Final DS Variables ---------------------------------
ds <- ds |>
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  ) |>
  arrange(USUBJID, DSSEQ)

# ---- 8. QC Checks -----------------------------------------------------------
cat("\n=== DS Domain: Final Structure ===\n")
str(ds)

cat("\n=== DS Domain: First 10 Rows ===\n")
print(head(ds, 10))

cat("\n=== DS Domain: Row Count & Unique Subjects ===\n")
cat("Total records:", nrow(ds), "\n")
cat("Unique subjects:", n_distinct(ds$USUBJID), "\n")

cat("\n=== DSDECOD Value Counts ===\n")
print(table(ds$DSDECOD, useNA = "always"))

cat("\n=== DSCAT Value Counts ===\n")
print(table(ds$DSCAT, useNA = "always"))

cat("\n=== Missing Values by Variable ===\n")
required_vars <- c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ",
                   "DSTERM", "DSDECOD", "DSCAT",
                   "DSDTC", "DSSTDTC", "DSSTDY")
print(sapply(required_vars, function(v) sum(is.na(ds[[v]]))))

# ---- 9. Save Output ---------------------------------------------------------
saveRDS(ds, file = "ds_domain.rds")
write.csv(ds, file = "ds_domain.csv", row.names = FALSE, na = "")
cat("\n=== Saved: ds_domain.rds and ds_domain.csv ===\n")
cat("Program completed successfully.\n")

# ---- 10. Session Info (for reproducibility log) -----------------------------
cat("\n=== Session Info ===\n")
print(sessionInfo())
