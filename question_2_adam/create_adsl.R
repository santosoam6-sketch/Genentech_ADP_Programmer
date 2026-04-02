# =============================================================================
# Q2: ADaM ADSL Dataset Creation
# =============================================================================
# Purpose  : Create an ADSL (Subject Level) dataset using SDTM source data, 
#           the {admiral} family of packages, and tidyverse tools.
# Input    : pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex,
#            pharmaversesdtm::ds, pharmaversesdtm::ae
# Output   : ADSL Dataset with variables:
#            AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF, ITTFL, and LSTAVLDT
# =============================================================================

# ---- 00 Load Packages --------------------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

# ---- 01 Load Data ------------------------------------------------------------
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# Start ADSL from DM
adsl <- dm 

# ---- 02 Derive AGEGR9 / AGEGR9N ----------------------------------------------
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      !is.na(AGE) & AGE < 18 ~ "<18",
      !is.na(AGE) & AGE >= 18 & AGE <= 50 ~ "18 - 50",
      !is.na(AGE) & AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      !is.na(AGE) & AGE < 18 ~ 1,
      !is.na(AGE) & AGE >= 18 & AGE <= 50 ~ 2,
      !is.na(AGE) & AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  )
# ---- 02 Derive TRTSDTM / TRTSTMF  --------------------------------------------
################################################################################
# TRTSDTM/TRTSTMF are derived from the first valid EX record per subject.
# A valid dose is defined as EXDOSE > 0, or EXDOSE = 0 when EXTRT contains "PLACEBO".
# Missing time components are imputed to 00:00:00.
# Per spec, seconds-only imputation should not set the imputation flag.
################################################################################
ex_ext <- ex %>%
derive_vars_dtm(
  dtc = EXSTDTC,
  new_vars_prefix = "EXST",
  highest_imputation = "s",
  time_imputation = "00:00:00",
  flag_imputation = "time",
  ignore_seconds_flag = TRUE
) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "s",
    time_imputation = "00:00:00",
    flag_imputation = "time",
    ignore_seconds_flag = TRUE
  )
# Merge the first qualifying EX start datetime into ADSL as TRTSDTM/TRTSTMF.
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add=ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>% 
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Convert datetime variables to dates 
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

# ---- 03 ITTFL ----------------------------------------------------------------
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N"))

# ---- 04 LSTAVLDT -------------------------------------------------------------
#######################################################
# 1. valid VS date
# 2. AE start date [var: AE.AESTDTC]
# 3. DS start date [var: DS.DSSTDTC]
# 4. treatment completion date (TRTEDTM -> TRTEDT) [var: ADSL.TRTEDTM]
#######################################################

# -------- 4.1 Pre-aggregate source datasets to one date per subject -----------
# Pre-aggregate each source to one candidate alive date per subject.
# This avoids duplicate event ties from multiple records on the same day.
vs_alive <- vs %>%
  filter(
    !is.na(convert_dtc_to_dt(VSDTC)),
    !(is.na(VSSTRESN) & is.na(VSSTRESC))
  ) %>%
  mutate(LSTAVLDT = convert_dtc_to_dt(VSDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTAVLDT = max(LSTAVLDT), .groups = "drop")

ae_alive <- ae %>%
  filter(!is.na(convert_dtc_to_dt(AESTDTC))) %>%
  mutate(LSTAVLDT = convert_dtc_to_dt(AESTDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTAVLDT = max(LSTAVLDT), .groups = "drop")

ds_alive <- ds %>%
  filter(!is.na(convert_dtc_to_dt(DSSTDTC))) %>%
  mutate(LSTAVLDT = convert_dtc_to_dt(DSSTDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTAVLDT = max(LSTAVLDT), .groups = "drop")

trt_alive <- adsl %>%
  filter(!is.na(TRTEDT)) %>%
  select(STUDYID, USUBJID, LSTAVLDT = TRTEDT)

# -------- 4.2 Derive LSTAVLDT  ------------------------------------------------
# Derive LSTAVLDT as the latest candidate alive date across VS, AE, DS, 
# and treatment end.
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs_alive",
        set_values_to = exprs(LSTAVLDT = LSTAVLDT)
      ),
      event(
        dataset_name = "ae_alive",
        set_values_to = exprs(LSTAVLDT = LSTAVLDT)
      ),
      event(
        dataset_name = "ds_alive",
        set_values_to = exprs(LSTAVLDT = LSTAVLDT)
      ),
      event(
        dataset_name = "trt_alive",
        set_values_to = exprs(LSTAVLDT = LSTAVLDT)
      )
    ),
    source_datasets = list(
      vs_alive = vs_alive,
      ae_alive = ae_alive,
      ds_alive = ds_alive,
      trt_alive = trt_alive
    ),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, event_nr),
    mode = "last",
    new_vars = exprs(LSTAVLDT)
  )

# ---- 05 Clean Dataset --------------------------------------------------------
adsl_out <- adsl %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    AGE, AGEU, AGEGR9, AGEGR9N,
    SEX, RACE, ETHNIC,
    ARM, ACTARM,
    TRTSDTM, TRTSTMF, TRTSDT,
    ITTFL,
    LSTAVLDT,
    everything()
  )

# ---- 06 Save Output ----------------------------------------------------------
saveRDS(adsl_out, "02_create_ADaM_ADSL.rds")
write.csv(adsl_out, "02_create_ADaM_ADSL.csv", row.names = FALSE, na = "")
