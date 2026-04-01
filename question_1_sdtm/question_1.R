# =============================================================================
# Q1: SDTM DS (Disposition) Domain Creation using {sdtm.oak}
# =============================================================================
# Purpose  : Create the SDTM DS domain from raw clinical trial data
# Input    : pharmaverseraw::ds_raw, study_ct.csv
# Output   : DS domain dataset with variables:
#            STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
#            DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
# =============================================================================

# ---- 00 Load Packages ----------------------------------------------
library(sdtm.oak)        
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)           
library(lubridate)        

# ---- 01 Load Data ----------------------------------------------
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm
study_ct <- read.csv("question_1_sdtm/study_ct.csv", stringsAsFactors = FALSE)

# ---- 02 Add Oak Variables -----------------------------------
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )
ds_raw

# ---- 03 Some Pre-Processing ------------------------------------------------
# Normalizing some raw values before CT Mapping
ds_raw <- ds_raw |>
  mutate(IT.DSDECOD = case_when(
    IT.DSDECOD == "Completed"~ "Complete",
    IT.DSDECOD == "Screen Failure"~ "Trial Screen Failure",
    IT.DSDECOD == "Lost to Follow-Up" ~ "Lost To Follow-Up",
    IT.DSDECOD == "Study Terminated by Sponsor" ~ "Study Terminated By Sponsor",
    TRUE ~ IT.DSDECOD
  ))

# Combine Date + Time into a single datetime column 
ds_raw <- ds_raw |>
  mutate(
    DSDTC_RAW = case_when(
      !is.na(DSDTCOL) & !is.na(DSTMCOL) & DSTMCOL != "" ~
        paste0(DSDTCOL, "T", DSTMCOL),
      !is.na(DSDTCOL) ~ as.character(DSDTCOL),
      TRUE ~ NA_character_
    )
  )

# ---- 04 Derive DS Domain using {sdtm.oak} -----------------------
# Derive topic variable for: STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, 
# DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
ds <- assign_no_ct(
  tgt_dat = NULL,         
  tgt_var  = "STUDYID",  
  raw_dat  = ds_raw,     
  raw_var  = "STUDY",   
  id_vars  = oak_id_vars() 
) |>
  
  # DOMAIN: hardcoded to "DS" 
  hardcode_no_ct(
    tgt_var = "DOMAIN",
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_val = "DS"
  ) |>
  
  # USUBJID
  assign_no_ct(
    tgt_var = "USUBJID",
    raw_dat = ds_raw,
    raw_var = "PATNUM"
  ) |>
  
  # DSTERM
  assign_no_ct(
    tgt_var = "DSTERM",
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM"    
  ) |>
  
  # DSDECOD
  assign_ct(
    tgt_var  = "DSDECOD",
    raw_dat  = ds_raw,
    raw_var  = "IT.DSDECOD",
    ct_spec  = study_ct,
    ct_clst  = "C66727"
  ) |>
  
  # DSDTC collection date-time 
  assign_no_ct(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = "DSDTC_RAW"
  ) |>
  
  # DSSTDTC start date of ds event
  assign_no_ct(
    tgt_var = "DSSTDTC",
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT"
  ) |>
  
  mutate(DSDECOD = case_when(
    is.na(DSDECOD) & DSTERM == "Randomized" ~ "RANDOMIZED",
    TRUE ~ DSDECOD
  )) |> 

  # DSCAT 
  mutate(DSCAT = case_when(
    DSDECOD %in% c("COMPLETED", "SCREEN FAILURE", "RANDOMIZED") ~ "PROTOCOL MILESTONE",
    DSDECOD == "DEATH"~ "DISPOSITION EVENT",
    TRUE~ "OTHER EVENT"
  )) 
  
# Add VISITNUM AND VISIT
# Both variables does not seem to be collected in raw data
# Will leave as NA 
ds <- ds |>
  mutate(
    VISITNUM = NA_real_,
    VISIT    = NA_character_
  )

# ---- 05 Derive DSSEQ --------------------------------------------------------
ds <- ds %>%
  derive_seq(
    tgt_var  = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC")
  ) %>%
  derive_study_day(
    sdtm_in      = .,       # dot = the piped ds dataset
    dm_domain    = dm,      # dm provides RFSTDTC per subject
    tgdt         = "DSSTDTC",   # the date to compute study day for
    refdt        = "RFSTDTC",   # reference start date from dm
    study_day_var = "DSSTDY"
  ) %>% 
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, 
         VISIT, DSDTC, DSSTDTC, DSSTDY)

# ---- 06 Save Output ---------------------------------------------------------
saveRDS(ds, "01_create_ds_domain.rds")
write.csv(ds, "01_create_ds_domain.csv", row.names = FALSE, na = "")
