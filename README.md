# Genentech_ADP_Programmer
This repository contains folders and files for the ADP Programmer screening interview coding assessment. 

## Repository Structure

- `question_1_sdtm/` : SDTM DS domain creation in R
- `question_2_adam/` : ADaM ADSL dataset creation in R
- `question_3_tlg/` : adverse event summary table and visualizations in R
- `question_4_genai/` : GenAI clinical data assistant in Python
- `README.md` : repository overview and notes

## Folder Contents
`question_1_sdtm/` /n
This folder contains the solution for Question 1: SDTM DS Domain Creation.

Contents:
- `01_create_ds_domain.R` : Main R script used to create the DS domain from the raw disposition data.
- `01_create_ds_domain.csv / 01_create_ds_domain.rds` : Output dataset files for the derived DS domain.
- `create_ds_domain_log.txt` : Log/output file included as evidence that the script runs error-free.
- `study_ct.csv` : Controlled terminology file used to support the derivation.

`question_2_adam/` /n
This folder contains the solution for Question 2: ADaM ADSL Dataset Creation.

Contents:
- `create_adsl.R` : Main R script used to create the ADSL dataset from SDTM source data.
- `02_create_ADaM_ADSL.csv / 02_create_ADaM_ADSL.rds` : Output dataset files for the derived ADSL dataset.
- `create_adsl_log.txt` : Log/output file included as evidence that the script runs error-free.
