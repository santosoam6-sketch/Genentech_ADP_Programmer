# Genentech_ADP_Programmer
This repository contains folders and files for the ADP Programmer screening interview coding assessment. 

## Repository Structure

- `question_1_sdtm/` : SDTM DS domain creation in R
- `question_2_adam/` : ADaM ADSL dataset creation in R
- `question_3_tlg/` : adverse event summary table and visualizations in R
- `question_4_genai/` : GenAI clinical data assistant in Python
- `README.md` : repository overview and notes

## Folder Contents
`question_1_sdtm/` \
This folder contains the solution for Question 1: SDTM DS Domain Creation.

Contents:
- `01_create_ds_domain.R` : Main R script used to create the DS domain from the raw disposition data.
- `01_create_ds_domain.csv / 01_create_ds_domain.rds` : Output dataset files for the derived DS domain.
- `create_ds_domain_log.txt` : Log/output file as evidence that the script runs error-free.
- `study_ct.csv` : Controlled terminology file used to support the derivation.

`question_2_adam/` \
This folder contains the solution for Question 2: ADaM ADSL Dataset Creation.

Contents:
- `create_adsl.R` : Main R script used to create the ADSL dataset from SDTM source data.
- `02_create_ADaM_ADSL.csv / 02_create_ADaM_ADSL.rds` : Output dataset files for the derived ADSL dataset.
- `create_adsl_log.txt` : Log/output file as evidence that the script runs error-free.

`question_3_tlg` \
This folder contains the solution for Question 3: TLG: Adverse Events Reporting. 

Contents: 
- `01_create_ae_summary_table.R` : Script used to create the treatment-emergent adverse event summary table.
- `02_create_visualizations.R` : Script used to create the adverse event visualizations.
- `ae_summary_table.html` : Output summary table.
- `ae_severity_by_arm.png` : Visualization of AE severity distribution by treatment arm.
- `ae_top10.png` : Visualization of the top 10 most frequent adverse events.
- `01_create_ae_summary_table_log.txt` : Log/output file for the summary table script.
- `02_create_visualizations_log.txt`: Log/output file for the visualization script.

`question_4_genai/` \
This folder contains the solution for Question 4: GenAI Clinical Data Assistant.

Contents: 
- `clinical_data_agent.py` : Main Python script implementing the clinical data assistant logic.
- `test_script.py` : Test script that runs 3 example queries and prints the results.
- `adae.csv` : AE dataset used for `test_script.py`.
- `adae_data.R` : Supporting script used to export the AE dataset for Python use.
