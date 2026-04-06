# Genentech_ADP_Programmer
This repository contains folders and files for the ADP Programmer screening interview coding assessment. 

## Repository Structure

- `question_1_sdtm/` : SDTM DS domain creation in R
- `question_2_adam/` : ADaM ADSL dataset creation in R
- `question_3_tlg/` : adverse event summary table and visualizations in R
- `question_4_genai/` : GenAI clinical data assistant in Python
- `README.md` : repository overview and notes

## Folder Contents
### `question_1_sdtm/`
Solution for **Question 1: SDTM DS Domain Creation**.

| File | Description |
|------|-------------|
| `01_create_ds_domain.R` | Main R script to create the DS domain from raw disposition data |
| `01_create_ds_domain.csv` | Output DS domain dataset (CSV format) |
| `01_create_ds_domain.rds` | Output DS domain dataset (RDS format) |
| `create_ds_domain_log.txt` | Script execution log confirming error-free run |
| `study_ct.csv` | Controlled terminology file used to support the derivation |

**To run:** Open `01_create_ds_domain.R` and run from the `question_1_sdtm/` directory. Requires R ≥ 4.1 with `tidyverse` and `haven` installed.

---

### `question_2_adam/`
Solution for **Question 2: ADaM ADSL Dataset Creation**.

| File | Description |
|------|-------------|
| `create_adsl.R` | Main R script to create the ADSL dataset from SDTM source data |
| `02_create_ADaM_ADSL.csv` | Output ADSL dataset (CSV format) |
| `02_create_ADaM_ADSL.rds` | Output ADSL dataset (RDS format) |
| `create_adsl_log.txt` | Script execution log confirming error-free run |

**To run:** Open `create_adsl.R` and run from the `question_2_adam/` directory. Requires R ≥ 4.1 with `tidyverse`, `haven`, and `admiral` installed.

---

### `question_3_tlg/`
Solution for **Question 3: TLG — Adverse Events Reporting**.

| File | Description |
|------|-------------|
| `01_create_ae_summary_table.R` | Script to create the treatment-emergent AE summary table |
| `02_create_visualizations.R` | Script to create AE visualizations |
| `ae_summary_table.html` | Output summary table (open in any browser) |
| `ae_severity_by_arm.png` | Visualization of AE severity distribution by treatment arm |
| `ae_top10.png` | Visualization of the top 10 most frequent adverse events |
| `01_create_ae_summary_table_log.txt` | Execution log for the summary table script |
| `02_create_visualizations_log.txt` | Execution log for the visualizations script |

**To run:** Run each script independently from the `question_3_tlg/` directory. Requires R ≥ 4.1 with `tidyverse`, `gt`, and `ggplot2` installed.

---

### `question_4_genai/`
Solution for **Question 4: GenAI Clinical Data Assistant**.

| File | Description |
|------|-------------|
| `clinical_data_agent.py` | Main Python script implementing the clinical data assistant logic |
| `test_script.py` | Test script that runs 3 example queries and prints results |
| `adae.csv` | AE dataset used as input for `test_script.py` |
| `adae_data.R` | Supporting R script used to export the AE dataset for Python use |

**To run:** From the `question_4_genai/` directory, run `python test_script.py`. Requires Python ≥ 3.9 with `pandas` installed. 

**Notes:** 
This solution maps natural language questions to the relevant AE dataset variable, applies a Pandas filter, and returns the count of unique subjects and matching subject IDs.

I chose a Claude-style parsing approach, but since no API key was used, I implemented a mock version using keyword-based simulation. This preserves the full logic flow:
- parse the user question
- identify the target column
- identify the filter value
- execute the filter on the dataset
- return matching subjects

No external API key is required to run this version.
