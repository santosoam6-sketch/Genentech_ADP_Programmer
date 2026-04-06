# =============================================================================
# Q1: DS Domain — Run Log
# Run this script to execute Q1 and produce a clean log file
# =============================================================================

log_file <- "question_1_sdtm/ds_domain_log.txt"
sink(log_file, split = TRUE)  # split=TRUE also prints to console while logging

cat("=============================================================================\n")
cat("Program  : question_1_sdtm/01_create_ds_domain.R\n")
cat("Purpose  : SDTM DS Domain Creation using {sdtm.oak}\n")
cat("Run Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================================\n\n")

# Run the main script, suppressing noisy oak info messages
suppressMessages(
  source("question_1_sdtm/01_create_ds_domain.R")
)

cat("=============================================================================\n")
cat("QC SUMMARY\n")
cat("=============================================================================\n")
cat("Total records    :", nrow(ds), "\n")
cat("Unique subjects  :", dplyr::n_distinct(ds$USUBJID), "\n\n")

cat("--- DSDECOD ---\n")
print(table(ds$DSDECOD, useNA = "always"))

cat("\n--- DSCAT ---\n")
print(table(ds$DSCAT, useNA = "always"))

cat("\n--- Missing Values by Variable ---\n")
missing <- sapply(names(ds), function(v) sum(is.na(ds[[v]])))
print(missing[missing > 0])
if (sum(missing) == 0) cat("No missing values in required variables.\n")

cat("\n--- Output Variables (in order) ---\n")
print(names(ds))

cat("\n=============================================================================\n")
cat("Program completed successfully.\n")
cat("Output saved to: question_1_sdtm/ds_domain.csv\n")
cat("                 question_1_sdtm/ds_domain.rds\n")
cat("=============================================================================\n\n")

cat("--- Session Info ---\n")
print(sessionInfo())

sink()
cat("Log written to:", log_file, "\n")
