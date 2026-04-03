# =============================================================================
# Q3: TLG - Adverse Events Reporting
# Script 1: Treatment-Emergent Adverse Events (TEAE) Summary Table
# =============================================================================
# Purpose  : Create outputs for adverse events summary using the ADAE dataset 
#            and {gtsummary}. This tests your ability to create regulatory-
#            compliant clinical reports.
# Input    : pharmaverseadam::adae, pharmaverseadam::adsl
# Output   : A summary table of treatment-emergent adverse events (TEAEs).
#            question_3_tlg/ae_summary_table.html
# =============================================================================

# --- 00 Load Packages --------------------------------------------------------
library(dplyr)
library(tidyr)
library(gtsummary)
library(gt)
library(pharmaverseadam) 

# ---- 01 Load Data ------------------------------------------------------------
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

cat("ADSL rows:", nrow(adsl), "\n")
cat("ADAE rows:", nrow(adae), "\n")

# ---- 02 Filter to Treatment-Emergent AEs -------------------------------------
teae <- adae %>%
  filter(TRTEMFL == "Y")

cat("TEAE rows after TRTEMFL filter:", nrow(teae), "\n")

# ---- 03 Derive denominator: treated subjects per arm -------------------------
################################################################################
# ADSL has one record per subject- Use ALL randomised / treated subjects from ADSL.
################################################################################
n_by_arm <- adsl %>%
  filter(!is.na(ACTARM) & ACTARM != "") %>%
  count(ACTARM, name = "N_arm")

n_total <- sum(n_by_arm$N_arm)

cat("Denominator subjects per arm:\n")
print(n_by_arm)
cat("Total treated subjects:", n_total, "\n")

# ---- 04 Count unique subjects with ≥1 TEAE -----------------------------------
################################################################################
# count_subjects() Function: Takes any groupings and adds ACTARM to the grouping
#                            and counts distinct subject IDs within each group.
################################################################################
count_subjects <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(c(group_vars, "ACTARM")))) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop")
}

# ---- 4.1 Any TEAE (Grand Summary) --------------------------------------------
# How many subjects had at least one TEAE per arm? 
any_teae <- teae %>%
  group_by(ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(AESOC = "Any TEAE", AETERM = "Any TEAE")

cat("Subjects that had at least one TEAE per arm: ", "\n") 
print(any_teae)

# ---- 4.2 By System Organ Class (AESOC) ---------------------------------------
# How many subjects had at least one TEAE in each body system per arm? 
by_soc <- count_subjects(teae, c("AESOC")) %>%
  mutate(AETERM = NA_character_)

cat("Subjects that had at least one TEAE in each AESOC: ", "\n") 
print(by_soc)

# ---- 4.3 By Preferred Term (nested under SOC) --------------------------------
# How many subjects had at least one instance of this specific AE per arm? 
by_pt <- count_subjects(teae, c("AESOC", "AETERM"))

cat("Subjects that had at least one instance of this specific AE per arm: ", "\n") 
print(by_pt)

# ---- 05 Pivot Wide -----------------------------------------------------------
################################################################################
# pivot_wide() Function: Reshapes into wide format, one col per treatment arm
#                        Outputs table in n(%) format.
################################################################################
pivot_wide <- function(df, n_by_arm_df) {
  df %>%
    left_join(n_by_arm_df, by = "ACTARM") %>%
    mutate(
      pct = round(n / N_arm * 100, 1),
      cell = paste0(n, " (", pct, "%)")
    ) %>%
    select(AESOC, AETERM, ACTARM, cell) %>%
    pivot_wider(names_from = ACTARM, values_from = cell, values_fill = "0 (0.0%)")
}

wide_any <- pivot_wide(any_teae, n_by_arm)
wide_soc <- pivot_wide(by_soc, n_by_arm)
wide_pt <- pivot_wide(by_pt, n_by_arm)

# ---- 06 Add Total Column -----------------------------------------------------
################################################################################
# add_total() Function: Takes groupings and counts distinct subject IDs 
#                       across all arms combined. Outputs in n(%) format.
################################################################################
add_total <- function(df_long, group_vars, n_total_val) {
  df_long  %>%
    group_by(across(all_of(group_vars)))  %>%
    summarise(n_total = n_distinct(USUBJID), .groups = "drop")  %>%
    mutate(
      pct_total = round(n_total / n_total_val * 100, 1),
      Total = paste0(n_total, " (", pct_total, "%)")
    )  %>%
    select(all_of(group_vars), Total)
}

total_any <- teae  %>%
  summarise(n_total = n_distinct(USUBJID))  %>%
  mutate(
    pct_total = round(n_total / n_total * 100, 1),
    Total = paste0(n_total, " (100.0%)"),
    AESOC = "Any TEAE",
    AETERM = "Any TEAE"
  )  %>%
  select(AESOC, AETERM, Total)

total_soc <- add_total(teae, c("AESOC"), n_total)  %>%
  mutate(AETERM = NA_character_)

total_pt <- add_total(teae, c("AESOC", "AETERM"), n_total)

# Join totals onto wide tables
wide_any <- wide_any %>% left_join(total_any, by = c("AESOC", "AETERM"))
wide_soc <- wide_soc %>% left_join(total_soc, by = c("AESOC", "AETERM"))
wide_pt <- wide_pt %>% left_join(total_pt, by = c("AESOC", "AETERM"))

# ---- 07 Sort: by descending frequency ----------------------------------------
# Sort SOC by total n descending
soc_order <- total_soc %>%
  arrange(desc(n_total)) %>%
  pull(AESOC)

# Sort PT within SOC by total n descending
pt_order <- total_pt |>
  arrange(AESOC, desc(n_total))

# ---- 08 Assemble -------------------------------------------------------------
################################################################################
# build_display_table() function: Builds table at each row. 
# Assembles the final summary table by stacking rows:
#   1. Grand total row (Subjects with at least 1 TEAE)
#   2. For each SOC (descending frequency):
#        - One SOC header row
#        - One row per PT under that SOC
################################################################################
arm_cols <- setdiff(names(wide_any), c("AESOC", "AETERM", "Total"))

build_display_table <- function(any_row, wide_soc_df, wide_pt_df,
                                soc_order_vec, pt_order_df, arm_cols_vec) {
  rows <- list()
  
  # Grand total row
  any_formatted <- any_row %>%
    mutate(Label = "Subjects with at least 1 TEAE", Indent = 0L) %>%
    select(Label, Indent, all_of(arm_cols_vec), Total)
  rows[["any"]] <- any_formatted
  
  # SOC + PT rows
  for (soc in soc_order_vec) {
    soc_row <- wide_soc_df %>%
      filter(AESOC == soc) %>%
      mutate(Label = soc, Indent = 0L) %>%
      select(Label, Indent, all_of(arm_cols_vec), Total)
    rows[[paste0("soc_", soc)]] <- soc_row
    
    pts <- pt_order_df %>%
      filter(AESOC == soc) %>%
      arrange(desc(n_total))
    
    for (i in seq_len(nrow(pts))) {
      pt_val <- pts$AETERM[i]
      pt_row <- wide_pt_df %>%
        filter(AESOC == soc, AETERM == pt_val) %>%
        mutate(Label = paste0("  ", pt_val), Indent = 1L) %>%
        select(Label, Indent, all_of(arm_cols_vec), Total)
      rows[[paste0("pt_", soc, "_", pt_val)]] <- pt_row
    }
  }
  
  bind_rows(rows)
}

display_tbl <- build_display_table(
  wide_any, wide_soc, wide_pt,
  soc_order, pt_order, arm_cols
)

cat("First 5 rows of summary table:")
print(head(display_tbl))
cat("Display table rows:", nrow(display_tbl), "\n")

# ---- 09 Render as gt table and export as html ---------------------------------
################################################################################
# Takes table and transforms into styled, publication-ready HTML table using {gt}
# package. 
################################################################################
# Build column header: Arm (N=xx)
arm_labels <- setNames(
  paste0(n_by_arm$ACTARM, "\n(N=", n_by_arm$N_arm, ")"),
  n_by_arm$ACTARM
)

gt_tbl <- display_tbl %>%
  select(-Indent) %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Summary of Treatment-Emergent Adverse Events (TEAEs)**"),
    subtitle = md("*Safety Analysis Set*")
  ) %>%
  cols_label(
    Label = md("**Adverse Event**"),
    Total = md(paste0("**Total**<br>(N=", n_total, ")"))
  ) %>%
  cols_align(align = "left",   columns = Label) %>%
  cols_align(align = "center", columns = c(all_of(arm_cols), "Total")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Label,
      rows = !startsWith(display_tbl$Label, "  ")
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = !startsWith(display_tbl$Label, "  "))
  ) %>%
  tab_footnote(
    footnote = paste0(
      "n = number of subjects with at least one occurrence; ",
      "% = n / N × 100 where N is the number of treated subjects in that arm. ",
      "Subjects are counted once per preferred term regardless of number of occurrences. ",
      "TEAEs defined as TRTEMFL = 'Y'."
    )
  ) %>%
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) %>%
  tab_options(
    table.width = pct(100),
    heading.align = "left",
    column_labels.font.size = px(13),
    data_row.padding = px(4)
  )

# Apply arm column labels
for (arm in arm_cols) {
  n_arm_val <- n_by_arm$N_arm[n_by_arm$ACTARM == arm]
  gt_tbl <- gt_tbl %>%
    cols_label(
      !!arm := md(paste0("**", arm, "**<br>(N=", n_arm_val, ")"))
    )
}

# Export to HTML
out_path <- file.path("question_3_tlg", "ae_summary_table.html")
gtsave(gt_tbl, filename = out_path)
cat("Saved:", out_path, "\n")

print("Summary Table creation complete.")
print("Files saved: ae_summary_table.html, 01_create_ae_summary_table.R")
