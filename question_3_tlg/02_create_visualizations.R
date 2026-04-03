# =============================================================================
# Q3: TLG - Adverse Events Reporting
# Script 2: Visualizations using {ggplot2)
# =============================================================================
# Purpose  : Create visualizations for:
#            Plot 1: AE severity distribution by treatment (bar chart or heatmap).
#            Plot 2: Top 10 most frequent AEs (with 95% CI for incidence rates)
# Input    : pharmaverseadam::adae, pharmaverseadam::adsl
# Output   :
# =============================================================================

# --- 00 Load Packages --------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(pharmaverseadam)

# ---- 01 Load Data ------------------------------------------------------------
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

teae <- adae %>% filter(TRTEMFL == "Y")

# ---- 02 Filter data to TEAEs ------------------------------------------------
# Denominators: number of treated subjects per arm
n_by_arm <- adsl %>%
  filter(!is.na(ACTARM) & ACTARM != "") %>%
  count(ACTARM, name = "N_arm")

n_total <- sum(n_by_arm$N_arm)

# ---- 03 Choose color palettes ------------------------------------------------
arm_colors <- c(
  "Xanomeline High Dose" = "#2166ac",
  "Xanomeline Low Dose" = "#74add1",
  "Placebo" = "#d73027"
)

sev_colors <- c(
  "MILD" = "#74c476",
  "MODERATE" = "#fd8d3c",
  "SEVERE" = "#d73027"
)

# Custom ggplot2 theme for clinical reports
theme_clinical <- function(base_size = 13) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = base_size + 1,
        hjust = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        color = "grey40",
        size = base_size - 1,
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        color = "grey50",
        size = base_size - 3,
        hjust = 0
      ),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#eef2f7"),
      strip.text = element_text(face = "bold")
    )
}

# ---- 04 Plot 1 ---------------------------------------------------------------
################################################################################
# AE Severity Distribution by Treatment Arm
# Subject-level: count distinct subjects per (ACTARM, AESEV) pair.
################################################################################
sev_data <- teae %>%
  filter (!is.na(AESEV) & ACTARM != "") %>%
  group_by(AESEV, ACTARM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(n_by_arm, by = "ACTARM") %>%
  mutate(AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")))

p1 <- ggplot(sev_data, aes(x = ACTARM, y = n_subj, fill = AESEV)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    color = "white"
  ) +
  geom_text(
    aes(label = n_subj),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 3.2,
    color = "grey25"
  ) +
  scale_fill_manual(
    values = sev_colors,
    name = "Severity/Intensity",
    breaks = c("MILD", "MODERATE", "SEVERE"),
    labels = c("Mild", "Moderate", "Severe")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Figure 1. Treatment-Emergent AE Severity Distribution by Treatment Arm",
    subtitle = "Safety Analysis Set; subjects counted once per severity level per arm",
    x = "Treatment Arm",
    y = "Number of Subjects",
    caption = "TEAEs defined as TRTEMFL = 'Y'. n = subjects with ≥1 AE at each severity."
  ) +
  theme_clinical()

ggsave(
  filename = file.path("question_3_tlg", "ae_severity_by_arm.png"),
  plot = p1,
  width = 10,
  height = 6,
  dpi = 150
)
cat("Saved: question_3_tlg/ae_severity_by_arm.png\n")


# ---- 05 Plot 2 ---------------------------------------------------------------
################################################################################
# Top 10 Most Frequent AEs with 95% CI for Incidence Rates
################################################################################
# Identify top 10 AEs by subject count
top10_terms <- teae %>%
  filter(!is.na(AETERM)) %>%
  group_by(AETERM) %>%
  summarise(n_total = n_distinct(USUBJID), .groups = "drop") %>%
  slice_max(order_by = n_total,
            n = 10, 
            with_ties = FALSE) %>%
  pull(AETERM)

cat("Top 10 AE terms:\n")
print(top10_terms)

# For each top-10 AETERM × ACTARM: count subjects and compute
# Wilson 95% CI for the IR.
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- x / n
  denom <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  list(
    est = phat,
    lo = pmax(0, centre - half),
    hi = pmin(1, centre + half)
  )
}

top10_data <- teae %>%
  filter (AETERM %in% top10_terms) %>%
  group_by(AETERM, ACTARM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  complete(AETERM, ACTARM, fill = list(n_subj = 0L)) %>%
  left_join(n_by_arm, by = "ACTARM") %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(n_subj, N_arm)),
    est = ci$est  * 100,
    lo = ci$lo   * 100,
    hi = ci$hi   * 100
  ) %>%
  ungroup() %>%
  select(-ci)

cat("First 5 rows of AETERM × ACTARM count subjects and 95% CI:\n")
print(head(top10_data))

# Reorder AETERM by overall frequency (total n_subj across arms, descending)
term_order <- top10_data %>%
  group_by(AETERM) %>%
  summarise (tot = sum(n_subj)) %>%
  arrange(tot) %>%
  pull(AETERM)

top10_data <- top10_data %>%
  mutate(
    AETERM = factor(AETERM, levels = term_order),
    ACTARM = factor(ACTARM, levels = arm_order)
  )

# Arm-level color mapping
arm_cols_plot <- if (all(arm_order %in% names(arm_colors))) {
  arm_colors[arm_order]
} else {
  setNames(scales::hue_pal()(length(arm_order)), arm_order)
}

# Make plot
p2 <- ggplot(top10_data, aes(
  x = AETERM,
  y = est,
  colour = ACTARM,
  shape = ACTARM
)) +
  geom_hline(yintercept = 0,
             colour = "grey80",
             linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = position_dodge(width = 0.55),
    width = 0.3,
    linewidth = 0.7
  ) +
  geom_point(position = position_dodge(width = 0.55), size     = 3) +
  coord_flip() +
  scale_colour_manual(values = arm_cols_plot, name = "Treatment Arm") +
  scale_shape_manual(values = c(16, 17, 15)[seq_along(arm_order)], name = "Treatment Arm") +
  scale_y_continuous(
    labels = function(x)
      paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Figure 2. Top 10 Most Frequent Treatment-Emergent Adverse Events",
    subtitle = "Incidence proportion (%) with 95% Wilson CI by treatment arm",
    x = NULL,
    y = "Incidence Rate (%)",
    caption = paste0(
      "TEAEs defined as TRTEMFL = 'Y'.",
      "Wilson 95% confidence interval."
    )
  ) +
  theme_clinical() +
  theme(
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_line(colour = "grey90")
  )

# Save plot 2
ggsave(
  filename = file.path("question_3_tlg", "ae_top10.png"),
  plot = p2,
  width = 11,
  height = 7,
  dpi = 150
)
cat("Saved: question_3_tlg/ae_top10.png\n")

print("Visualization creation complete.")
print("Files saved: ae_severity_by_arm.png, ae_top10.png")
