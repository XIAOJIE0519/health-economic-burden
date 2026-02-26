################################################################################
## 05_transition_analysis.R
## 
## Purpose: Transition analysis between financial burden states
## 
## Outputs: Transition probabilities and odds ratios
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(broom)
})

################################################################################
## Load data
################################################################################

if (file.exists("./output/harmonized_data_corrected.RData")) {
  load("./output/harmonized_data_corrected.RData")
} else {
  stop("Data file not found. Please run 01_data_cleaning.R first.")
}

################################################################################
## Transition matrix
################################################################################

cat("\n========== Transition matrix ==========\n")

transition_data <- pooled_data %>%
  filter(!is.na(high_burden_10) & !is.na(id) & !is.na(wave)) %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    burden_state = case_when(
      burden_ratio < 0.05 ~ "Low",
      burden_ratio >= 0.05 & burden_ratio < 0.10 ~ "Medium",
      burden_ratio >= 0.10 ~ "High",
      TRUE ~ NA_character_
    ),
    burden_state = factor(burden_state, levels = c("Low", "Medium", "High")),
    next_burden_state = lead(burden_state),
    has_transition = !is.na(next_burden_state)
  ) %>%
  ungroup() %>%
  filter(has_transition)

transition_matrix <- transition_data %>%
  group_by(country, burden_state, next_burden_state) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(country, burden_state) %>%
  mutate(
    total = sum(n),
    prob = n / total
  ) %>%
  ungroup()

print(transition_matrix)

write_csv(transition_matrix, "./output/transition_probabilities.csv")

################################################################################
## Transition odds ratios
################################################################################

cat("\n========== Transition odds ratios ==========\n")

transition_or_data <- transition_data %>%
  filter(burden_state %in% c("Low", "Medium")) %>%
  mutate(
    transition_to_high = ifelse(next_burden_state == "High", 1, 0),
    age_continuous = age,
    educ_var = if("educ_level" %in% names(.)) educ_level else educ,
    educ_continuous = as.numeric(educ_var),
    has_insurance = ifelse(!is.na(insurance_any), insurance_any, 0)
  ) %>%
  filter(!is.na(gender) & !is.na(multimorbidity) & !is.na(rural) & 
         !is.na(ever_smoke) & !is.na(educ_continuous) & !is.na(age_continuous))

cat("Transition analysis sample size:\n")
transition_or_data %>%
  group_by(country) %>%
  summarise(
    n_transitions = n(),
    n_to_high = sum(transition_to_high),
    pct_to_high = mean(transition_to_high) * 100
  ) %>%
  print()

formula_transition <- transition_to_high ~ age_continuous + gender + rural + 
  educ_continuous + ever_smoke + has_insurance + multimorbidity

or_results_list <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\nFitting transition model for", cty, "...\n")
  
  data_cty <- transition_or_data %>% filter(country == cty)
  
  if (nrow(data_cty) < 50) {
    cat("  Insufficient sample size, skipping\n")
    next
  }
  
  model_cty <- glm(formula_transition, 
                   data = data_cty, 
                   family = binomial(link = "logit"))
  
  or_cty <- tidy(model_cty, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(country = cty)
  
  or_results_list[[cty]] <- or_cty
}

or_results <- bind_rows(or_results_list) %>%
  mutate(
    term_label = case_when(
      term == "age_continuous" ~ "Age (per year)",
      term == "gender" ~ "Female (vs Male)",
      term == "rural" ~ "Rural (vs Urban)",
      term == "educ_continuous" ~ "Education (per level)",
      term == "ever_smoke" ~ "Ever smoked (vs never)",
      term == "has_insurance" ~ "Has insurance (vs no)",
      term == "multimorbidity" ~ "Multimorbidity (>=2 conditions)",
      TRUE ~ term
    ),
    sig_label = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

print(or_results)

write_csv(or_results, "./output/transition_odds_ratios.csv")

cat("\n========== Transition analysis completed ==========\n")
