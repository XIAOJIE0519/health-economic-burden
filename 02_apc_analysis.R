################################################################################
## 02_apc_analysis.R
## 
## Purpose: Age-Period-Cohort analysis using Generalized Additive Models
## 
## Outputs: APC effects for overall and subgroups
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(mgcv)
  library(patchwork)
})

################################################################################
## Load cleaned data
################################################################################

if (file.exists("./output/harmonized_data_corrected.RData")) {
  load("./output/harmonized_data_corrected.RData")
} else {
  stop("Data file not found. Please run 01_data_cleaning.R first.")
}

################################################################################
## Parameters
################################################################################
AGE_MIN <- 50
AGE_MAX <- 90
AGE_BIN_WIDTH <- 5
PERIOD_BIN_WIDTH_CHARLS <- 3
PERIOD_BIN_WIDTH_SHARE <- 4
PERIOD_BIN_WIDTH_HRS <- 5
COHORT_BIN_WIDTH <- 5
MIN_CELL_SIZE <- 30

################################################################################
## Utility functions
################################################################################
bin_var <- function(x, width = 5) {
  floor(x / width) * width
}

safe_k <- function(n_unique, max_k = 8) {
  max(3, min(max_k, n_unique - 1))
}

################################################################################
## APC data preparation
################################################################################
prepare_apc_data <- function(data, country_name, outcome = "log_oop") {
  
  period_width <- case_when(
    country_name == "China" ~ PERIOD_BIN_WIDTH_CHARLS,
    country_name == "Europe" ~ PERIOD_BIN_WIDTH_SHARE,
    country_name == "USA" ~ PERIOD_BIN_WIDTH_HRS,
    TRUE ~ 5
  )
  
  apc_data <- data %>%
    filter(country == country_name) %>%
    filter(!is.na(age) & !is.na(cohort) & !is.na(period)) %>%
    filter(!is.na(!!sym(outcome))) %>%
    filter(age >= AGE_MIN & age <= AGE_MAX) %>%
    mutate(
      age_int = as.integer(round(age)),
      period_int = as.integer(period),
      cohort_int = as.integer(cohort),
      age_gap = abs(age_int - (period_int - cohort_int))
    ) %>%
    filter(age_gap <= 2) %>%
    mutate(
      age_bin = bin_var(age_int, AGE_BIN_WIDTH),
      period_bin = bin_var(period_int, period_width),
      cohort_bin = bin_var(cohort_int, COHORT_BIN_WIDTH),
      age_mid = age_bin + AGE_BIN_WIDTH / 2,
      period_mid = period_bin + period_width / 2,
      cohort_mid = cohort_bin + COHORT_BIN_WIDTH / 2
    )
  
  cat("  Using observations:", nrow(apc_data), "\n")
  
  cell_counts <- apc_data %>%
    group_by(age_bin, period_bin, cohort_bin) %>%
    summarise(n_cell = n(), .groups = "drop") %>%
    filter(n_cell >= MIN_CELL_SIZE)
  
  apc_filtered <- apc_data %>%
    semi_join(cell_counts, by = c("age_bin", "period_bin", "cohort_bin"))
  
  return(apc_filtered)
}

################################################################################
## GAM fitting
################################################################################
fit_apc_gam <- function(data, outcome = "log_oop", subgroup_name = "Overall") {
  
  n_age <- length(unique(data$age_mid))
  n_per <- length(unique(data$period_mid))
  n_coh <- length(unique(data$cohort_mid))
  
  k_age <- safe_k(n_age)
  k_per <- safe_k(n_per)
  k_coh <- safe_k(n_coh)
  
  cat("  [", subgroup_name, "] n_age =", n_age, ", n_period =", n_per, 
      ", n_cohort =", n_coh, "\n")
  
  if (n_age < 3 || n_per < 3 || n_coh < 3) {
    cat("  WARNING: Too few unique values. Skipping.\n")
    return(NULL)
  }
  
  formula_str <- paste0(outcome, " ~ s(age_mid, bs='tp', k=", k_age, 
                        ") + s(period_mid, bs='tp', k=", k_per,
                        ") + s(cohort_mid, bs='tp', k=", k_coh, ")")
  
  gam_model <- gam(as.formula(formula_str), data = data, method = "REML")
  
  cat("  Dev. explained:", round(summary(gam_model)$dev.expl * 100, 2), "%\n")
  
  return(gam_model)
}

################################################################################
## Extract GAM effects
################################################################################
extract_gam_effects <- function(gam_model, data, country_name, subgroup_name) {
  
  age_vals <- sort(unique(data$age_mid))
  period_vals <- sort(unique(data$period_mid))
  cohort_vals <- sort(unique(data$cohort_mid))
  
  med_age <- median(data$age_mid)
  med_period <- median(data$period_mid)
  med_cohort <- median(data$cohort_mid)
  
  # Age effect
  newdata_age <- data.frame(
    age_mid = age_vals,
    period_mid = med_period,
    cohort_mid = med_cohort
  )
  pred_age <- predict(gam_model, newdata_age, se.fit = TRUE, type = "terms")
  age_df <- tibble(
    x = age_vals,
    fit = as.numeric(pred_age$fit[, "s(age_mid)"]),
    se = as.numeric(pred_age$se.fit[, "s(age_mid)"]),
    ci_lower = fit - 1.96 * se,
    ci_upper = fit + 1.96 * se,
    term = "Age",
    country = country_name,
    subgroup = subgroup_name
  )
  
  # Period effect
  newdata_period <- data.frame(
    age_mid = med_age,
    period_mid = period_vals,
    cohort_mid = med_cohort
  )
  pred_period <- predict(gam_model, newdata_period, se.fit = TRUE, type = "terms")
  period_df <- tibble(
    x = period_vals,
    fit = as.numeric(pred_period$fit[, "s(period_mid)"]),
    se = as.numeric(pred_period$se.fit[, "s(period_mid)"]),
    ci_lower = fit - 1.96 * se,
    ci_upper = fit + 1.96 * se,
    term = "Period",
    country = country_name,
    subgroup = subgroup_name
  )
  
  # Cohort effect
  newdata_cohort <- data.frame(
    age_mid = med_age,
    period_mid = med_period,
    cohort_mid = cohort_vals
  )
  pred_cohort <- predict(gam_model, newdata_cohort, se.fit = TRUE, type = "terms")
  cohort_df <- tibble(
    x = cohort_vals,
    fit = as.numeric(pred_cohort$fit[, "s(cohort_mid)"]),
    se = as.numeric(pred_cohort$se.fit[, "s(cohort_mid)"]),
    ci_lower = fit - 1.96 * se,
    ci_upper = fit + 1.96 * se,
    term = "Cohort",
    country = country_name,
    subgroup = subgroup_name
  )
  
  bind_rows(age_df, period_df, cohort_df)
}

################################################################################
## Overall APC analysis
################################################################################
cat("\n========== Overall APC analysis ==========\n")

overall_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  apc_data <- prepare_apc_data(pooled_data, cty, outcome = "log_oop")
  
  if (nrow(apc_data) < 100) {
    cat("  Insufficient data. Skipping.\n")
    next
  }
  
  gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", subgroup_name = "Overall")
  
  if (!is.null(gam_model)) {
    effects <- extract_gam_effects(gam_model, apc_data, cty, "Overall")
    overall_results[[cty]] <- effects
  }
}

overall_effects <- bind_rows(overall_results)
write_csv(overall_effects, "./output/apc_effects_overall.csv")

################################################################################
## Subgroup analysis: Gender
################################################################################
cat("\n========== Subgroup analysis: Gender ==========\n")

gender_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  for (gnd in c(0, 1)) {
    gender_label <- ifelse(gnd == 0, "Male", "Female")
    cat("  Subgroup:", gender_label, "\n")
    
    data_sub <- pooled_data %>% filter(gender == gnd)
    apc_data <- prepare_apc_data(data_sub, cty, outcome = "log_oop")
    
    if (nrow(apc_data) < 100) {
      cat("    Insufficient data. Skipping.\n")
      next
    }
    
    gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", 
                             subgroup_name = gender_label)
    
    if (!is.null(gam_model)) {
      effects <- extract_gam_effects(gam_model, apc_data, cty, gender_label)
      gender_results[[paste(cty, gender_label, sep = "_")]] <- effects
    }
  }
}

gender_effects <- bind_rows(gender_results)
write_csv(gender_effects, "./output/apc_effects_by_gender.csv")

################################################################################
## Subgroup analysis: Residence
################################################################################
cat("\n========== Subgroup analysis: Residence ==========\n")

rural_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  for (rur in c(0, 1)) {
    rural_label <- ifelse(rur == 0, "Urban", "Rural")
    cat("  Subgroup:", rural_label, "\n")
    
    data_sub <- pooled_data %>% filter(rural == rur)
    apc_data <- prepare_apc_data(data_sub, cty, outcome = "log_oop")
    
    if (nrow(apc_data) < 100) {
      cat("    Insufficient data. Skipping.\n")
      next
    }
    
    gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", 
                             subgroup_name = rural_label)
    
    if (!is.null(gam_model)) {
      effects <- extract_gam_effects(gam_model, apc_data, cty, rural_label)
      rural_results[[paste(cty, rural_label, sep = "_")]] <- effects
    }
  }
}

rural_effects <- bind_rows(rural_results)
write_csv(rural_effects, "./output/apc_effects_by_residence.csv")

################################################################################
## Subgroup analysis: Education
################################################################################
cat("\n========== Subgroup analysis: Education ==========\n")

education_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  educ_var <- if("educ_level" %in% names(pooled_data)) "educ_level" else "educ"
  
  for (edu_level in c(1, 2, 3)) {
    edu_label <- case_when(
      edu_level == 1 ~ "Low",
      edu_level == 2 ~ "Medium",
      edu_level == 3 ~ "High"
    )
    cat("  Subgroup:", edu_label, "\n")
    
    data_sub <- pooled_data %>%
      filter(country == cty) %>%
      filter(!is.na(!!sym(educ_var)) & !!sym(educ_var) == edu_level)
    
    if (nrow(data_sub) < 100) {
      cat("    Insufficient data. Skipping.\n")
      next
    }
    
    apc_data <- prepare_apc_data(data_sub, cty, outcome = "log_oop")
    
    if (nrow(apc_data) < 100) {
      cat("    Insufficient data after filtering. Skipping.\n")
      next
    }
    
    gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", 
                             subgroup_name = edu_label)
    
    if (!is.null(gam_model)) {
      effects <- extract_gam_effects(gam_model, apc_data, cty, edu_label)
      education_results[[paste(cty, edu_level, sep = "_")]] <- effects
    }
  }
}

education_effects <- bind_rows(education_results)
write_csv(education_effects, "./output/apc_effects_by_education.csv")

################################################################################
## Subgroup analysis: Smoking
################################################################################
cat("\n========== Subgroup analysis: Smoking ==========\n")

smoking_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  for (smk in c(0, 1)) {
    smoking_label <- ifelse(smk == 0, "Never Smoked", "Ever Smoked")
    cat("  Subgroup:", smoking_label, "\n")
    
    data_sub <- pooled_data %>% filter(ever_smoke == smk)
    apc_data <- prepare_apc_data(data_sub, cty, outcome = "log_oop")
    
    if (nrow(apc_data) < 100) {
      cat("    Insufficient data. Skipping.\n")
      next
    }
    
    gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", 
                             subgroup_name = smoking_label)
    
    if (!is.null(gam_model)) {
      effects <- extract_gam_effects(gam_model, apc_data, cty, smoking_label)
      smoking_results[[paste(cty, smoking_label, sep = "_")]] <- effects
    }
  }
}

smoking_effects <- bind_rows(smoking_results)
write_csv(smoking_effects, "./output/apc_effects_by_smoking.csv")

################################################################################
## Subgroup analysis: Multimorbidity
################################################################################
cat("\n========== Subgroup analysis: Multimorbidity ==========\n")

multimorbidity_results <- list()

for (cty in c("China", "Europe", "USA")) {
  cat("\n--- ", cty, " ---\n")
  
  for (mm in c(0, 1)) {
    mm_label <- ifelse(mm == 0, "No Multimorbidity", "Multimorbidity")
    cat("  Subgroup:", mm_label, "\n")
    
    data_sub <- pooled_data %>% filter(multimorbidity == mm)
    apc_data <- prepare_apc_data(data_sub, cty, outcome = "log_oop")
    
    if (nrow(apc_data) < 100) {
      cat("    Insufficient data. Skipping.\n")
      next
    }
    
    gam_model <- fit_apc_gam(apc_data, outcome = "log_oop", 
                             subgroup_name = mm_label)
    
    if (!is.null(gam_model)) {
      effects <- extract_gam_effects(gam_model, apc_data, cty, mm_label)
      multimorbidity_results[[paste(cty, mm_label, sep = "_")]] <- effects
    }
  }
}

multimorbidity_effects <- bind_rows(multimorbidity_results)
write_csv(multimorbidity_effects, "./output/apc_effects_by_multimorbidity.csv")

cat("\n========== APC analysis completed ==========\n")
