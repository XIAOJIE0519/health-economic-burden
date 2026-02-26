################################################################################
## 03_burden_analysis.R
## 
## Purpose: Analysis of financial burden determinants
## 
## Outputs: Logistic regression models and predicted probabilities
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(splines)
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
## Prepare analysis data
################################################################################

burden_data <- pooled_data %>%
  filter(!is.na(age) & age >= 50 & age <= 90) %>%
  filter(!is.na(burden_ratio) & !is.na(hh_income_intl)) %>%
  filter(hh_income_intl > 0) %>%
  filter(!is.na(gender) & !is.na(n_chronic)) %>%
  mutate(
    age_centered = age - 65,
    educ_var = if("educ_level" %in% names(pooled_data)) educ_level else educ,
    educ_cat = factor(educ_var, levels = c(1, 2, 3), labels = c("Low", "Medium", "High")),
    chronic_cat = case_when(
      n_chronic == 0 ~ "0",
      n_chronic == 1 ~ "1",
      n_chronic == 2 ~ "2",
      n_chronic == 3 ~ "3",
      n_chronic >= 4 ~ "4+",
      TRUE ~ NA_character_
    ),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "2", "3", "4+")),
    income_quartile = ntile(hh_income_intl, 4),
    income_quartile = factor(income_quartile, levels = 1:4, 
                             labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    country = factor(country, levels = c("USA", "China", "Europe"))
  )

cat("Analysis sample size:", nrow(burden_data), "\n")
cat("High burden (10%) rate:", mean(burden_data$high_burden_10, na.rm = TRUE) * 100, "%\n")

################################################################################
## Logistic regression: High burden (10% threshold)
################################################################################
cat("\n========== Logistic regression: High burden (10%) ==========\n")

formula_burden10 <- high_burden_10 ~ ns(age, df = 4) * country + 
  gender + rural + educ_cat + chronic_cat + 
  insurance_any + income_quartile

model_burden10 <- glm(formula_burden10, 
                      data = burden_data, 
                      family = binomial(link = "logit"))

summary(model_burden10)

coef_burden10 <- tidy(model_burden10, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(model = "HighBurden_10pct") %>%
  rename(OR = estimate, OR_lower = conf.low, OR_upper = conf.high)

write_csv(coef_burden10, "./output/model_burden10_coefficients.csv")

cat("High burden model fitted\n")

################################################################################
## Interaction model: Chronic conditions × Insurance (China & USA only)
################################################################################
cat("\n========== Interaction model: Chronic × Insurance (China & USA) ==========\n")

burden_data_china_usa <- burden_data %>%
  filter(country %in% c("China", "USA")) %>%
  mutate(country = factor(country, levels = c("USA", "China")))

formula_interaction <- high_burden_10 ~ ns(age, df = 4) + country + 
  gender + rural + educ_cat + income_quartile +
  chronic_cat * insurance_any * country

model_interaction <- glm(formula_interaction, 
                         data = burden_data_china_usa, 
                         family = binomial(link = "logit"))

summary(model_interaction)

coef_interaction <- tidy(model_interaction, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(model = "Interaction_Chronic_Insurance_ChinaUSA") %>%
  rename(OR = estimate, OR_lower = conf.low, OR_upper = conf.high)

write_csv(coef_interaction, "./output/model_interaction_coefficients_china_usa.csv")

cat("Interaction model fitted\n")

################################################################################
## Predicted probabilities: Age effects
################################################################################
cat("\n========== Predicted probabilities: Age effects ==========\n")

age_seq <- seq(50, 90, by = 1)

country_avg <- burden_data %>%
  group_by(country) %>%
  summarise(
    prop_female = mean(gender, na.rm = TRUE),
    prop_rural = mean(rural, na.rm = TRUE),
    prop_insured = mean(insurance_any, na.rm = TRUE),
    .groups = 'drop'
  )

pred_data_list <- list()

for (cty in c("China", "Europe", "USA")) {
  avg_features <- country_avg %>% filter(country == cty)
  
  pred_data_list[[cty]] <- data.frame(
    age = age_seq,
    country = cty,
    gender = avg_features$prop_female,
    rural = avg_features$prop_rural,
    insurance_any = avg_features$prop_insured,
    age_centered = age_seq - 65
  )
  
  chronic_dist <- burden_data %>%
    filter(country == cty) %>%
    count(chronic_cat) %>%
    mutate(prop = n / sum(n))
  
  most_common_chronic <- chronic_dist %>% 
    arrange(desc(prop)) %>% 
    slice(1) %>% 
    pull(chronic_cat)
  
  pred_data_list[[cty]]$chronic_cat <- as.character(most_common_chronic)
  pred_data_list[[cty]]$educ_cat <- "Medium"
  pred_data_list[[cty]]$income_quartile <- "Q2"
}

pred_data <- bind_rows(pred_data_list) %>%
  mutate(
    country = factor(country, levels = c("China", "Europe", "USA")),
    chronic_cat = factor(as.character(chronic_cat), levels = c("0", "1", "2", "3", "4+")),
    educ_cat = factor(as.character(educ_cat), levels = c("Low", "Medium", "High")),
    income_quartile = factor(as.character(income_quartile), levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
  )

pred_data$prob_burden10 <- predict(model_burden10, newdata = pred_data, type = "response")

pred_matrix <- predict(model_burden10, 
                      newdata = pred_data, 
                      type = "link", 
                      se.fit = TRUE)

pred_data$prob_lower <- plogis(pred_matrix$fit - 1.96 * pred_matrix$se.fit)
pred_data$prob_upper <- plogis(pred_matrix$fit + 1.96 * pred_matrix$se.fit)

write_csv(pred_data, "./output/predicted_burden_by_age.csv")

cat("Age effects predicted\n")

################################################################################
## Predicted probabilities: Chronic conditions
################################################################################
cat("\n========== Predicted probabilities: Chronic conditions ==========\n")

overall_avg <- burden_data %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    prop_female = mean(gender, na.rm = TRUE),
    prop_rural = mean(rural, na.rm = TRUE),
    prop_insured = mean(insurance_any, na.rm = TRUE)
  )

chronic_pred_data <- expand.grid(
  age = overall_avg$avg_age,
  country = c("China", "Europe", "USA"),
  chronic_cat = c("0", "1", "2", "3", "4+"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    gender = overall_avg$prop_female,
    rural = overall_avg$prop_rural,
    educ_cat = "Medium",
    insurance_any = overall_avg$prop_insured,
    income_quartile = "Q2",
    age_centered = age - 65,
    country = factor(country, levels = c("China", "Europe", "USA")),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "2", "3", "4+")),
    educ_cat = factor(educ_cat, levels = c("Low", "Medium", "High")),
    income_quartile = factor(income_quartile, levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
  )

chronic_pred_data$prob_burden10 <- predict(model_burden10, 
                                           newdata = chronic_pred_data, 
                                           type = "response")

pred_matrix_chronic <- predict(model_burden10, 
                               newdata = chronic_pred_data, 
                               type = "link", 
                               se.fit = TRUE)

chronic_pred_data$prob_lower <- plogis(pred_matrix_chronic$fit - 1.96 * pred_matrix_chronic$se.fit)
chronic_pred_data$prob_upper <- plogis(pred_matrix_chronic$fit + 1.96 * pred_matrix_chronic$se.fit)

write_csv(chronic_pred_data, "./output/predicted_burden_by_chronic.csv")

cat("Chronic conditions effects predicted\n")

################################################################################
## Predicted probabilities: Insurance protection
################################################################################
cat("\n========== Predicted probabilities: Insurance protection ==========\n")

overall_avg_ins <- burden_data %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    prop_female = mean(gender, na.rm = TRUE),
    prop_rural = mean(rural, na.rm = TRUE)
  )

insurance_pred_data <- expand.grid(
  age = overall_avg_ins$avg_age,
  country = c("China", "USA"),
  chronic_cat = c("0", "1", "2", "3", "4+"),
  insurance_any = c(0, 1),
  stringsAsFactors = FALSE
) %>%
  mutate(
    gender = overall_avg_ins$prop_female,
    rural = overall_avg_ins$prop_rural,
    educ_cat = "Medium",
    income_quartile = "Q2",
    age_centered = age - 65,
    country = factor(country, levels = c("USA", "China", "Europe")),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "2", "3", "4+")),
    educ_cat = factor(educ_cat, levels = c("Low", "Medium", "High")),
    income_quartile = factor(income_quartile, levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    insurance_label = ifelse(insurance_any == 1, "With Insurance", "Without Insurance")
  )

insurance_pred_data$prob_burden10 <- predict(model_burden10, 
                                             newdata = insurance_pred_data, 
                                             type = "response")

pred_matrix_ins <- predict(model_burden10, 
                           newdata = insurance_pred_data, 
                           type = "link", 
                           se.fit = TRUE)

insurance_pred_data$prob_lower <- plogis(pred_matrix_ins$fit - 1.96 * pred_matrix_ins$se.fit)
insurance_pred_data$prob_upper <- plogis(pred_matrix_ins$fit + 1.96 * pred_matrix_ins$se.fit)

write_csv(insurance_pred_data, "./output/predicted_burden_by_insurance.csv")

cat("Insurance protection effects predicted\n")

################################################################################
## Save models
################################################################################
save(model_burden10, model_interaction,
     file = "./output/burden_models.RData")

cat("\n========== Burden analysis completed ==========\n")
