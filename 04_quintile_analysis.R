################################################################################
## 04_quintile_analysis.R
## 
## Purpose: Income and chronic condition quintile analysis
## 
## Outputs: High burden rates by income quintiles and chronic conditions
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
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
## Data preparation
################################################################################

analysis_data <- pooled_data %>%
  filter(!is.na(age) & age >= 50 & age <= 90) %>%
  filter(!is.na(high_burden_10) & !is.na(hh_income_intl)) %>%
  filter(hh_income_intl > 0) %>%
  mutate(
    income_quintile = ntile(hh_income_intl, 5),
    income_quintile_label = factor(
      income_quintile,
      levels = 1:5,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)")
    ),
    chronic_cat = case_when(
      n_chronic == 0 ~ "0",
      n_chronic == 1 ~ "1",
      n_chronic == 2 ~ "2",
      n_chronic == 3 ~ "3",
      n_chronic >= 4 ~ "4+",
      TRUE ~ NA_character_
    ),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "2", "3", "4+"))
  )

cat("Analysis sample size:", nrow(analysis_data), "\n")

################################################################################
## Income quintile analysis
################################################################################

cat("\n========== Income quintile analysis ==========\n")

income_stats <- analysis_data %>%
  group_by(country, income_quintile_label) %>%
  summarise(
    n = n(),
    n_burden = sum(high_burden_10, na.rm = TRUE),
    burden_rate = mean(high_burden_10, na.rm = TRUE),
    se = sqrt(burden_rate * (1 - burden_rate) / n),
    ci_lower = pmax(0, burden_rate - 1.96 * se),
    ci_upper = pmin(1, burden_rate + 1.96 * se),
    .groups = 'drop'
  )

print(income_stats)

# Significance test (Q1 vs Q5)
cat("\nIncome quintile comparison (Q1 vs Q5):\n")

for (ctry in c("China", "Europe", "USA")) {
  data_q1 <- analysis_data %>% 
    filter(country == ctry & income_quintile == 1)
  data_q5 <- analysis_data %>% 
    filter(country == ctry & income_quintile == 5)
  
  if (nrow(data_q1) > 0 & nrow(data_q5) > 0) {
    test_result <- prop.test(
      x = c(sum(data_q1$high_burden_10, na.rm = TRUE), 
            sum(data_q5$high_burden_10, na.rm = TRUE)),
      n = c(nrow(data_q1), nrow(data_q5))
    )
    
    cat(sprintf("  %s: Q1=%.1f%%, Q5=%.1f%%, p=%.4f\n",
                ctry,
                mean(data_q1$high_burden_10, na.rm = TRUE) * 100,
                mean(data_q5$high_burden_10, na.rm = TRUE) * 100,
                test_result$p.value))
  }
}

write_csv(income_stats, "./output/income_quintile_burden_stats.csv")

################################################################################
## Chronic condition analysis
################################################################################

cat("\n========== Chronic condition analysis ==========\n")

chronic_stats <- analysis_data %>%
  filter(!is.na(chronic_cat)) %>%
  group_by(country, chronic_cat) %>%
  summarise(
    n = n(),
    n_burden = sum(high_burden_10, na.rm = TRUE),
    burden_rate = mean(high_burden_10, na.rm = TRUE),
    se = sqrt(burden_rate * (1 - burden_rate) / n),
    ci_lower = pmax(0, burden_rate - 1.96 * se),
    ci_upper = pmin(1, burden_rate + 1.96 * se),
    .groups = 'drop'
  )

print(chronic_stats)

# Significance test (0 vs 4+)
cat("\nChronic condition comparison (0 vs 4+):\n")

for (ctry in c("China", "Europe", "USA")) {
  data_0 <- analysis_data %>% 
    filter(country == ctry & chronic_cat == "0")
  data_4plus <- analysis_data %>% 
    filter(country == ctry & chronic_cat == "4+")
  
  if (nrow(data_0) > 0 & nrow(data_4plus) > 0) {
    test_result <- prop.test(
      x = c(sum(data_0$high_burden_10, na.rm = TRUE), 
            sum(data_4plus$high_burden_10, na.rm = TRUE)),
      n = c(nrow(data_0), nrow(data_4plus))
    )
    
    cat(sprintf("  %s: 0=%.1f%%, 4+=%.1f%%, p=%.4f\n",
                ctry,
                mean(data_0$high_burden_10, na.rm = TRUE) * 100,
                mean(data_4plus$high_burden_10, na.rm = TRUE) * 100,
                test_result$p.value))
  }
}

write_csv(chronic_stats, "./output/chronic_count_burden_stats.csv")

################################################################################
## Trend tests
################################################################################

cat("\n========== Trend tests ==========\n")

# Income quintile trend
cat("\nIncome quintile trend test:\n")

for (ctry in c("China", "Europe", "USA")) {
  data_trend <- analysis_data %>%
    filter(country == ctry) %>%
    group_by(income_quintile) %>%
    summarise(
      n_total = n(),
      n_burden = sum(high_burden_10, na.rm = TRUE)
    )
  
  if (nrow(data_trend) >= 3) {
    trend_test <- prop.trend.test(
      x = data_trend$n_burden,
      n = data_trend$n_total
    )
    
    cat(sprintf("  %s: Chi-square=%.2f, p=%.4f\n",
                ctry,
                trend_test$statistic,
                trend_test$p.value))
  }
}

# Chronic condition trend
cat("\nChronic condition trend test:\n")

for (ctry in c("China", "Europe", "USA")) {
  data_trend <- analysis_data %>%
    filter(country == ctry & !is.na(chronic_cat)) %>%
    mutate(chronic_num = as.numeric(chronic_cat)) %>%
    group_by(chronic_num) %>%
    summarise(
      n_total = n(),
      n_burden = sum(high_burden_10, na.rm = TRUE)
    )
  
  if (nrow(data_trend) >= 3) {
    trend_test <- prop.trend.test(
      x = data_trend$n_burden,
      n = data_trend$n_total
    )
    
    cat(sprintf("  %s: Chi-square=%.2f, p=%.4f\n",
                ctry,
                trend_test$statistic,
                trend_test$p.value))
  }
}

cat("\n========== Quintile analysis completed ==========\n")
