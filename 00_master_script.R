################################################################################
## 00_master_script.R
## 
## Purpose: Master script to run all analyses
## 
## This script executes all analysis scripts in sequence
################################################################################

rm(list = ls())
gc()

cat("\n")
cat("================================================================================\n")
cat("||                                                                            ||\n")
cat("||     Financial Burden of Healthcare in Aging Populations                   ||\n")
cat("||     China, Europe, and USA Comparative Analysis                           ||\n")
cat("||                                                                            ||\n")
cat("================================================================================\n")
cat("\n")

################################################################################
## Create output directories
################################################################################

cat("\n========== Creating output directories ==========\n")

output_dirs <- c(
  "./output"
)

for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  } else {
    cat("Directory exists:", dir, "\n")
  }
}

################################################################################
## Record start time
################################################################################

start_time_total <- Sys.time()

################################################################################
## Step 1: Data cleaning and harmonization
################################################################################

cat("\n========== Step 1: Data cleaning and harmonization ==========\n")
cat("Output: harmonized_data_corrected.RData\n\n")

tryCatch({
  source("code/01_data_cleaning.R", encoding = "UTF-8")
  cat("\n✓ Step 1 completed\n")
}, error = function(e) {
  cat("\n✗ Step 1 failed:", e$message, "\n")
  stop("Data cleaning failed, stopping pipeline")
})

################################################################################
## Step 2: APC analysis
################################################################################

cat("\n========== Step 2: APC analysis ==========\n")
cat("Output: APC effects for overall and subgroups\n\n")

tryCatch({
  source("code/02_apc_analysis.R", encoding = "UTF-8")
  cat("\n✓ Step 2 completed\n")
}, error = function(e) {
  cat("\n✗ Step 2 failed:", e$message, "\n")
  cat("Continuing with next steps...\n")
})

################################################################################
## Step 3: Financial burden analysis
################################################################################

cat("\n========== Step 3: Financial burden analysis ==========\n")
cat("Output: Logistic regression models and predictions\n\n")

tryCatch({
  source("code/03_burden_analysis.R", encoding = "UTF-8")
  cat("\n✓ Step 3 completed\n")
}, error = function(e) {
  cat("\n✗ Step 3 failed:", e$message, "\n")
  cat("Continuing with next steps...\n")
})

################################################################################
## Step 4: Income and chronic condition quintile analysis
################################################################################

cat("\n========== Step 4: Quintile analysis ==========\n")
cat("Output: High burden rates by quintiles\n\n")

tryCatch({
  source("code/04_quintile_analysis.R", encoding = "UTF-8")
  cat("\n✓ Step 4 completed\n")
}, error = function(e) {
  cat("\n✗ Step 4 failed:", e$message, "\n")
  cat("Continuing with next steps...\n")
})

################################################################################
## Step 5: Transition analysis
################################################################################

cat("\n========== Step 5: Transition analysis ==========\n")
cat("Output: Transition probabilities and odds ratios\n\n")

tryCatch({
  source("code/05_transition_analysis.R", encoding = "UTF-8")
  cat("\n✓ Step 5 completed\n")
}, error = function(e) {
  cat("\n✗ Step 5 failed:", e$message, "\n")
  cat("Continuing with next steps...\n")
})

################################################################################
## Generate analysis report
################################################################################

cat("\n========== Generating analysis report ==========\n")

if (exists("start_time_total")) {
  total_time <- Sys.time() - start_time_total
  time_str <- paste0("Total time: ", round(difftime(Sys.time(), start_time_total, units = "mins"), 2), " minutes\n")
} else {
  time_str <- "Total time: Not recorded\n"
}

report <- paste0(
  "\n",
  "================================================================================\n",
  "||                          Analysis Report                                   ||\n",
  "================================================================================\n",
  "\n",
  time_str,
  "\n",
  "Output structure:\n",
  "  ./output/\n",
  "    ├── harmonized_data_corrected.RData\n",
  "    ├── apc_effects_*.csv\n",
  "    ├── model_*_coefficients.csv\n",
  "    ├── predicted_burden_*.csv\n",
  "    ├── income_quintile_burden_stats.csv\n",
  "    ├── chronic_count_burden_stats.csv\n",
  "    ├── transition_probabilities.csv\n",
  "    └── transition_odds_ratios.csv\n",
  "\n",
  "Main outputs:\n",
  "  ✓ Data harmonization: CHARLS, SHARE, HRS\n",
  "  ✓ APC analysis: Overall and subgroups\n",
  "  ✓ Financial burden analysis: Logistic regression\n",
  "  ✓ Quintile analysis: Income and chronic conditions\n",
  "  ✓ Transition analysis: Dynamic patterns\n",
  "\n",
  "Data quality:\n",
  "  - Baseline sample: CHARLS 17,708, SHARE 37,132, HRS 19,578\n",
  "  - Total observations: ~489,404 person-times\n",
  "  - Average follow-up: CHARLS 7.0 years, SHARE 7.4 years, HRS 10.2 years\n",
  "\n",
  "Analysis standards:\n",
  "  - High burden threshold: 10% (WHO standard)\n",
  "  - Confidence interval: 95% CI\n",
  "  - Significance level: p<0.05\n",
  "\n",
  "================================================================================\n",
  "||                    All analyses completed!                                 ||\n",
  "================================================================================\n",
  "\n"
)

cat(report)

writeLines(report, "./output/analysis_report.txt")

cat("Analysis report saved to: ./output/analysis_report.txt\n")
cat("\n✓ All analyses completed successfully!\n\n")
