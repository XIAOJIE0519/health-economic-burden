################################################################################
## 01_data_cleaning.R
## 
## Purpose: Data harmonization and standardization for CHARLS, SHARE, and HRS
## 
## Outputs: harmonized_data_corrected.RData
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(haven)
  library(tibble)
})

cat("\n========== Data Sources ==========\n")
cat("1. CHARLS: 2011-2018 (waves 1-4)\n")
cat("2. SHARE: 2004-2017 (waves 1,2,3,4,5,6,7)\n")
cat("3. HRS: 2000-2020 (waves 5-15)\n\n")

################################################################################
## Load raw data
################################################################################

load("./RData/share.Rdata")
load("./RData/hrs.Rdata")
load("./RData/charls.Rdata")

charls <- zap_labels(charls)
share  <- zap_labels(share)
hrs    <- zap_labels(hrs)

################################################################################
## Utility functions
################################################################################

col_or_na <- function(df, var, default = NA) {
  if (!is.na(var) && var %in% names(df)) df[[var]] else rep(default, nrow(df))
}

to_num <- function(x) suppressWarnings(as.numeric(x))

naify_special <- function(x) {
  x <- to_num(x)
  x[x %in% c(-1,-2,-3,-7,-8,-9, 7,8,9, 97,98,99, 999,9999,99999,999999)] <- NA
  x
}

clean_year <- function(x) {
  x <- naify_special(x)
  x[x < 1800 | x > 2100] <- NA
  as.integer(round(x))
}

clean_age <- function(x) {
  x <- naify_special(x)
  x[x < 0 | x > 120] <- NA
  x
}

clean_spend <- function(x) {
  x <- naify_special(x)
  x[x < 0] <- NA
  x
}

clean_income <- function(x) {
  x <- naify_special(x)
  x[x < 0] <- NA
  x
}

clean_binary <- function(x) {
  x <- naify_special(x)
  x[!(x %in% c(0, 1))] <- NA
  x
}

################################################################################
## PPP and CPI data
################################################################################

ppp_rates <- tibble(
  country = c("China", "Europe", "USA"),
  ppp_2017 = c(3.506, 0.844, 1.0)
)

cpi_index <- tibble(
  year = 2000:2021,
  cpi_china = c(
    68.4, 69.2, 68.9, 70.5, 73.8, 75.3, 76.8, 81.5, 87.4, 86.6,
    90.3, 95.5, 98.6, 101.5, 103.6, 105.0, 107.1, 100.0, 102.1, 104.9, 107.4, 108.4
  ),
  cpi_europe = c(
    82.1, 84.2, 86.3, 88.4, 90.5, 92.5, 94.3, 96.2, 100.2, 100.5,
    102.5, 105.8, 108.6, 110.0, 110.4, 110.4, 110.6, 100.0, 101.8, 103.1, 103.4, 106.2
  ),
  cpi_usa = c(
    82.4, 84.7, 86.1, 88.3, 90.9, 94.0, 97.3, 100.0, 103.8, 103.2,
    105.2, 108.6, 110.8, 112.3, 113.7, 113.8, 115.3, 100.0, 102.4, 104.2, 105.6, 110.2
  )
)

################################################################################
## CHARLS data processing
################################################################################

cat("\n========== Processing CHARLS data ==========\n")

charls_clean <- tibble(
  id      = as.character(col_or_na(charls, "ID", NA_character_)),
  wave    = as.integer(to_num(col_or_na(charls, "wave"))),
  country = "China",
  
  age     = clean_age(col_or_na(charls, "age")),
  cohort  = clean_year(col_or_na(charls, "rabyear")),
  period  = clean_year(col_or_na(charls, "iwy")),
  
  gender  = clean_binary(col_or_na(charls, "ragender")),
  rural   = clean_binary(col_or_na(charls, "hrural")),
  educ    = clean_age(col_or_na(charls, "raeducl")),
  
  ever_smoke = clean_binary(col_or_na(charls, "smokev")),
  curr_smoke = clean_binary(col_or_na(charls, "smoken")),
  
  hibpe   = clean_binary(col_or_na(charls, "hibpe")),
  diabe   = clean_binary(col_or_na(charls, "diabe")),
  cancre  = clean_binary(col_or_na(charls, "cancre")),
  lunge   = clean_binary(col_or_na(charls, "lunge")),
  hearte  = clean_binary(col_or_na(charls, "hearte")),
  stroke  = clean_binary(col_or_na(charls, "stroke")),
  psyche  = clean_binary(col_or_na(charls, "psyche")),
  arthre  = clean_binary(col_or_na(charls, "arthre")),
  
  tot_outpatient_1m = clean_spend(col_or_na(charls, "totdoc1m")),
  tot_inpatient_1y  = clean_spend(col_or_na(charls, "tothos1y")),
  
  any_insurance = clean_binary(col_or_na(charls, "ins")),
  ins_employee  = clean_binary(col_or_na(charls, "ea001s1")),
  ins_resident  = clean_binary(col_or_na(charls, "ea001s2")),
  ins_nrcms     = clean_binary(col_or_na(charls, "ea001s3")),
  ins_public    = clean_binary(col_or_na(charls, "ea001s5")),
  ins_private   = clean_binary(col_or_na(charls, "ea001s11")),
  
  hh_income_raw = clean_income(col_or_na(charls, "income_total"))
) %>%
  filter(is.na(wave) | wave != 5) %>%
  mutate(
    year = case_when(
      wave == 1 ~ 2011,
      wave == 2 ~ 2013,
      wave == 3 ~ 2015,
      wave == 4 ~ 2018,
      TRUE ~ NA_real_
    ),
    oop_annual_raw = rowSums(cbind(tot_outpatient_1m * 12, tot_inpatient_1y), na.rm = TRUE)
  )

cat("CHARLS cleaned:", nrow(charls_clean), "rows\n")

################################################################################
## SHARE data processing
################################################################################

cat("\n========== Processing SHARE data ==========\n")

share_clean <- tibble(
  id      = as.character(col_or_na(share, "mergeid", NA_character_)),
  wave    = as.integer(to_num(col_or_na(share, "wave"))),
  country = "Europe",
  
  age     = clean_age(col_or_na(share, "agey")),
  cohort  = clean_year(col_or_na(share, "rabyear")),
  period  = clean_year(col_or_na(share, "iwy")),
  
  gender  = clean_binary(col_or_na(share, "ragender")),
  rural   = clean_binary(col_or_na(share, "rural")),
  educ    = clean_age(col_or_na(share, "raeducl")),
  
  ever_smoke = clean_binary(col_or_na(share, "smokev")),
  curr_smoke = clean_binary(col_or_na(share, "smoken")),
  
  hibpe   = clean_binary(col_or_na(share, "hibpe")),
  diabe   = clean_binary(col_or_na(share, "diabe")),
  cancre  = clean_binary(col_or_na(share, "cancre")),
  lunge   = clean_binary(col_or_na(share, "lunge")),
  hearte  = clean_binary(col_or_na(share, "hearte")),
  stroke  = clean_binary(col_or_na(share, "stroke")),
  psyche  = clean_binary(col_or_na(share, "psyche")),
  arthre  = clean_binary(col_or_na(share, "arthre")),
  
  oop_outpatient_raw = clean_spend(col_or_na(share, "oopdoc1y")),
  oop_drugs_raw      = clean_spend(col_or_na(share, "oopdrug1y")),
  oop_inpatient_raw  = clean_spend(col_or_na(share, "oophos1y")),
  oop_other_raw      = clean_spend(col_or_na(share, "oopmd1y")),
  
  hh_income_raw = clean_income(col_or_na(share, "hhitothhinc"))
) %>%
  filter(is.na(wave) | wave != 8) %>%
  mutate(
    year = case_when(
      wave == 1 ~ 2004,
      wave == 2 ~ 2006,
      wave == 3 ~ 2008,
      wave == 4 ~ 2011,
      wave == 5 ~ 2013,
      wave == 6 ~ 2015,
      wave == 7 ~ 2017,
      TRUE ~ NA_real_
    ),
    oop_annual_raw = rowSums(cbind(oop_outpatient_raw, oop_drugs_raw, 
                                    oop_inpatient_raw, oop_other_raw), na.rm = TRUE)
  )

cat("SHARE cleaned:", nrow(share_clean), "rows\n")

################################################################################
## HRS data processing
################################################################################

cat("\n========== Processing HRS data ==========\n")

hrs_clean <- tibble(
  id      = as.character(col_or_na(hrs, "hhidpn", NA_character_)),
  wave    = as.integer(to_num(col_or_na(hrs, "wave"))),
  country = "USA",
  
  age     = clean_age(col_or_na(hrs, "ragey_m")),
  cohort  = clean_year(col_or_na(hrs, "rabyear")),
  period  = clean_year(col_or_na(hrs, "iwendy")),
  
  gender  = clean_binary(col_or_na(hrs, "ragender")),
  rural   = clean_binary(col_or_na(hrs, "rural")),
  educ    = clean_age(col_or_na(hrs, "raeducl")),
  
  ever_smoke = clean_binary(col_or_na(hrs, "smokev")),
  curr_smoke = clean_binary(col_or_na(hrs, "smoken")),
  
  hibpe   = clean_binary(col_or_na(hrs, "hibpe")),
  diabe   = clean_binary(col_or_na(hrs, "diabe")),
  cancre  = clean_binary(col_or_na(hrs, "cancre")),
  lunge   = clean_binary(col_or_na(hrs, "lunge")),
  hearte  = clean_binary(col_or_na(hrs, "hearte")),
  stroke  = clean_binary(col_or_na(hrs, "stroke")),
  psyche  = clean_binary(col_or_na(hrs, "psyche")),
  arthre  = clean_binary(col_or_na(hrs, "arthre")),
  
  oop_2year_raw = clean_spend(col_or_na(hrs, "oopmd")),
  
  any_coverage = clean_binary(col_or_na(hrs, "covr")),
  medicare = clean_binary(col_or_na(hrs, "govmr")),
  medicaid = clean_binary(col_or_na(hrs, "govmd")),
  va_insurance = clean_binary(col_or_na(hrs, "govva")),
  private_count = clean_age(col_or_na(hrs, "prpcnt")),
  
  indiv_income_raw = clean_income(col_or_na(hrs, "iearn")),
  couple_income_raw = clean_income(col_or_na(hrs, "itot"))
) %>%
  mutate(
    year = case_when(
      wave == 5  ~ 2000, wave == 6  ~ 2002, wave == 7  ~ 2004,
      wave == 8  ~ 2006, wave == 9  ~ 2008, wave == 10 ~ 2010,
      wave == 11 ~ 2012, wave == 12 ~ 2014, wave == 13 ~ 2016,
      wave == 14 ~ 2018, wave == 15 ~ 2020,
      TRUE ~ NA_real_
    ),
    oop_annual_raw = oop_2year_raw / 2
  )

cat("HRS cleaned:", nrow(hrs_clean), "rows\n")

################################################################################
## Inflation adjustment and PPP conversion
################################################################################

cat("\n========== Inflation adjustment and PPP conversion ==========\n")

charls_clean <- charls_clean %>%
  left_join(cpi_index %>% select(year, cpi_china), by = "year") %>%
  mutate(
    oop_annual_2017 = oop_annual_raw * (100 / cpi_china),
    hh_income_2017  = hh_income_raw * (100 / cpi_china),
    oop_annual_intl = oop_annual_2017 / 3.506,
    hh_income_intl  = hh_income_2017 / 3.506
  ) %>%
  select(-cpi_china)

share_clean <- share_clean %>%
  left_join(cpi_index %>% select(year, cpi_europe), by = "year") %>%
  mutate(
    oop_annual_2017 = oop_annual_raw * (100 / cpi_europe),
    hh_income_2017  = hh_income_raw * (100 / cpi_europe),
    oop_annual_intl = oop_annual_2017 / 0.844,
    hh_income_intl  = hh_income_2017 / 0.844
  ) %>%
  select(-cpi_europe)

hrs_clean <- hrs_clean %>%
  left_join(cpi_index %>% select(year, cpi_usa), by = "year") %>%
  mutate(
    oop_annual_2017 = oop_annual_raw * (100 / cpi_usa),
    hh_income_2017  = ifelse(!is.na(couple_income_raw), 
                             couple_income_raw * (100 / cpi_usa),
                             indiv_income_raw * (100 / cpi_usa)),
    oop_annual_intl = oop_annual_2017 / 1.0,
    hh_income_intl  = hh_income_2017 / 1.0
  ) %>%
  select(-cpi_usa)

################################################################################
## Derive variables
################################################################################

cat("\n========== Deriving variables ==========\n")

add_derived_vars <- function(df, has_insurance = TRUE) {
  df <- df %>%
    mutate(
      n_chronic = rowSums(cbind(hibpe, diabe, cancre, lunge, 
                                hearte, stroke, psyche, arthre), na.rm = TRUE),
      multimorbidity = as.integer(n_chronic >= 2),
      chronic_cat = case_when(
        n_chronic == 0 ~ "0",
        n_chronic == 1 ~ "1",
        n_chronic == 2 ~ "2",
        n_chronic >= 3 ~ "3+",
        TRUE ~ NA_character_
      ),
      burden_ratio = ifelse(hh_income_intl > 0, 
                            oop_annual_intl / hh_income_intl, 
                            NA_real_),
      high_burden_10 = as.integer(burden_ratio >= 0.10),
      log_oop = log1p(oop_annual_intl),
      log_income = log1p(hh_income_intl)
    )
  
  if (has_insurance) {
    if ("any_insurance" %in% names(df)) {
      df <- df %>%
        mutate(
          insurance_any = any_insurance,
          insurance_public = pmax(ins_employee, ins_resident, ins_nrcms, ins_public, na.rm = TRUE),
          insurance_private = ins_private
        )
    } else if ("any_coverage" %in% names(df)) {
      df <- df %>%
        mutate(
          insurance_any = any_coverage,
          insurance_public = pmax(medicare, medicaid, va_insurance, na.rm = TRUE),
          insurance_private = as.integer(private_count > 0)
        )
    }
  }
  
  return(df)
}

charls_clean <- add_derived_vars(charls_clean, has_insurance = TRUE)
share_clean <- add_derived_vars(share_clean, has_insurance = FALSE)
hrs_clean <- add_derived_vars(hrs_clean, has_insurance = TRUE)

################################################################################
## Merge datasets
################################################################################

cat("\n========== Merging datasets ==========\n")

required_cols <- c("id", "wave", "country", "year", "age", "cohort", "period",
                   "gender", "rural", "educ", "ever_smoke", "curr_smoke",
                   "hibpe", "diabe", "cancre", "lunge", "hearte", "stroke", "psyche", "arthre",
                   "n_chronic", "multimorbidity", "chronic_cat",
                   "oop_annual_raw", "oop_annual_2017", "oop_annual_intl",
                   "hh_income_raw", "hh_income_2017", "hh_income_intl",
                   "burden_ratio", "high_burden_10", "log_oop", "log_income")

harmonize_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df %>% select(any_of(cols), everything())
}

charls_harmonized <- harmonize_cols(charls_clean, required_cols)
share_harmonized <- harmonize_cols(share_clean, required_cols)
hrs_harmonized <- harmonize_cols(hrs_clean, required_cols)

################################################################################
## Construct baseline cohorts
################################################################################

cat("\n========== Constructing baseline cohorts ==========\n")

charls_baseline_ids <- charls_harmonized %>%
  filter(wave == 1) %>%
  pull(id) %>%
  unique()

charls_cohort <- charls_harmonized %>%
  filter(id %in% charls_baseline_ids)

cat("CHARLS baseline sample:", length(charls_baseline_ids), "individuals\n")
cat("CHARLS cohort total observations:", nrow(charls_cohort), "rows\n")

share_baseline_ids <- share_harmonized %>%
  filter(wave == 2) %>%
  pull(id) %>%
  unique()

share_cohort <- share_harmonized %>%
  filter(id %in% share_baseline_ids)

cat("SHARE baseline sample:", length(share_baseline_ids), "individuals\n")
cat("SHARE cohort total observations:", nrow(share_cohort), "rows\n")

hrs_baseline_ids <- hrs_harmonized %>%
  filter(wave == 5) %>%
  pull(id) %>%
  unique()

hrs_cohort <- hrs_harmonized %>%
  filter(id %in% hrs_baseline_ids)

cat("HRS baseline sample:", length(hrs_baseline_ids), "individuals\n")
cat("HRS cohort total observations:", nrow(hrs_cohort), "rows\n")

pooled_data <- bind_rows(charls_cohort, share_cohort, hrs_cohort)

cat("\nTotal pooled sample:", nrow(pooled_data), "rows\n")

################################################################################
## Save data
################################################################################

dir.create("./output", showWarnings = FALSE, recursive = TRUE)

write_csv(pooled_data, "./output/pooled_harmonized_data_corrected.csv")
save(pooled_data, charls_harmonized, share_harmonized, hrs_harmonized,
     file = "./output/harmonized_data_corrected.RData")

cat("\nData saved to ./output/harmonized_data_corrected.RData\n")

################################################################################
## Descriptive statistics
################################################################################

desc_stats <- pooled_data %>%
  filter(!is.na(oop_annual_intl) & !is.na(hh_income_intl)) %>%
  group_by(country) %>%
  summarise(
    n_observations = n(),
    n_individuals = n_distinct(id),
    age_mean = mean(age, na.rm = TRUE),
    oop_mean = mean(oop_annual_intl, na.rm = TRUE),
    oop_median = median(oop_annual_intl, na.rm = TRUE),
    high_burden_10_pct = mean(high_burden_10, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(desc_stats)
write_csv(desc_stats, "./output/descriptive_statistics.csv")

cat("\n========== Data cleaning completed ==========\n")
