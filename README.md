# Data Access Instructions

## Overview

This study uses data from three publicly available longitudinal surveys. Due to data use agreements, we cannot share the raw data directly. Researchers must obtain data access independently from each source.

## Data Sources

### 1. China Health and Retirement Longitudinal Study (CHARLS)

**Website**: http://charls.pku.edu.cn/

**Coverage**: 2011-2018 (Waves 1-4)

**Access Process**:
1. Visit the CHARLS website
2. Register for an account
3. Complete the data use agreement
4. Download the harmonized dataset

**Required Files**:
- Harmonized CHARLS dataset (all waves)
- Variable codebook

**Expected Filename**: `charls.Rdata`

---

### 2. Survey of Health, Ageing and Retirement in Europe (SHARE)

**Website**: http://www.share-project.org/

**Coverage**: 2004-2017 (Waves 1-7, excluding Wave 8)

**Access Process**:
1. Visit the SHARE website
2. Register for an account
3. Complete the data use agreement
4. Download the harmonized dataset (easySHARE or generated variables)

**Required Files**:
- SHARE harmonized dataset (Waves 1-7)
- Variable documentation

**Expected Filename**: `share.Rdata`

**Note**: This study uses Waves 1, 2, 3, 4, 5, 6, and 7. Wave 8 is excluded.

---

### 3. Health and Retirement Study (HRS)

**Website**: https://hrs.isr.umich.edu/

**Coverage**: 2000-2020 (Waves 5-15)

**Access Process**:
1. Visit the HRS website
2. Register for an account
3. Complete the data use agreement
4. Download the RAND HRS Longitudinal File (recommended) or core data files

**Required Files**:
- RAND HRS Longitudinal File (preferred)
- Or HRS core data files for Waves 5-15

**Expected Filename**: `hrs.Rdata`

---

## Data Preparation

After obtaining data access, prepare the files as follows:

### Directory Structure
```
project_root/
├── RData/
│   ├── charls.Rdata
│   ├── share.Rdata
│   └── hrs.Rdata
├── code/
│   └── [analysis scripts]
└── output/
    └── [will be created automatically]
```

### Variable Requirements

The analysis scripts expect the following variables in each dataset. If your downloaded data uses different variable names, you may need to rename them or modify the `01_data_cleaning.R` script.

#### CHARLS Variables
- `ID`: Individual identifier
- `wave`: Survey wave
- `age`: Age in years
- `rabyear`: Birth year
- `iwy`: Interview year
- `ragender`: Gender (0=Male, 1=Female)
- `hrural`: Rural residence (0=Urban, 1=Rural)
- `raeducl`: Education level
- `smokev`: Ever smoked
- `hibpe`, `diabe`, `cancre`, `lunge`, `hearte`, `stroke`, `psyche`, `arthre`: Chronic conditions
- `totdoc1m`: Outpatient expenditure (past month)
- `tothos1y`: Inpatient expenditure (past year)
- `ins`: Any insurance
- `income_total`: Household income

#### SHARE Variables
- `mergeid`: Individual identifier
- `wave`: Survey wave
- `agey`: Age in years
- `rabyear`: Birth year
- `iwy`: Interview year
- `ragender`: Gender
- `rural`: Rural residence
- `raeducl`: Education level
- `smokev`: Ever smoked
- Chronic conditions (same as CHARLS)
- `oopdoc1y`, `oopdrug1y`, `oophos1y`, `oopmd1y`: OOP expenditures
- `hhitothhinc`: Household income

#### HRS Variables
- `hhidpn`: Individual identifier
- `wave`: Survey wave
- `ragey_m`: Age in years
- `rabyear`: Birth year
- `iwendy`: Interview year
- `ragender`: Gender
- `rural`: Rural residence
- `raeducl`: Education level
- `smokev`: Ever smoked
- Chronic conditions (same as CHARLS)
- `oopmd`: OOP medical expenditure (2-year)
- `covr`: Any coverage
- `govmr`, `govmd`, `govva`: Public insurance
- `prpcnt`: Private insurance count
- `iearn`: Individual income
- `itot`: Couple income

---

## Data Use Agreements

By using data from these sources, you agree to:

1. **Cite the data sources** in all publications
2. **Not redistribute** the raw data
3. **Use data only for research purposes**
4. **Follow ethical guidelines** for human subjects research
5. **Acknowledge funding sources** of the original surveys

### Recommended Citations

**CHARLS**:
> Zhao Y, Hu Y, Smith JP, Strauss J, Yang G. Cohort profile: the China Health and Retirement Longitudinal Study (CHARLS). Int J Epidemiol. 2014;43(1):61-68.

**SHARE**:
> Börsch-Supan A, et al. Data Resource Profile: the Survey of Health, Ageing and Retirement in Europe (SHARE). Int J Epidemiol. 2013;42(4):992-1001.

**HRS**:
> Sonnega A, et al. Cohort Profile: the Health and Retirement Study (HRS). Int J Epidemiol. 2014;43(2):576-585.

---

## Troubleshooting

### Common Issues

**Issue**: Variable names don't match
- **Solution**: Check the codebook for your downloaded data version. You may need to update variable names in `01_data_cleaning.R`.

**Issue**: Missing waves
- **Solution**: Ensure you downloaded all required waves. The analysis will skip missing waves but may affect results.

**Issue**: File format errors
- **Solution**: Ensure data files are in `.Rdata` format. If you have `.dta` (Stata) or `.sav` (SPSS) files, convert them using:
```r
library(haven)
data <- read_dta("file.dta")  # or read_sav("file.sav")
save(data, file = "file.Rdata")
```

**Issue**: Data access denied
- **Solution**: Contact the respective data provider's support team. Access is typically granted within 1-2 weeks for academic researchers.

---

## Data Quality Checks

After preparing your data files, run the following checks:

```r
# Load data
load("./RData/charls.Rdata")
load("./RData/share.Rdata")
load("./RData/hrs.Rdata")

# Check dimensions
cat("CHARLS:", nrow(charls), "rows,", ncol(charls), "columns\n")
cat("SHARE:", nrow(share), "rows,", ncol(share), "columns\n")
cat("HRS:", nrow(hrs), "rows,", ncol(hrs), "columns\n")

# Check key variables exist
required_vars_charls <- c("ID", "wave", "age", "ragender", "income_total")
cat("CHARLS required vars:", all(required_vars_charls %in% names(charls)), "\n")

required_vars_share <- c("mergeid", "wave", "agey", "ragender", "hhitothhinc")
cat("SHARE required vars:", all(required_vars_share %in% names(share)), "\n")

required_vars_hrs <- c("hhidpn", "wave", "ragey_m", "ragender", "itot")
cat("HRS required vars:", all(required_vars_hrs %in% names(hrs)), "\n")
```

---

## Support

For data access issues:
- **CHARLS**: charls@nsd.pku.edu.cn
- **SHARE**: share@share-eric.eu
- **HRS**: hrsquest@umich.edu

For code-related issues:
- Open an issue on GitHub: https://github.com/XIAOJIE0519/health-economic-burden/issues

---

**Last Updated**: February 26, 2026
