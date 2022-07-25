#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates);
library(plm); library(WDI); library(lmtest); library(sandwich); library(interactions); library(rmarkdown)
#--------------------------------------------------------------------------------------
# Load Data
#--------------------------------------------------------------------------------------
regression_ready_df <- readRDS("~/My Drive/3 - Misc. Data Research/Private-Welfare-Project/regression_ready_df.rds")
#--------------------------------------------------------------------------------------
regression_subset_df <- regression_ready_df %>%
  select(iso3,
         year,
         oecd_total_private_socspend_full_pop_pct_gdp,
         pwt_marx_rop,
         pwt_irr,
         CPDS_unemployment_rate,
         CPDS_inflation_rate,
         pwt_pct_gdp_international_trade,
         WID_pct_gdp_govt_subsidies,
         GDD_pct_gdp_central_govt_debt,
         cabinet_majority_right,
         cabinet_majority_left,
         legislature_majority_right,
         legislature_majority_left,
         CPDS_legis_seat_share_right_party,
         CPDS_legis_seat_share_left_party,
         CPDS_pct_govt_cabinet_right_party,
         CPDS_pct_govt_cabinet_left_party) %>%
  rename(legislature_pct_right = CPDS_legis_seat_share_right_party,
         legislature_pct_left = CPDS_legis_seat_share_left_party,
         cabinet_pct_right = CPDS_pct_govt_cabinet_right_party,
         cabinet_pct_left = CPDS_pct_govt_cabinet_left_party,
         pct_gdp_private_welfare = oecd_total_private_socspend_full_pop_pct_gdp,
         marx_rop = pwt_marx_rop,
         non_marx_irr = pwt_irr,
         unemployment = CPDS_unemployment_rate,
         inflation = CPDS_inflation_rate,
         pct_gdp_trade = pwt_pct_gdp_international_trade,
         pct_gdp_subsidies = WID_pct_gdp_govt_subsidies,
         pct_gdp_govt_debt = GDD_pct_gdp_central_govt_debt) %>%
  select('iso3', 'year', sort(colnames(.))) %>%
  na.omit()
#--------------------------------------------------------------------------------------
write_rds(regression_subset_df, "regression_subset_df.rds")
write_csv(regression_subset_df, "regression_subset_df.csv")
#--------------------------------------------------------------------------------------












