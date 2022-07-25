#-----------------------------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates);
library(plm); library(WDI); library(lmtest); library(sandwich)
#-----------------------------------------------------------------------------------------------
# Load data
#-----------------------------------------------------------------------------------------------
regression_ready_df <- readRDS("~/My Drive/3 - Misc. Data Research/Private-Welfare-Project/regression_ready_df.rds")
#-----------------------------------------------------------------------------------------------
# Subset Data
#-----------------------------------------------------------------------------------------------
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
         cabinet_control_right,
         cabinet_right_over_left,
         cabinet_left_over_right,
         cabinet_left_dominance,
         cabinet_right_center_dominance,
         cabinet_left_hegemony,
         cabinet_right_center_hegemony,
         legislature_majority_right,
         legislature_majority_left,
         legislature_right_over_left,
         legislature_left_over_right,
         CPDS_legis_seat_share_right_party,
         CPDS_pct_govt_cabinet_right_party,
         CPDS_legis_seat_share_left_party,
         CPDS_pct_govt_cabinet_left_party) %>% 
  rename(pct_gdp_private_welfare = oecd_total_private_socspend_full_pop_pct_gdp,
         marx_rop = pwt_marx_rop,
         non_marx_irr = pwt_irr,
         unemployment = CPDS_unemployment_rate,
         inflation = CPDS_inflation_rate,
         pct_gdp_trade = pwt_pct_gdp_international_trade,
         pct_gdp_subsidies = WID_pct_gdp_govt_subsidies,
         pct_gdp_govt_debt = GDD_pct_gdp_central_govt_debt,
         pct_cabinet_right = CPDS_pct_govt_cabinet_right_party,
         pct_cabinet_left = CPDS_pct_govt_cabinet_left_party,
         pct_legis_right = CPDS_legis_seat_share_right_party,
         pct_legis_left = CPDS_legis_seat_share_left_party) %>%
  na.omit() %>%
  select('iso3', 'year', sort(colnames(.)))
#-----------------------------------------------------------------------------------------------
write_rds(regression_subset_df, "regression_subset_df.rds")
#-----------------------------------------------------------------------------------------------
names(regression_subset_df)








