#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling); library(rticles)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(ggthemr)
library(plm); library(WDI); library(lmtest); library(sandwich); library(interactions); library(rmarkdown);
library(gtable); library(grid); library(gridExtra); library(captioner); library(countrycode)
#--------------------------------------------------------------------------------------
# Load Data
#--------------------------------------------------------------------------------------
pwt_mega_df <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/pwt_mega_df.rds")
cpds.df_tidy <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/cpds.df_tidy.rds")
Global_Debt_Database_tidy_v2 <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/Global_Debt_Database_tidy_v2.rds")
manifesto_project_df_tidy <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/manifesto_project_df_tidy.rds")
vdem_full_tidy <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/vdem_full_tidy.rds")
global_oecd_dfs_merged <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/global_oecd_dfs_merged.rds")
DPI_df_tidy <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/DPI_df_tidy.rds")
WDI_full_df_edit_v2 <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/WDI_full_df_edit_v2.rds")
wid_df_final <- readRDS("~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/wid_df_final.rds")
#--------------------------------------------------------------------------------------
# No scientific notation
options(scipen = 100)
#--------------------------------------------------------------------------------------
# Country Groupings
g7_countries <- c("CAN", "USA", "GBR", "FRA", "JPN", "DEU", "ITA")
#--------------------------------------------------------------------------------------
## Adding prefixes
df %>% rename_with( ~ paste0("a", .x))

## Adding suffixes
df %>% rename_with( ~ paste0(.x, "a"))
#--------------------------------------------------------------------------------------
# Creating the base mega-dataset 
mega_combined_vars_df <- full_OECD_country_list_df %>%
  left_join(wid_df_final, by = c('iso2')) %>%
  left_join(cpds.df_tidy, by = c('iso3', 'year')) %>%
  left_join(pwt_mega_df, by = c('iso3', 'year')) %>%
  left_join(WDI_full_df_edit_v2, by = c('iso3', 'year')) %>%
  left_join(Global_Debt_Database_tidy_v2, by = c('iso3', 'year')) %>%
  left_join(global_oecd_dfs_merged, by = c('iso3', 'year')) %>%
  left_join(vdem_full_tidy, by = c('iso3', 'year')) %>%
  left_join(manifesto_project_df_tidy, by = c('iso2', 'year')) %>%
  left_join(DPI_df_tidy, by = c('iso3', 'year')) %>%
  set_names(~ (.) %>%
              str_replace_all("Non_financial_corporations", "nonfinancial_corps") %>%
              str_replace_all("non_financial_corporations", "nonfinancial_corps") %>%
              str_replace_all("Total_economy", "national") %>%
              str_replace_all("total_economy", "national") %>%
              str_replace_all("Financial_corporations", "financial_corps")) %>%
  group_by(iso3) %>%
  mutate(wid_ppp_LCU_per_USD_2015 = case_when(year == 2015 ~ wid_ppp_convesion_rate_LCU_per_USD),
         wid_ppp_LCU_per_USD_2017 = case_when(year == 2017 ~ wid_ppp_convesion_rate_LCU_per_USD),
         wid_ppp_LCU_per_USD_2019 = case_when(year == 2019 ~ wid_ppp_convesion_rate_LCU_per_USD),
         wid_ppp_LCU_per_USD_2020 = case_when(year == 2020 ~ wid_ppp_convesion_rate_LCU_per_USD),
         wid_market_exchange_rate_LCU_per_USD_2015 = case_when(year == 2015 ~ wid_market_exchange_rate_LCU_per_USD),
         wid_market_exchange_rate_LCU_per_USD_2017 = case_when(year == 2017 ~ wid_market_exchange_rate_LCU_per_USD),
         wid_market_exchange_rate_LCU_per_USD_2019 = case_when(year == 2019 ~ wid_market_exchange_rate_LCU_per_USD),
         wid_market_exchange_rate_LCU_per_USD_2020 = case_when(year == 2020 ~ wid_market_exchange_rate_LCU_per_USD)) %>%
  fill(c(wid_ppp_LCU_per_USD_2015,
         wid_ppp_LCU_per_USD_2017,
         wid_ppp_LCU_per_USD_2019,
         wid_market_exchange_rate_LCU_per_USD_2015,
         wid_market_exchange_rate_LCU_per_USD_2017,
         wid_market_exchange_rate_LCU_per_USD_2019), .direction = "downup") %>%
  ungroup(iso3) %>%
  mutate(country_name = countrycode(iso3, origin = 'iso3c', destination = 'p4.name')) %>%
  select('country_name', 'iso3', 'iso2', 'year', sort(colnames(.))) %>%
  arrange(iso3, year) %>%
  distinct(iso3, year, .keep_all = TRUE)
#--------------------------------------------------------------------------------------
write_rds(mega_combined_vars_df, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/mega_combined_vars_df.rds")
#--------------------------------------------------------------------------------------
mega_combined_vars_df_final <- mega_combined_vars_df %>%
  group_by(iso3) %>%
  rename(national_income_price_index_base_2021 = wid_national_income_price_index) %>%
  mutate(wid_national_price_idex_2017 = (national_income_price_index_base_2021/national_income_price_index_base_2021[year == 2017])*1,
         wid_national_price_idex_2015 = (national_income_price_index_base_2021/national_income_price_index_base_2021[year == 2015])*1) %>%
  mutate(wid_national_financial_assets_2021_LCU = wid_corporate_financial_assets_2021_LCU + wid_govt_financial_assets_2021_LCU + wid_NPISH_household_financial_assets_2021_LCU,
         wid_pct_total_assets_financial_assets = (wid_national_financial_assets_2021_LCU/(wid_national_financial_assets_2021_LCU + wid_national_nonfinancial_assets_2021_LCU))*100,
         wid_pct_gdp_corporate_financial_assets = (wid_corporate_financial_assets_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_corporate_nonfinancial_assets = (wid_corporate_nonfinancial_assets_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_national_financial_assets = (wid_national_financial_assets_2021_LCU/wid_GDP_2021_LCU),
         wid_ratio_financial_assets_to_nonfinancial_assets = wid_national_financial_assets_2021_LCU/wid_national_nonfinancial_assets_2021_LCU,
         wid_pct_gdp_govt_nonfinancial_assets = (wid_govt_nonfinancial_assets_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_corporate_taxes = (wid_corporate_tax_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_corporate_debt = (wid_corporate_debt_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_govt_expenditure_subsidies = (wid_govt_subsidies_production_imports_2021_LCU/wid_govt_total_expenditure_2021_LCU)*100,
         wid_pct_gdp_govt_subsidies = (wid_govt_subsidies_production_imports_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_national_nonfinancial_assets = (wid_national_nonfinancial_assets_2021_LCU/wid_GDP_2021_LCU)*100,
         wid_pct_gdp_national_financial_assets_annual_growth = pct_change_function(wid_pct_gdp_national_financial_assets),
         wid_pct_change_pct_total_assets_financial_assets = pct_change_function(wid_pct_total_assets_financial_assets),
         wid_pct_gdp_national_nonfinancial_assets_pct_change = pct_change_function(wid_pct_gdp_national_nonfinancial_assets),
         wid_ratio_financial_vs_nonfinancial_assets = wid_national_financial_assets_2021_LCU/wid_national_nonfinancial_assets_2021_LCU,
         wid_ratio_pct_gdp_financial_assets_vs_nonfinancial_assets = (wid_pct_gdp_national_financial_assets/wid_pct_gdp_national_nonfinancial_assets)*100,
         wid_ratio_income_share_99th_50th_pctile = wid_pre_tax_income_share_top_1pct/wid_pre_tax_income_share_50th_pctile,
         wid_ratio_income_share_99th_10th_pctile = wid_pre_tax_income_share_top_1pct/wid_pre_tax_income_share_10th_pctile,
         wid_ratio_income_share_90th_50th_pctile = wid_pre_tax_income_share_90th_pctile/wid_pre_tax_income_share_50th_pctile,
         wid_ratio_income_share_90th_10th_pctile = wid_pre_tax_income_share_90th_pctile/wid_pre_tax_income_share_10th_pctile,
         wid_ratio_income_share_50th_10th_pctile = wid_pre_tax_income_share_50th_pctile/wid_pre_tax_income_share_10th_pctile,
         wid_yoy_pct_change_pct_gdp_national_nonfinancial_assets = pct_change_function(wid_pct_gdp_national_nonfinancial_assets)) %>%
  mutate_at(vars(starts_with("wid_pre_tax_income_share")), x100_func) %>%
  mutate(pwt_pct_GDP_GFCF = (pwt_gross_fixed_capital_formation_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_pct_gdp_GFCF_5yr_ma = rollmean(pwt_pct_GDP_GFCF, k = 5, fill = NA),
         pwt_pct_gdp_imports = (pwt_imports_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_pct_gdp_exports = (pwt_exports_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_pct_gdp_total_investment = (pwt_national_investment_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_pct_change_pct_gdp_total_investment = pct_change_function(pwt_pct_gdp_total_investment),
         pwt_pct_total_investment_GFCF = (pwt_gross_fixed_capital_formation_current_LCU/pwt_national_investment_current_LCU)*100,
         pwt_pct_change_average_hours_worked_annual = pct_change_function(pwt_average_hours_worked_per_worker_annual)*100,
         pwt_employee_incomes_current_LCU = pwt_gdp_current_LCU*pwt_employee_labor_share_income_gdp,
         pwt_employee_compensation_current_LCU = pwt_gdp_current_LCU * pwt_labor_share_compensation,
         pwt_gdp_per_capita_2017_LCU = pwt_gdp_constant_2017_LCU/pwt_population_total,
         pwt_gdp_per_capita_2017_USD = pwt_gdp_constant_2017_USD/pwt_population_total,
         pwt_gdp_per_capita_2017_ppp = pwt_gdp_output_constant_2017_chained_ppp/pwt_population_total,
         pwt_gdp_pc_growth_2017_LCU = pct_change_function(pwt_gdp_per_capita_2017_LCU),
         pwt_gdp_pc_growth_2017_USD = pct_change_function(pwt_gdp_per_capita_2017_USD),
         pwt_gdp_pc_growth_2017_ppp = pct_change_function(pwt_gdp_per_capita_2017_ppp),
         pwt_gdp_pc_growth_2017_LCU_5yr_MA = rollmean(pwt_gdp_pc_growth_2017_LCU, k = 5, fill = NA),
         pwt_gdp_pc_growth_2017_USD_5yr_MA = rollmean(pwt_gdp_pc_growth_2017_USD, k = 5, fill = NA),
         pwt_gdp_pc_growth_2017_ppp_5yr_MA = rollmean(pwt_gdp_pc_growth_2017_ppp, k = 5, fill = NA),
         pwt_gdp_growth_2017_LCU = pct_change_function(pwt_gdp_constant_2017_LCU),
         pwt_gdp_growth_2017_USD = pct_change_function(pwt_gdp_constant_2017_USD),
         pwt_gdp_growth_2017_ppp = pct_change_function(pwt_gdp_output_constant_2017_chained_ppp),
         pwt_gdp_growth_2017_LCU_5yr_MA = rollmean(pwt_gdp_growth_2017_LCU, k = 5, fill = NA),
         pwt_gdp_growth_2017_USD_5yr_MA = rollmean(pwt_gdp_growth_2017_USD, k = 5, fill = NA),
         pwt_gdp_growth_2017_ppp_5yr_MA = rollmean(pwt_gdp_growth_2017_ppp, k = 5, fill = NA),
         pwt_icor_number_LCU = pwt_gdp_growth_2017_LCU/pwt_pct_GDP_GFCF,
         pwt_icor_number_USD = pwt_gdp_growth_2017_USD/pwt_pct_GDP_GFCF,
         pwt_icor_number_ppp = pwt_gdp_growth_2017_ppp/pwt_pct_GDP_GFCF,
         pwt_capital_depcreciation_current_LCU = pwt_net_fixed_capital_stock_total_current_LCU*pwt_capital_deprec_decimal,
         pwt_net_fixed_capital_formation_current_LCU = pwt_gross_fixed_capital_formation_current_LCU - pwt_capital_depcreciation_current_LCU,
         pwt_pct_gdp_net_fixed_capital_formation_number = (pwt_net_fixed_capital_formation_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_ratio_GFCF_vs_NFCF = pwt_gross_fixed_capital_formation_current_LCU/pwt_net_fixed_capital_formation_current_LCU,
         pwt_pct_gdp_structural_investment = (pwt_investment_res_nonres_structures_current_LCU/pwt_gdp_current_LCU)*100,
         pwt_pct_total_pop_working_pop = (pwt_employed_population/pwt_population_total)*100,
         pwt_marx_rop =
           ((pwt_gdp_current_LCU - pwt_employee_incomes_current_LCU - pwt_capital_depcreciation_current_LCU)/
           (pwt_net_fixed_capital_stock_total_current_LCU + pwt_employee_incomes_current_LCU))*100,
         pwt_marx_rop_lag = dplyr::lag(pwt_marx_rop),
         pwt_irr = pwt_internal_rate_return_number,
         pwt_irr_lag = dplyr::lag(pwt_irr),
         pwt_pct_gdp_international_trade = pwt_pct_gdp_imports + pwt_pct_gdp_exports) %>%
  mutate(oecd_total_socspend_current_LCU_millions_FULLPOP = oecd_total_private_socspend_full_pop_current_LCU_millions + oecd_public_socspend_full_pop_current_LCU_millions,
         private_socspend_FULLPOP_2017_USD_millions = oecd_total_private_socspend_full_pop_current_LCU_millions/wid_market_exchange_rate_LCU_per_USD_2017,
         public_socspend_FULLPOP_2017_USD_millions = oecd_public_socspend_full_pop_current_LCU_millions/wid_market_exchange_rate_LCU_per_USD_2017,
         total_socspend_FULLPOP_2017_USD_millions = oecd_total_socspend_current_LCU_millions_FULLPOP/wid_market_exchange_rate_LCU_per_USD_2017,
         faricy_ratio = (oecd_total_private_socspend_full_pop_pct_gdp/oecd_public_socspend_full_pop_pct_gdp)*100,
         pct_total_socspend_private = (oecd_total_private_socspend_full_pop_current_LCU_millions/oecd_total_socspend_current_LCU_millions_FULLPOP)*100,
         pct_change_private_welfare = pct_change_function(oecd_total_private_socspend_full_pop_pct_gdp),
         pct_change_private_welfare_lag = dplyr::lag(pct_change_private_welfare),
         private_welfare_pc_current_LCU = oecd_total_private_socspend_full_pop_current_LCU_millions/pwt_population_total,
         gdp_pc_current_LCU = pwt_gdp_current_LCU/pwt_population_total,
         pct_gdp_pc_private_welfare_pc = (private_welfare_pc_current_LCU/gdp_pc_current_LCU)*100,
         private_welfare_work_pc_current_LCU = oecd_total_private_socspend_full_pop_current_LCU_millions/pwt_total_working_population,
         gdp_work_pc_current_LCU = pwt_gdp_current_LCU/pwt_total_working_population,
         pct_gdp_pc_private_welfare_work_pc = (private_welfare_work_pc_current_LCU/gdp_work_pc_current_LCU)*100,
         pct_gdp_private_welfare = oecd_total_private_socspend_full_pop_pct_gdp,
         pct_gdp_public_welfare = oecd_public_socspend_full_pop_pct_gdp,
         pct_gdp_total_welfare = pct_gdp_private_welfare + pct_gdp_public_welfare,
         pct_gdp_wa_private_welfare = oecd_total_private_socspend_wa_pct_gdp,
         pct_gdp_wa_public_welfare = oecd_public_socspend_wa_pct_gdp,
         pct_gdp_total_wa_welfare = pct_gdp_wa_private_welfare + pct_gdp_wa_public_welfare) %>%
  mutate(right_exec = if_else(DPI_left_right_chief_exec_party == 1, 1, 0),
         left_exec = if_else(DPI_left_right_chief_exec_party == 3, 1, 0),
         centrist_exec = if_else(DPI_left_right_chief_exec_party == 2, 1, 0),
         legislature_majority_right = if_else(cpds_legis_seat_share_right_party > 50, 1, 0),
         legislature_majority_left = if_else(cpds_legis_seat_share_left_party > 50, 1, 0),
         legislature_right_over_left = if_else(cpds_legis_seat_share_right_party > cpds_legis_seat_share_left_party, 1, 0),
         legislature_left_over_right = if_else(cpds_legis_seat_share_left_party > cpds_legis_seat_share_right_party, 1, 0),
         cabinet_majority_right = if_else(cpds_pct_govt_cabinet_right_party > 50, 1, 0),
         cabinet_majority_left = if_else(cpds_pct_govt_cabinet_left_party > 50, 1, 0),
         cabinet_control_right = if_else(cpds_pct_govt_cabinet_right_party > cpds_pct_govt_cabinet_left_party & cpds_pct_govt_cabinet_right_party > cpds_pct_govt_cabinet_center_party, 1, 0),
         cabinet_control_left = if_else(cpds_pct_govt_cabinet_left_party > cpds_pct_govt_cabinet_right_party & cpds_pct_govt_cabinet_left_party > cpds_pct_govt_cabinet_center_party, 1, 0),
         cabinet_right_over_left = if_else(cpds_pct_govt_cabinet_right_party > cpds_pct_govt_cabinet_left_party, 1, 0),
         cabinet_left_over_right = if_else(cpds_pct_govt_cabinet_left_party > cpds_pct_govt_cabinet_right_party, 1, 0),
         cabinet_left_dominance = if_else(cpds_ideology_dominance_RL == 4, 1, 0),
         cabinet_right_center_dominance = if_else(cpds_ideology_dominance_RL == 2, 1, 0)) %>%
  ungroup(iso3) %>%
  mutate(manif_party_LR_idx_wt_avg = (-1*manif_party_right_left_idx_wt_avg)) %>%
  select('country_name', 'iso3', 'iso2', 'year', sort(colnames(.)))
#---------------------------------------------------------------------------------------------------------------------------
write_rds(mega_combined_vars_df_final, '~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/mega_combined_vars_df_final.rds')
#---------------------------------------------------------------------------------------------------------------------------





