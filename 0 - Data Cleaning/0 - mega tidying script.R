#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling); library(rticles)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(ggthemr)
library(plm); library(WDI); library(lmtest); library(sandwich); library(interactions); library(rmarkdown);
library(gtable); library(grid); library(gridExtra); library(captioner)
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
  left_join(mega_WID_df_final, by = c('country_code_ISO2')) %>%
  left_join(DPI_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(CPDS.df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(merged_misc_pwt_dfs, by = c('country_code_ISO3', 'year')) %>%
  left_join(misc_WDI_vars_df, by = c('country_code_ISO2', 'year')) %>%
  left_join(Global_Debt_Database_tidy_v2, by = c('country_code_ISO3', 'year')) %>%
  left_join(global_oecd_dfs_merged, by = c('country_code_ISO3', 'year')) %>%
  left_join(vdem_full_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(manifesto_project_df_tidy, by = c('country_code_ISO2', 'year')) %>%
  set_names(~ (.) %>%
              str_replace_all("Non_financial_corporations", "nonfinancial_corps") %>%
              str_replace_all("non_financial_corporations", "nonfinancial_corps") %>%
              str_replace_all("Total_economy", "national") %>%
              str_replace_all("total_economy", "national") %>%
              str_replace_all("Financial_corporations", "financial_corps")) %>%
  group_by(country_code_ISO3) %>%
  mutate(WID_ppp_LCU_per_USD_2015 = case_when(year == 2015 ~ WID_ppp_convesion_rate_LCU_per_USD),
         WID_ppp_LCU_per_USD_2017 = case_when(year == 2017 ~ WID_ppp_convesion_rate_LCU_per_USD),
         WID_ppp_LCU_per_USD_2019 = case_when(year == 2019 ~ WID_ppp_convesion_rate_LCU_per_USD),
         WID_ppp_LCU_per_USD_2020 = case_when(year == 2020 ~ WID_ppp_convesion_rate_LCU_per_USD),
         WID_market_exchange_rate_LCU_per_USD_2015 = case_when(year == 2015 ~ WID_market_exchange_rate_LCU_per_USD),
         WID_market_exchange_rate_LCU_per_USD_2017 = case_when(year == 2017 ~ WID_market_exchange_rate_LCU_per_USD),
         WID_market_exchange_rate_LCU_per_USD_2019 = case_when(year == 2019 ~ WID_market_exchange_rate_LCU_per_USD),
         WID_market_exchange_rate_LCU_per_USD_2020 = case_when(year == 2020 ~ WID_market_exchange_rate_LCU_per_USD)) %>%
  fill(c(WID_ppp_LCU_per_USD_2015,
         WID_ppp_LCU_per_USD_2017,
         WID_ppp_LCU_per_USD_2019,
         WID_market_exchange_rate_LCU_per_USD_2015,
         WID_market_exchange_rate_LCU_per_USD_2017,
         WID_market_exchange_rate_LCU_per_USD_2019), .direction = "downup") %>%
  ungroup(country_code_ISO3) %>%
  select('country_name', 'country_code_ISO3', 'country_code_ISO2', 'year', sort(colnames(.))) %>%
  arrange(country_name, year) %>%
  distinct(country_code_ISO3, year, .keep_all = TRUE)
#--------------------------------------------------------------------------------------
write_rds(mega_combined_vars_df, "mega_combined_vars_df.rds")
#--------------------------------------------------------------------------------------
# Creating more usable subset of data
manifesto_vars <- colnames(manifesto_project_df_tidy[4:19])

mega_combined_vars_df_SUBSET <- mega_combined_vars_df %>%
  select(country_name, country_code_ISO3, country_code_ISO2, year,
         contains(c("WID_", "pwt_", "CPDS_",
                    "GFCF_", 'KOFGI', "WB_WDI",
                    "wt_avg", "GDD", 'socspend', 'DPI_',
                    "oecd", "vdem", 'manif'))) %>%
  set_names(~ (.) %>%
              str_replace_all("Annual_growth_change_Percentage", "pct_growth")) %>%
  group_by(country_code_ISO3) %>%
  fill(c(vdem_economic_issues_LR_idx_wt_avg,
         vdem_economic_issues_LR_ord_wt_avg,
         vdem_welfare_LR_idx_wt_avg,
         vdem_welfare_LR_ord_wt_avg,
         all_of(manifesto_vars)), .direction = "down") %>%
  ungroup(country_code_ISO3) %>%
  select('country_code_ISO3','year', sort(colnames(.))) %>%
  distinct(country_code_ISO3, year, .keep_all = TRUE)
#---------------------------------------------------------------------------------------------------------------------------
write_rds(mega_combined_vars_df_SUBSET, "mega_combined_vars_df_SUBSET.rds")
#---------------------------------------------------------------------------------------------------------------------------
# adding custom variables to usable subset
mega_combined_vars_df_SUBSET_added_vars <- mega_combined_vars_df_SUBSET %>%
  group_by(country_code_ISO3) %>%
  rename(national_income_price_index_base_2020 = WID_national_income_price_index) %>%
  mutate(WID_private_nonfinancial_assets_2020_LCU = WID_national_nonfinancial_assets_2020_LCU - WID_govt_nonfinancial_assets_2020_LCU,
         WID_private_machinery_plus_equip_2020_LCU = WID_national_machinery_plus_equip_2020_LCU - WID_govt_machinery_plus_equip_2020_LCU,
         WID_private_consumption_fixed_capital_2020_LCU = WID_national_consumption_fixed_capital_2020_LCU - WID_govt_consumption_fixed_capital_2020_LCU,
         WID_private_business_assets_2020_LCU = WID_national_business_assets_2020_LCU - WID_govt_business_nonfinancial_assets_2020_LCU) %>%
  mutate(marx_rop_total_economy_v1 = ((WID_GDP_2020_LCU - WID_compensation_employees_total_2020_LCU - WID_national_consumption_fixed_capital_2020_LCU)/(WID_national_nonfinancial_assets_2020_LCU + WID_compensation_employees_total_2020_LCU))*100,
         marx_rop_total_economy_v2 = ((WID_GDP_2020_LCU - WID_compensation_employees_total_2020_LCU - WID_national_consumption_fixed_capital_2020_LCU)/(WID_national_machinery_plus_equip_2020_LCU + WID_compensation_employees_total_2020_LCU))*100,
         marx_rop_total_economy_v3 = ((WID_GDP_2020_LCU - WID_compensation_employees_total_2020_LCU - WID_national_consumption_fixed_capital_2020_LCU)/(WID_national_business_assets_2020_LCU + WID_compensation_employees_total_2020_LCU))*100,
         marx_rop_total_economy_v1_lag = dplyr::lag(marx_rop_total_economy_v1),
         marx_rop_total_economy_v2_lag = dplyr::lag(marx_rop_total_economy_v2),
         marx_rop_total_economy_v3_lag = dplyr::lag(marx_rop_total_economy_v3),
         pct_change_marx_rop_total_economy_v1 = pct_change_function(marx_rop_total_economy_v1),
         pct_change_marx_rop_total_economy_v2 = pct_change_function(marx_rop_total_economy_v2),
         pct_change_marx_rop_total_economy_v3 = pct_change_function(marx_rop_total_economy_v3)) %>%
  mutate(WID_national_net_operating_surplus_2020_LCU = WID_govt_net_operating_surplus_2020_LCU + WID_corporations_net_operating_surplus_2020_LCU + WID_NPISH_housholds_net_operating_surplus_2020_LCU,
         WID_private_net_operating_surplus_2020_LCU = WID_corporations_net_operating_surplus_2020_LCU + WID_NPISH_housholds_net_operating_surplus_2020_LCU,
         WID_national_financial_assets_2020_LCU = WID_corporate_financial_assets_2020_LCU + WID_govt_financial_assets_constant_2015_LCU + WID_NPISH_household_financial_assets_2020_LCU,
         WID_pct_gdp_corporate_financial_assets = (WID_corporate_financial_assets_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_pct_gdp_corporate_nonfinancial_assets = (WID_corporate_nonfinancial_assets_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_pct_gdp_national_financial_assets = (WID_national_financial_assets_2020_LCU/WID_GDP_2020_LCU),
         WID_ratio_financial_assets_to_nonfinancial_assets = WID_national_financial_assets_2020_LCU/WID_national_nonfinancial_assets_2020_LCU,
         WID_pct_gdp_govt_nonfinancial_assets = (WID_govt_nonfinancial_assets_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_national_price_idex_2017 = (national_income_price_index_base_2020/national_income_price_index_base_2020[year == 2017])*1,
         WID_national_price_idex_2015 = (national_income_price_index_base_2020/national_income_price_index_base_2020[year == 2015])*1,
         WID_pct_gdp_corporate_taxes = (WID_corporate_tax_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_pct_gdp_corporate_debt = (WID_corporate_debt_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_pct_govt_expenditure_subsidies = (WID_govt_subsidies_production_imports_2020_LCU/WID_govt_total_expenditure_2020_LCU)*100,
         WID_pct_gdp_govt_subsidies = (WID_govt_subsidies_production_imports_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_pct_total_assets_financial_assets = (WID_national_financial_assets_2020_LCU/(WID_national_financial_assets_2020_LCU + WID_national_nonfinancial_assets_2020_LCU))*100,
         WID_pct_gdp_national_nonfinancial_assets = (WID_national_nonfinancial_assets_2020_LCU/WID_GDP_2020_LCU)*100,
         WID_annual_growth_pct_gdp_national_nonfinancial_assets = pct_change_function(WID_pct_gdp_national_nonfinancial_assets),
         WID_pct_gdp_national_financial_assets_annual_growth = pct_change_function(WID_pct_gdp_national_financial_assets),
         WID_annual_growth_pct_gdp_national_nonfinancial_assets_5yr_ma = rollmean(WID_annual_growth_pct_gdp_national_nonfinancial_assets, k = 5, fill = NA),
         WID_annual_growth_pct_gdp_national_financial_assets_5yr_ma = rollmean(WID_pct_gdp_national_financial_assets_annual_growth, k = 5, fill = NA),
         WID_pct_change_pct_total_assets_financial_assets = pct_change_function(WID_pct_total_assets_financial_assets),
         WID_pct_gdp_national_nonfinancial_assets_pct_change = pct_change_function(WID_pct_gdp_national_nonfinancial_assets),
         WID_ratio_financial_vs_nonfinancial_assets = WID_national_financial_assets_2020_LCU/WID_national_nonfinancial_assets_2020_LCU,
         WID_ratio_pct_gdp_financial_assets_vs_nonfinancial_assets = (WID_pct_gdp_national_financial_assets/WID_pct_gdp_national_nonfinancial_assets)*100,
         WID_ratio_income_share_90th_50th_pctile = WID_pre_tax_income_share_90th_pctile/WID_pre_tax_income_share_50th_pctile,
         WID_ratio_income_share_50th_10th_pctile = WID_pre_tax_income_share_50th_pctile/WID_pre_tax_income_share_10th_pctile,
         WID_skew = WID_ratio_income_share_90th_50th_pctile/WID_ratio_income_share_50th_10th_pctile,
         WID_pct_gdp_govt_debt = (WID_govt_debt_2020_LCU/WID_GDP_2020_LCU)*100) %>%
  mutate_at(vars(starts_with("WID_pre_tax_income_share")), x100_func) %>%
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
         private_socspend_FULLPOP_2017_USD_millions = oecd_total_private_socspend_full_pop_current_LCU_millions/WID_market_exchange_rate_LCU_per_USD_2017,
         public_socspend_FULLPOP_2017_USD_millions = oecd_public_socspend_full_pop_current_LCU_millions/WID_market_exchange_rate_LCU_per_USD_2017,
         total_socspend_FULLPOP_2017_USD_millions = oecd_total_socspend_current_LCU_millions_FULLPOP/WID_market_exchange_rate_LCU_per_USD_2017,
         oecd_total_private_socspend_full_pop_pct_gdp_lag1 = dplyr::lag(oecd_total_private_socspend_full_pop_pct_gdp),
         faricy_ratio = (oecd_total_private_socspend_full_pop_pct_gdp/oecd_public_socspend_full_pop_pct_gdp)*100,
         pct_total_socspend_private = (oecd_total_private_socspend_full_pop_current_LCU_millions/oecd_total_socspend_current_LCU_millions_FULLPOP)*100,
         faricy_ratio_pct_change = pct_change_function(faricy_ratio),
         faricy_ratio_lag1 = dplyr::lag(faricy_ratio),
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
  mutate(total_private_socx_wa_pct_gdp = total_private_socx_fullpop_pct_gdp - total_private_socx_elderly_pct_gdp,
         ratio_private_vs_public_socx_v1 = total_private_socx_fullpop_current_LCU/total_public_socx_fullpop_current_LCU,
         ratio_private_vs_public_socx_v2 = total_private_socx_fullpop_pct_gdp/total_public_socx_fullpop_pct_gdp,
         ratio_wa_private_vs_public_socx = (total_private_socx_fullpop_current_LCU - total_private_socx_elderly_current_LCU)/(total_public_socx_fullpop_current_LCU - total_public_socx_elderly_current_LCU),
         pct_total_socx_private_v1 = (total_private_socx_fullpop_current_LCU/total_socx_fullpop_current_LCU)*100,
         pct_total_socx_private_v2 = (total_private_socx_fullpop_pct_gdp/total_socx_fullpop_pct_gdp)*100,
         pct_total_socx_private_wa_v1 = (total_private_socx_wa_pct_gdp/total_socx_fullpop_pct_gdp)*100,
         pct_total_socx_private_wa_v2 = (total_private_socx_fullpop_pct_gdp/total_socx_fullpop_pct_gdp)*100) %>%
  mutate(conservative_exec = if_else(DPI_left_right_chief_exec_party == 1, 1, 0),
         left_exec = if_else(DPI_left_right_chief_exec_party == 3, 1, 0),
         centrist_exec = if_else(DPI_left_right_chief_exec_party == 2, 1, 0),
         conservative_or_centrist_exec = if_else(DPI_left_right_chief_exec_party %in% c(1, 2), 1, 0),
         vdem_unified_govt_idx = rescale(-1*vdem_divided_govt_control),
         legislature_majority_right = if_else(CPDS_legis_seat_share_right_party > 50, 1, 0),
         legislature_majority_left = if_else(CPDS_legis_seat_share_left_party > 50, 1, 0),
         legislature_right_over_left = if_else(CPDS_legis_seat_share_right_party > CPDS_legis_seat_share_left_party, 1, 0),
         legislature_left_over_right = if_else(CPDS_legis_seat_share_left_party > CPDS_legis_seat_share_right_party, 1, 0),
         cabinet_majority_right = if_else(CPDS_pct_govt_cabinet_right_party > 50, 1, 0),
         cabinet_majority_left = if_else(CPDS_pct_govt_cabinet_left_party > 50, 1, 0),
         cabinet_control_right = if_else(CPDS_pct_govt_cabinet_right_party > CPDS_pct_govt_cabinet_left_party & CPDS_pct_govt_cabinet_right_party > CPDS_pct_govt_cabinet_center_party, 1, 0),
         cabinet_control_left = if_else(CPDS_pct_govt_cabinet_left_party > CPDS_pct_govt_cabinet_right_party & CPDS_pct_govt_cabinet_left_party > CPDS_pct_govt_cabinet_center_party, 1, 0),
         cabinet_right_over_left = if_else(CPDS_pct_govt_cabinet_right_party > CPDS_pct_govt_cabinet_left_party, 1, 0),
         cabinet_left_over_right = if_else(CPDS_pct_govt_cabinet_left_party > CPDS_pct_govt_cabinet_right_party, 1, 0),
         cabinet_left_dominance = if_else(CPDS_ideology_dominance_RL == 4, 1, 0),
         cabinet_right_center_dominance = if_else(CPDS_ideology_dominance_RL == 2, 1, 0),
         cabinet_left_hegemony = if_else(CPDS_ideology_dominance_RL == 5, 1, 0),
         cabinet_right_center_hegemony = if_else(CPDS_ideology_dominance_RL == 1, 1, 0),
         cabinet_balanced_power = if_else(CPDS_ideology_dominance_RL == 3, 1, 0),
         cabinet_majority_category = case_when(CPDS_pct_govt_cabinet_right_party > 50 ~ 1, CPDS_pct_govt_cabinet_center_party > 50 ~ 2, CPDS_pct_govt_cabinet_left_party > 50 ~ 3),
         cabinet_majority_category_v2 = case_when(CPDS_ideology_dominance_RL %in% c(1, 2) ~ 1,
                                                  CPDS_ideology_dominance_RL == 3 ~ 2,
                                                  CPDS_ideology_dominance_RL %in% c(4, 5) ~ 3)) %>%
  ungroup(country_code_ISO3) %>%
  mutate(manif_party_LR_idx_wt_avg = (-1*manif_party_right_left_idx_wt_avg)) %>%
  select('country_code_ISO3', 'country_code_ISO2', 'year', sort(colnames(.))) %>%
  distinct(country_code_ISO3, year, .keep_all = TRUE)
#---------------------------------------------------------------------------------------------------------------------------
write_rds(mega_combined_vars_df_SUBSET_added_vars, 'mega_combined_vars_df_SUBSET_added_vars.rds')
#---------------------------------------------------------------------------------------------------------------------------
regression_ready_df <- mega_combined_vars_df_SUBSET_added_vars %>%
  filter(year >= 1980 & year <= 2017) %>%
  mutate(iso3 = country_code_ISO3)

write_rds(regression_ready_df, 'regression_ready_df.rds')
#---------------------------------------------------------------------------------------------------------------------------
tinytex::install_tinytex()
install.packages("devtools")




