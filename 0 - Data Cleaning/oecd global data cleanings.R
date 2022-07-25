#-------------------------------------------------------------------------------------------------------------
# Load packages
library(tidyverse) ; library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(OECD); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(rqdatatable)
#-------------------------------------------------------------------------------------------------------------
oecd_income_dist_global_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Income Distribution Database (GLOBAL).csv")

oecd_natl_accnts_glance_global_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd National Accounts at a Glance (GLOBAL).csv")

oecd_econ_outlook_global_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Economic Outlook (GLOBAL).csv")

oecd_socspend_df_wa_old_age <- read_csv("1 - Data/Extra data/1 - OECD/oecd socspend df.csv")

oecd_union_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Trade Union Dataset (TOTAL).csv")
#-------------------------------------------------------------------------------------------------------------
oecd_natl_accnts_glance_global_df_tidy <- oecd_natl_accnts_glance_global_df %>%
  mutate(variable = str_replace_all(variable, c("," = "",
                                                "Gross domestic product [(]GDP[)]" = "GDP",
                                                "percentage of GDP" = "pct GDP",
                                                "US dollars" = "USD",
                                                "percentage of net value added" = "pct NVA",
                                                "financial corporations" = "finan corps",
                                                "Purchasing power parities" = "PPPs",
                                                "disposable income" = "disp income",
                                                "Gross fixed capital formation" = "GFCF",
                                                "Social benefits and social transfers" = "socspend",
                                                "percentage of total expenditure of general government (GG)" = "pct govt expend",
                                                "Leverage of the banking sector ratio of selected assets to equity number of times" = "banking sector leverage to equity",
                                                "annual growth rates percentage" = "annual growth",
                                                "Compensation of employees" = "compensation employees",
                                                "household" = "HH",
                                                "Debt of" = "debt",
                                                "year 2015 = 100" = "base 2015",
                                                "General government" = "govt",
                                                "general government" = "govt",
                                                "of national currency" = "LCU",
                                                "socspend in kind pct GDP" = "pct GDP socspend",
                                                "deflated by" = "deflator",
                                                "current prices and current PPPs" = "current LCU current PPPs",
                                                "percentage of" = "pct",
                                                "gross value added [(]GVA[)]" = "GVA",
                                                "Gross value added [(]GVA[)]" = "GVA",
                                                "Financial corporations debt debt to equity number of times" = "finan corps debt to equity",
                                                "annual growth rates in percentage" = "annual growth",
                                                " [(]GG[)]" = "",
                                                " " = "_"))) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  select('country_code_ISO3', 'year', sort(colnames(.))) %>%
  rename_at(vars(banking_sector_leverage_to_equity:Total_expenditure_of_govt_pct_GDP), tolower)
#-------------------------------------------------------------------------------------------------------------
oecd_econ_outlook_global_df_tidy <- oecd_econ_outlook_global_df %>%
  select(country_code_ISO3, variable, year, value) %>%
  mutate(variable = str_replace_all(variable, c("Compensation of employees, total economy" = "compensation_employees_total_econ",
                                                "Compensation rate, total economy" = "compensation_rate_total_econ",
                                                "Core inflation index" = "core_inflation_idx",
                                                "Core inflation" = "core_inflation_rate",
                                                "General government employment" = "govt_employment",
                                                "Gross disposable income of household and non-profit institutions serving households" = "gross_disp_income_HH_plus_NPISH",
                                                "Gross fixed capital formation of private non-residential industry excluding shipping and oil, volume" = "GFCF_private_nonres_industry_excl_shipping_oil",
                                                "Gross fixed capital formation, total, nominal value" = "GFCF_nominal",
                                                "Gross fixed capital formation, total, volume" = "GFCF_volume",
                                                "Hours worked per worker, total economy" = "hours_worked_per_worker",
                                                "Labour force participation rate, as a percentage of population aged 15-74" = "labor_partip_rate",
                                                "Labour productivity, total economy" = "labor_productivity",
                                                "Long-term interest rate on government bonds" = "longterm_rate_govt_bonds",
                                                "Net disposable income of households and non-profit institutions serving households" = "net_disp_income_HH_plus_NPISH",
                                                "Productive capital stock, volume, growth" = "productive_capital_stock_volume_yoy_growth",
                                                "Productive capital stock, volume, annual average, growth" = "productive_capital_stock_volume_avg_growth",
                                                "Real gross disposable income of households and non-profit institutions serving households" = "real_gross_disp_income_HH_plus_NPISH",
                                                "Real net disposable income of households and non-profit institutions serving households" = "real_net_disp_income_HH_plus_NPISH",
                                                "Short-term interest rate" = "short_interest_rate",
                                                "Total employment, growth" = "employment_growth",
                                                "Unit labour cost in total economy" = "unit_labor_costs",
                                                "Wage rate, total economy" = "wage_rate_total_econ",
                                                "Working-age population, age 15-74" = "working_age_pop"))) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  select('country_code_ISO3', 'year', sort(colnames(.)))
#-------------------------------------------------------------------------------------------------------------
oecd_corporate_taxes_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Statutory corporate income tax rate.csv")

oecd_corporate_taxes_df_tidy <- oecd_corporate_taxes_df %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  rename(corporate_tax_rate = `Corporate income tax rate`,
         combined_corporate_tax_rate = `Combined corporate income tax rate`) %>%
  select('country_code_ISO3', 'year', sort(colnames(.))) %>%
  arrange(country_code_ISO3, year)
#------------------------------------------------------------------------------
oecd_socspend_TOTAL_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Social Expenditure - Aggregated data (TOTAL).csv")

oecd_socspend_TOTAL_df_tidy <- oecd_socspend_TOTAL_df %>%
  pivot_wider(names_from = c(source, variable),
              values_from = value) %>%
  arrange(country_code_ISO3, year) %>%
  rename(public_socspend_current_LCU_millions = `Public_At current prices in national currency, in millions`,
         pct_gdp_public_socspend = `Public_In percentage of Gross Domestic Product`,
         pct_govt_expenditure_public_socspend = `Public_In percentage of Total General Government Expenditure`,
         private_socspend_current_LCU_millions = `Private (Mandatory and Voluntary)_At current prices in national currency, in millions`,
         pct_gdp_private_socspend = `Private (Mandatory and Voluntary)_In percentage of Gross Domestic Product`,
         pct_govt_expenditure_private_socspend = `Private (Mandatory and Voluntary)_In percentage of Total General Government Expenditure`,
         pct_gdp_net_total_socspend = `Net Total_In percentage of Gross Domestic Product`,
         pct_gdp_net_public_socspend = `Net Public_In percentage of Gross Domestic Product`) %>%
  mutate(total_socspend_current_LCU_millions = public_socspend_current_LCU_millions + private_socspend_current_LCU_millions)
#------------------------------------------------------------------------------
oecd_socspend_df_wa_old_age_tidy <- oecd_socspend_df_wa_old_age %>%
  mutate(source = str_replace_all(source, c("Mandatory private" = "mand_private_socspend",
                                            "Voluntary private" = "volun_private_socspend",
                                            "Private [(]Mandatory and Voluntary[)]" = "total_private_socspend",
                                            "Net Public" = "net_public",
                                            "Net Total" = "net_total",
                                            "Public" = "public_socspend")),
         branch = str_replace_all(branch, c("Old age" = "old_age",
                                            "Total" = "full_pop")),
         measure = str_replace_all(measure, c("At current prices in national currency, in millions" = "current_LCU_millions",
                                              "In percentage of Gross Domestic Product" = "pct_gdp",
                                              "In percentage of Total General Government Expenditure" = "pct_govt_expenditure"))) %>%
  pivot_wider(names_from = c(source, branch, measure),
              values_from = value) %>%
  arrange(country_code_ISO3, year) %>%
  mutate(mand_private_socspend_wa_pct_gdp = mand_private_socspend_full_pop_pct_gdp - mand_private_socspend_old_age_pct_gdp,
         mand_private_socspend_wa_current_LCU_millions = mand_private_socspend_full_pop_current_LCU_millions - mand_private_socspend_old_age_current_LCU_millions,
         mand_private_socspend_wa_pct_govt_expenditure = mand_private_socspend_full_pop_pct_govt_expenditure - mand_private_socspend_old_age_pct_govt_expenditure,
         net_private_total_pct_gdp = net_total_full_pop_pct_gdp - net_public_full_pop_pct_gdp,
         total_private_socspend_wa_current_LCU_millions = total_private_socspend_full_pop_current_LCU_millions - total_private_socspend_old_age_current_LCU_millions,
         total_private_socspend_wa_pct_gdp = total_private_socspend_full_pop_pct_gdp - total_private_socspend_old_age_pct_gdp,
         total_private_socspend_wa_pct_govt_expenditure = total_private_socspend_full_pop_pct_govt_expenditure - total_private_socspend_old_age_pct_govt_expenditure,
         public_socspend_wa_pct_govt_expenditure = public_socspend_full_pop_pct_govt_expenditure - public_socspend_old_age_pct_govt_expenditure,
         public_socspend_wa_current_LCU_millions = public_socspend_full_pop_current_LCU_millions - public_socspend_old_age_current_LCU_millions,
         public_socspend_wa_pct_gdp = public_socspend_full_pop_pct_gdp - public_socspend_old_age_pct_gdp,
         volun_private_socspend_wa_pct_govt_expenditure = volun_private_socspend_full_pop_pct_govt_expenditure - volun_private_socspend_old_age_pct_govt_expenditure,
         volun_private_socspend_wa_pct_gdp = volun_private_socspend_full_pop_pct_gdp - volun_private_socspend_old_age_pct_gdp,
         volun_private_socspend_wa_current_LCU_millions = volun_private_socspend_full_pop_current_LCU_millions - volun_private_socspend_old_age_current_LCU_millions,
         total_socspend_wa_current_LCU_millions = public_socspend_wa_current_LCU_millions + total_private_socspend_wa_current_LCU_millions,
         total_socspend_wa_pct_govt_expenditure = public_socspend_wa_pct_govt_expenditure + total_private_socspend_wa_pct_govt_expenditure,
         total_socspend_wa_pct_gdp = total_private_socspend_wa_pct_gdp + public_socspend_wa_pct_gdp) %>%
  select('country_code_ISO3', 'year', sort(colnames(.)))
#-------------------------------------------------------------------------------------------------------------
oecd_GDI_df <- read_csv("1 - Data/Extra data/1 - OECD/Gross domestic product (GDP) income approach.csv")
#-------------------------------------------------------------------------------------------------------------
oecd_industrial_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Production and sales.csv")

names(oecd_industrial_df)

oecd_industrial_df_tidy <- oecd_industrial_df %>%
  pivot_wider(names_from = c(variable, unit_code, ref_year),
              values_from = value) %>%
  arrange(country_code_ISO3, year) %>%
  rename(total_industrial_production_idx_2015_100 = 'Production of total industry sa, Index_IDX_2015_100',
         manufacturing_production_idx_2015_100 = 'Production in total manufacturing sa, Index_IDX_2015_100',
         manufacturing_intermed_goods_production_idx_2010_100 = 'Production of total manufactured intermediate goods sa, Index_IDX_2010_100',
         manufacturing_investment_goods_production_idx_2010_100 = 'Production of total manufactured investment goods sa, Index_IDX_2010_100',
         construction_production_idx_2015_100 = 'Production of total construction sa, Index_IDX_2015_100',
         total_energy_production_idx_2010_100 = 'Production of total construction sa, Index_IDX_2015_100',
         electrictity_gas_steam_etc__production_idx = 'Production of electricity, gas, steam and air conditioning supply sa, index_NA_NA')
#-------------------------------------------------------------------------------------------------------------
oecd_population_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Historical population data.csv")

oecd_population_df_tidy <- oecd_population_df %>%
  pivot_wider(names_from = age, values_from = value) %>%
  rename(total_population = Total,
         population_15_64 = "15 to 64",
         population_20_64 = "20 to 64",
         population_65plus = "65 and over",
         pct_population_elderly = "Share of 65 and over - elderly",
         pct_population_children = "Share of under 15 - children",
         old_age_dependency_ratio = "Old age dependency ratio (65 and over/15-64)",
         total_dependecy_ratio = "Total dependency ratio ((<20 & 65+) / 20-64)",
         pct_population_working_age = "Share of 15 to 64 - working age")
#-------------------------------------------------------------------------------------------------------------
oecd_govt_spending_df <- read_csv("1 - Data/Extra data/1 - OECD/oecd Government expenditure by function (COFOG).csv")

oecd_govt_spending_df_tidy <- oecd_govt_spending_df %>%
  filter(variable == 'Total function',
         sub_variable %in% c('Total compensation of employees paid by the government', 'Total government expenditure')) %>%
  pivot_wider(names_from = c(gov_type, variable, sub_variable, power_code),
              values_from = value) %>%
  rename(ALL_GOV_total_expenditure_current_LCU_millions = "General government_Total function_Total government expenditure_Millions",
         ALL_GOV_employee_compensation_current_LCU_millions = "General government_Total function_Total compensation of employees paid by the government_Millions",
         central_govt_total_expenditure_current_LCU = "Central government_Total function_Total government expenditure_Millions",
         central_govt_employee_compensation_current_LCU_millions = "Central government_Total function_Total compensation of employees paid by the government_Millions")
#-------------------------------------------------------------------------------------------------------------
global_oecd_dfs_merged <- oecd_econ_outlook_global_df_tidy %>%
  left_join(oecd_natl_accnts_glance_global_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_corporate_taxes_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_socspend_df_wa_old_age_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_union_df, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_industrial_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_population_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  left_join(oecd_govt_spending_df_tidy, by = c('country_code_ISO3', 'year')) %>%
  select('country_code_ISO3', 'year', sort(colnames(.))) %>%
  rename_with( ~ paste("oecd", .x, sep = "_")) %>%
  rename(country_code_ISO3 = oecd_country_code_ISO3,
         year = oecd_year)
#-------------------------------------------------------------------------------------------------------------
write_rds(global_oecd_dfs_merged, "1 - Data/global_oecd_dfs_merged.rds")
#-------------------------------------------------------------------------------------------------------------

names(global_oecd_dfs_merged)






