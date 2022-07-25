library(tidyverse)
library(wid)
#--------------------------------------------------------------------------------------
mega_WID_df <- download_wid(
  indicators = c('mcfcco', 'mcfcnf', 'mcwbus',
                 'mcwdeb', 'mnsrco', 'mgdpro',
                 'mnwbus', 'mcomhn', 'mssbgo',
                 'mssbco', 'mconfc', 'msopgo',
                 'mspigo', 'mprinf', 'mprgnf',
                 'mgsrnf', 'mnsrnf', 'mtaxnf',
                 'inyixx', 'mprifc', 'mpwbus',
                 'mpinnx', 'mfkpin', 'mprpgo',
                 'mprpnf', 'mprpco', 'mprico',
                 'mprpho', 'mpriho', 'mcomnp',
                 'mprpnp', 'mcfcho', 'mgwbus',
                 'mnmxho', 'mptxgo', 'mcfchn',
                 'mcfcgo', 'mnninc', 'mprigo',
                 'mprpfc', 'mgsrfc', 'mprgfc',
                 'mtaxfc', 'mcwfin', 'mnsrfc',
                 'mpwfin', 'mgwfin', 'npopul',
                 'npopem', 'xlcusx', 'xlcusp',
                 'wlabsh', 'wcapsh', 'wwealn',
                 'wwealp', 'wwealh', 'wwealc',
                 'wwealg', 'mnwnfa', 'mtaxco',
                 'mgwnfa', 'mssbhn', 'mcwnfa',
                 'mnsrgo', 'mnsrhn', 'mnsrco',
                 'mnwdwe', 'mnwlan', 'mnwagr',
                 'mnwnat', 'mnwodk', 'mfdirx',
                 'mfsubx', 'mprggo', 'mgwagr',
                 "mgwnat", "mgwhou", 'mcongo',
                 'mgpsgo', 'mdefgo', 'mecogo',
                 'mhougo', 'mheago', 'medugo',
                 'msopgo', 'mprihn', 'mprphn',
                 'mnwhou'),
  areas = c("AU", "AT", "BE", "CA", "CL",
            "CZ", "FR", "DE", "EE", "ES",
            'FI', "FR", "GB", "GR", "HU",
            'IE', "IL", "IS", "IT", "JP",
            "KR", "LU", "MX", "NL", "NZ",
            "NO", "PL", "PT", "SK", "SI",
            "SE", "CH", "TR", "US"),
  years = 1960:2020,
  ages = 999,
  perc = "all") %>%
  select(-percentile) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(country_code_ISO2 = country,
         corporate_business_assets_2020_LCU = mcwbus999i,
         corporate_consumption_fixed_capital_2020_LCU = mcfcco999i,
         nonfinancial_corp_consumption_fixed_capital_2020_LCU = mcfcnf999i,
         compensation_employees_total_2020_LCU = mcomhn999i,
         national_consumption_fixed_capital_2020_LCU = mconfc999i,
         corporate_debt_2020_LCU = mcwdeb999i,
         GDP_2020_LCU = mgdpro999i,
         corporate_net_operating_surplus_2020_LCU = mnsrco999i,
         national_business_assets_2020_LCU = mnwbus999i,
         govt_social_protection_spending_2020_LCU = msopgo999i,
         govt_subsidies_production_imports_2020_LCU = mspigo999i,
         private_social_insurance_2020_LCU = mssbco999i,
         public_social_insurance_2020_LCU = mssbgo999i,
         total_social_insurance_2020_LCU = mssbhn999i,
         national_income_price_index = inyixx999i,
         nonfinancial_corp_net_primary_income_2020_LCU = mprinf999i,
         financial_corp_net_primary_income_2020_LCU = mprifc999i,
         private_business_assets_2020_LCU_current_ppp = mpwbus999i,
         household_consumption_fixed_capital_2020_LCU = mcfcho999i,
         NPISH_compensation_employees_2020_LCU = mcomnp999i,
         national_net_capital_income_2020_LCU = mfkpin999i,
         nonfinancial_corp_gross_operating_surplus_2020_LCU = mgsrnf999i,
         govt_business_nonfinancial_assets_2020_LCU = mgwbus999i,
         nonfinancial_corps_net_operating_surplus_2020_LCU = mnsrnf999i,
         net_foreign_capital_income_2020_LCU = mpinnx999i,
         nonfinancial_corps_gross_primary_income_2020_LCU = mprgnf999i,
         corporate_net_primary_income_2020_LCU = mprico999i,
         household_net_primary_income_2020_LCU = mpriho999i,
         corporate_net_property_income_2020_LCU = mprpco999i,
         govt_net_property_income_2020_LCU = mprpgo999i,
         household_net_property_income_2020_LCU = mprpho999i,
         nonfinancial_corps_net_property_income_2020_LCU = mprpnf999i,
         NPISH_net_property_income_2020_LCU = mprpnp999i,
         nonfinancial_corps_corporate_tax_2020_LCU = mtaxnf999i,
         ppp_LCU_per_USD_YOY = xlcusp999i,
         govt_consumption_fixed_capital_2020_LCU = mcfcgo999i,
         NPISH_households_consumption_fixed_capital_2020_LCU = mcfchn999i,
         household_net_mixed_income_2020_LCU = mnmxho999i,
         net_national_income_2020_LCU = mnninc999i,
         govt_net_primary_income_2020_LCU = mprigo999i,
         NPISH_households_net_primary_income_2020_LCU = mprihn999i,
         NPISH_households_property_income_2020_LCU = mprphn999i,
         taxes_products_production_2020_LCU = mptxgo999i,
         national_taxes_less_subsidies_production_imports_2020_LCU = mptxgo999i,
         financial_corps_gross_operating_surplus_2020_LCU = mgsrfc999i,
         financial_corps_net_operating_surplus_2020_LCU = mnsrfc999i,
         financial_corps_gross_primary_income_2020_LCU = mprgfc999i,
         financial_corps_net_property_income_2020_LCU = mprpfc999i,
         financial_corps_paid_corporate_taxes_2020_LCU = mtaxfc999i,
         financial_corps_consumption_fixed_capital_2020_LCU = mcfcnf999i,
         corporate_financial_assets_2020_LCU = mcwfin999i,
         NPISH_household_financial_assets_2020_LCU = mpwfin999i,
         govt_financial_assets_constant_2015_LCU = mgwfin999i,
         population_employed_total = npopem999i,
         population_total_male = npopul999m,
         population_total_female = npopul999f,
         population_total = npopul999i,
         market_exchange_rate_LCU_per_USD = xlcusx999i,
         ppp_convesion_rate_LCU_per_USD = xlcusp999i,
         ratio_labor_share_factor_price_national_income = wlabsh999i,
         ratio_capital_share_factor_price_national_income = wcapsh999i,
         ratio_net_national_wealth_to_net_national_income = wwealn999i,
         ratio_net_priavte_wealth_to_net_national_income = wwealp999i,
         ratio_net_personal_wealth_to_net_national_income = wwealh999i,
         ratio_net_public_wealth_to_net_national_income = wwealg999i,
         national_nonfinancial_assets_2020_LCU = mnwnfa999i,
         corporate_tax_2020_LCU = mtaxco999i,
         govt_nonfinancial_assets_2020_LCU = mgwnfa999i,
         corporate_nonfinancial_assets_2020_LCU = mcwnfa999i,
         corporations_net_operating_surplus_2020_LCU = mnsrco999i,
         financial_corps_net_operating_surplus_2020_LCU = mnsrfc999i,
         govt_net_operating_surplus_2020_LCU = mnsrgo999i,
         NPISH_housholds_net_operating_surplus_2020_LCU = mnsrhn999i,
         nonfinancial_corps_net_operating_surplus_2020_LCU = mnsrnf999i,
         national_housing_assets_2020_LCU = mnwhou999i,
         national_agricultural_land_2020_LCU = mnwagr999i,
         national_dwellings_2020_LCU = mnwdwe999i,
         national_land_under_dwellings_2020_LCU = mnwlan999i,
         national_natural_capital_2020_LCU = mnwnat999i,
         national_machinery_plus_equip_2020_LCU = mnwodk999i,
         national_FDI_recieved_2020_LCU = mfdirx999i,
         national_foreign_subsidies_production_2020_LCU = mfsubx999i,
         govt_gross_primary_income_2020_LCU = mprggo999i,
         govt_housing_assets_2020_LCU = mgwhou999i,
         govt_agricultural_land_2020_LCU = mgwagr999i,
         govt_natural_capital_2020_LCU = mgwnat999i)
#---------------------------------------------------------------------------------------
write_rds(mega_WID_df, "1 - Data/1 - Tidy Data/mega_WID_df.rds")
#---------------------------------------------------------------------------------------
wid_inequality_df <- download_wid(
  indicators = 'sptinc',
  areas = c("AU", "AT", "BE", "CA", "CL",
            "CZ", "FR", "DE", "EE", "ES",
            'FI', "FR", "GB", "GR", "HU",
            'IE', "IL", "IS", "IT", "JP",
            "KR", "LU", "MX", "NL", "NZ",
            "NO", "PL", "PT", "SK", "SI",
            "SE", "CH", "TR", "US"),
  years = 1960:2020,
  ages = 992,
  perc = c('p90p100', 'p99p100', 'p99.9p100',
           'p0p20', 'p0p25', 'p0p30', 'p0p50', 'p0p99', 'p0p90', 'p0p99.9',
           'p10p11', 'p20p21', 'p25p26','p30p31', 'p50p51', 'p90p91'),
  pop = 'j') %>%
  pivot_wider(names_from = c(variable, percentile), values_from = value) %>%
  arrange(country, year) %>%
  rename(country_code_ISO2 = country,
         pre_tax_income_share_bottom_20pct = sptinc992j_p0p20,
         pre_tax_income_share_bottom_25pct = sptinc992j_p0p25,
         pre_tax_income_share_bottom_30pct = sptinc992j_p0p30,
         pre_tax_income_share_bottom_50pct = sptinc992j_p0p50,
         pre_tax_income_share_bottom_90pct = sptinc992j_p0p90,
         pre_tax_income_share_bottom_99pct = sptinc992j_p0p99,
         pre_tax_income_share_top_10pct = sptinc992j_p90p100,
         pre_tax_income_share_top_0.1pct = sptinc992j_p99.9p100,
         pre_tax_income_share_top_1pct = sptinc992j_p99p100,
         pre_tax_income_share_10th_pctile = sptinc992j_p10p11,
         pre_tax_income_share_20th_pctile = sptinc992j_p20p21,
         pre_tax_income_share_25th_pctile = sptinc992j_p25p26,
         pre_tax_income_share_30th_pctile = sptinc992j_p30p31,
         pre_tax_income_share_50th_pctile = sptinc992j_p50p51,
         pre_tax_income_share_90th_pctile = sptinc992j_p90p91)
#---------------------------------------------------------------------------------------
write_rds(wid_inequality_df, "1 - Data/1 - Tidy Data/wid_inequality_df.rds")
#---------------------------------------------------------------------------------------
wid_pt_disp_income_df <- download_wid(
  indicators = c('acainc'),
  areas = c("AU", "AT", "BE", "CA", "CL",
            "CZ", "FR", "DE", "EE", "ES",
            'FI', "FR", "GB", "GR", "HU",
            'IE', "IL", "IS", "IT", "JP",
            "KR", "LU", "MX", "NL", "NZ",
            "NO", "PL", "PT", "SK", "SI",
            "SE", "CH", "TR", "US"),
  years = 1960:2020,
  ages = 992,
  pop = 'j',
  perc = c('p20p21',
           'p30p31',
           'p50p51',
           'p80p81',
           'p90p91',
           'p99p100')) %>%
  pivot_wider(names_from = c(variable, percentile), values_from = value) %>%
  arrange(country, year) %>%
  rename(country_code_ISO2 = country,
         avg_post_tax_disp_income_20th_pctile_2020_LCU = acainc992j_p20p21,
         avg_post_tax_disp_income_30th_pctile_2020_LCU = acainc992j_p30p31,
         avg_post_tax_disp_income_50th_pctile_2020_LCU = acainc992j_p50p51,
         avg_post_tax_disp_income_80th_pctile_2020_LCU = acainc992j_p80p81,
         avg_post_tax_disp_income_90th_pctile_2020_LCU = acainc992j_p90p91,
         avg_post_tax_disp_income_99th_pctile_2020_LCU = acainc992j_p99p100)
#---------------------------------------------------------------------------------------
write_rds(wid_pt_disp_income_df, "1 - Data/1 - Tidy Data/wid_pt_disp_income_df.rds")
#---------------------------------------------------------------------------------------
wid_govt_spending_df <- download_wid(
  indicators = c('mcongo', 'mgpsgo', 'mdefgo',
                 'mecogo', 'mhougo', 'mheago',
                 'medugo', 'msopgo', "mgwdeb"),
  areas = c("AU", "AT", "BE", "CA", "CL",
            "CZ", "FR", "DE", "EE", "ES",
            'FI', "FR", "GB", "GR", "HU",
            'IE', "IL", "IS", "IT", "JP",
            "KR", "LU", "MX", "NL", "NZ",
            "NO", "PL", "PT", "SK", "SI",
            "SE", "CH", "TR", "US"),
  years = 1960:2020,
  ages = 999,
  perc = "all") %>%
  select(-percentile) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  arrange(country, year) %>%
  rename(country_code_ISO2 = country,
         govt_total_expenditure_2020_LCU = mcongo999i,
         govt_defense_expenditure_2020_LCU = mdefgo999i,
         govt_econ_affairs_expenditure_2020_LCU = mecogo999i,
         govt_education_expenditure_2020_LCU = medugo999i,
         govt_misc_public_services_2020_LCU = mgpsgo999i,
         govt_health_spending_2020_LCU = mheago999i,
         govt_housing_spending_2020_LCU = mhougo999i,
         govt_debt_2020_LCU = mgwdeb999i)
#---------------------------------------------------------------------------------------
write_rds(wid_govt_spending_df, "1 - Data/1 - Tidy Data/wid_govt_spending_df.rds")
#---------------------------------------------------------------------------------------
wid__machinery_plus_equip_df <- download_wid(
  indicators = c('mgwodk', 'mpwodk',
                 'mhwodk', 'miwodk', 'mcwodk'),
  areas = c("AU", "AT", "BE", "CA", "CL",
            "CZ", "FR", "DE", "EE", "ES",
            'FI', "FR", "GB", "GR", "HU",
            'IE', "IL", "IS", "IT", "JP",
            "KR", "LU", "MX", "NL", "NZ",
            "NO", "PL", "PT", "SK", "SI",
            "SE", "CH", "TR", "US"),
  years = 1960:2020,
  ages = 999,
  perc = "all") %>%
  select(-percentile) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(country_code_ISO2 = country,
         govt_machinery_plus_equip_2020_LCU = mgwodk999i,
         NPISH_plus_household_machinery_plus_equip_2020_LCU = mpwodk999i,
         household_machinery_plus_equip_2020_LCU = mhwodk999i,
         NPISH_machinery_plus_equip_2020_LCU = miwodk999i,
         corporate_machinery_plus_equip_2020_LCU = mcwodk999i) %>%
  arrange(country_code_ISO2, year)
#---------------------------------------------------------------------------------------
write_rds(wid__machinery_plus_equip_df, "1 - Data/1 - Tidy Data/wid__machinery_plus_equip_df.rds")
#---------------------------------------------------------------------------------------
mega_WID_df_final <- mega_WID_df %>%
  left_join(wid_inequality_df, by = c('country_code_ISO2', 'year')) %>%
  left_join(wid_govt_spending_df, by = c('country_code_ISO2', 'year')) %>%
  left_join(wid__machinery_plus_equip_df, by = c('country_code_ISO2', 'year')) %>%
  left_join(wid_pt_disp_income_df, by = c('country_code_ISO2', 'year')) %>%
  select('country_code_ISO2', 'year',
         'market_exchange_rate_LCU_per_USD', 'ppp_convesion_rate_LCU_per_USD', 'national_income_price_index',
         sort(colnames(.))) %>%
  rename_with( ~ paste("WID", .x, sep = "_")) %>%
  rename(country_code_ISO2 = WID_country_code_ISO2,
         year = WID_year)
#---------------------------------------------------------------------------------------
write_rds(mega_WID_df_final, "1 - Data/1 - Tidy Data/mega_WID_df_final.rds")
#---------------------------------------------------------------------------------------
