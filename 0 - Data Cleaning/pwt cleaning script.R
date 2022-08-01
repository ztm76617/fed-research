# Load packages
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates);
library(plm); library(WDI); library(lmtest); library(sandwich); library(nlme)
#---------------------------------------------------------------------------------
pwt10 <- read_excel("1 - Data/2 - Raw Data/Penn World Tables/pwt100.xlsx", sheet = "Data") %>%
  rename(country_code_ISO3 = countrycode)
#---------------------------------------------------------------------------------
pwt10_main_vars_df <- pwt10 %>%
  select(country_code_ISO3, year, irr, rgdpna, rgdpo, labsh,
         rtfpna, rwtfpna, cn, rnna, ck, rkna, delta, cgdpo, cgdpe,
         avh, emp, pop, csh_c, hc, csh_i) %>%
  rename(pwt_gdp_constant_2017_USD = rgdpna,
         pwt_gdp_output_constant_2017_chained_ppp = rgdpo,
         pwt_gdp_output_approach_constant_2017_current_ppp = cgdpo,
         pwt_gdp_expend_approach_constant_2017_current_ppp = cgdpe,
         pwt_share_household_consumption_gdp_current_ppp = csh_c,
         pwt_labor_compensation_share_gdp_decimal = labsh,
         pwt_human_capital_index = hc,
         pwt_internal_rate_return = irr,
         pwt_total_factor_product = rtfpna,
         pwt_welfare_total_factor_product = rwtfpna,
         pwt_capital_stock_2017_price_2017_USD = rnna,
         pwt_capital_stock_2017_price_2017_ppp = cn,
         pwt_capital_services_2017_price_2017_ppp = ck,
         pwt_capital_services_idx_2017_1 = rkna,
         pwt_capital_deprec_decimal = delta,
         pwt_average_hours_worked_per_worker_annual = avh,
         pwt_population_total = pop,
         pwt_employed_population = emp,
         pwt_gdp_share_gross_capital_formation = csh_i) %>%
  mutate(pwt_internal_rate_return_number = (pwt_internal_rate_return*100)) %>%
  arrange(country_code_ISO3, year)
#---------------------------------------------------------------------------------
pwt10_labor_detail <- read_dta("1 - Data/2 - Raw Data/Penn World Tables/pwt100-labor-detail.dta") %>%
  rename(country_code_ISO3 = countrycode)
#---------------------------------------------------------------------------------
pwt10_labor_detail_tidy <- pwt10_labor_detail %>%
  select(country_code_ISO3, year,
         emp, labsh, comp_sh, lab_sh1,
         lab_sh2, lab_sh3, yr_sch) %>%
  rename(pwt_total_working_population = emp,
         pwt_employee_labor_share_income_gdp = comp_sh,
         pwt_labor_share_compensation = labsh,
         pwt_labor_income_share_gdp_all_mixed_income = lab_sh1,
         pwt_labor_income_share_gdp_partial_mixed_income = lab_sh2,
         pwt_labor_income_share_gdp_avg_employee_income = lab_sh3,
         pwt_avg_years_schooling_25yr_plus_pop = yr_sch)
#---------------------------------------------------------------------------------
pwt10_trade_detail <- read_dta("1 - Data/2 - Raw Data/Penn World Tables//pwt100-trade-detail.dta") %>%
  rename(country_code_ISO3 = countrycode)
#---------------------------------------------------------------------------------
pwt10_trade_detail_tidy <- pwt10_trade_detail %>%
  select(country_code_ISO3, year,
         csh_x2, csh_m2,
         csh_x4, csh_m4,
         csh_x6, csh_m6,
         csh_x5, csh_m5) %>%
  rename(pwt_share_industrial_exports = csh_x2,
         pwt_share_industrial_imports = csh_m2,
         pwt_share_capital_goods_exports = csh_x4,
         pwt_share_capital_goods_imports = csh_m4,
         pwt_share_transport_equip_exports = csh_x5,
         pwt_share_transport_equip_imports = csh_m5,
         pwt_share_consumer_goods_exports = csh_x6,
         pwt_share_consumer_goods_imports = csh_m6)
#---------------------------------------------------------------------------------
pwt10_NA_data <-read_excel("1 - Data/2 - Raw Data/Penn World Tables/pwt100-na-data.xlsx", sheet = "Data") %>%
  rename(country_code_ISO3 = countrycode)
#---------------------------------------------------------------------------------
pwt10_NA_data_tidy <- pwt10_NA_data %>%
  select(country_code_ISO3, year,
         v_gdp, v_c, v_i,
         v_x, v_m, v_gfcf,
         q_gdp) %>%
  rename(pwt_household_consumption_current_LCU = v_c,
         pwt_national_investment_current_LCU = v_i,
         pwt_exports_current_LCU = v_x,
         pwt_imports_current_LCU = v_m,
         pwt_gross_fixed_capital_formation_current_LCU = v_gfcf,
         pwt_gdp_current_LCU = v_gdp,
         pwt_gdp_constant_2017_LCU = q_gdp)
#---------------------------------------------------------------------------------
pwt10_capital_detail <- read_excel("1 - Data/2 - Raw Data/Penn World Tables/pwt100-capital-detail.xlsx", sheet = "Data") %>%
  rename(country_code_ISO3 = countrycode)
#---------------------------------------------------------------------------------
pwt10_capital_detail_tidy <- pwt10_capital_detail %>%
  select(country_code_ISO3, year,
         Ic_Struc, Ic_Mach, Ic_TraEq,
         Nc_Struc, Nc_Mach, Nc_TraEq,
         Dc_Struc, Dc_Mach, Dc_TraEq,
         Ksh_Struc, Ksh_Mach, Ksh_TraEq,
         Ic_Other, Nc_Other, Dc_Other,
         Nc_Other) %>%
  rename(pwt_investment_res_nonres_structures_current_LCU = Ic_Struc,
         pwt_investment_machinery_plus_equipment_current_LCU = Ic_Mach,
         pwt_investment_transport_equip_current_LCU = Ic_TraEq,
         pwt_investment_other_assets_current_LCU = Ic_Other,
         pwt_net_capital_stock_structures_current_LCU = Nc_Struc,
         pwt_net_capital_stock_machinery_plus_equipment_current_LCU = Nc_Mach,
         pwt_net_capital_stock_transport_equip_current_LCU = Nc_TraEq,
         pwt_net_capital_stock_other_assets_current_LCU = Nc_Other,
         pwt_capital_consumption_structures_current_LCU = Dc_Struc,
         pwt_capital_consumption_machinery_current_LCU = Dc_Mach,
         pwt_capital_consumption_transport_equip_current_LCU = Dc_TraEq,
         pwt_capital_consumption_other_assets_current_LCU = Dc_Other,
         pwt_share_net_capital_services_res_nonres_structures = Ksh_Struc,
         pwt_share_net_capital_services_machinery = Ksh_Mach,
         pwt_share_net_capital_services_transport_equip = Ksh_TraEq) %>%
  mutate(pwt_net_fixed_capital_stock_total_current_LCU =
           pwt_net_capital_stock_structures_current_LCU +
           pwt_net_capital_stock_machinery_plus_equipment_current_LCU +
           pwt_net_capital_stock_transport_equip_current_LCU +
           pwt_net_capital_stock_other_assets_current_LCU,
         pwt_total_capital_consumption_current_LCU =
           pwt_capital_consumption_structures_current_LCU +
           pwt_capital_consumption_machinery_current_LCU +
           pwt_capital_consumption_transport_equip_current_LCU +
           pwt_capital_consumption_other_assets_current_LCU)
#---------------------------------------------------------------------------------
merged_misc_pwt_dfs <- Reduce(function(...) merge(..., all=TRUE),
                              list(pwt10_main_vars_df,
                                   pwt10_labor_detail_tidy,
                                   pwt10_trade_detail_tidy,
                                   pwt10_NA_data_tidy,
                                   pwt10_capital_detail_tidy)) %>%
  select('country_code_ISO3', 'year',
         sort(colnames(.))) %>%
  arrange(country_code_ISO3, year) %>%
  rename(iso3 = country_code_ISO3)
#---------------------------------------------------------------------------------
pwt_mega_df_v2 <- pwt_mega_df %>%
  rename(iso3 = country_code_ISO3)
#---------------------------------------------------------------------------------
write_rds(pwt_mega_df_v2, '~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/pwt_mega_df.rds')
#---------------------------------------------------------------------------------








  


