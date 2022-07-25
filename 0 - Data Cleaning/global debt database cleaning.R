# Load packages
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates);
library(plm); library(WDI); library(lmtest); library(sandwich); library(countrycode)
#---------------------------------------------------------------------------
Global_Debt_Database <- read_dta("~/My Drive/Misc PDFs + eBooks/Misc. Data/Global Debt Database/Global Debt Database.dta")
#---------------------------------------------------------------------------
table(Global_Debt_Database$country)

Global_Debt_Database_tidy <- Global_Debt_Database %>%
  rename(GDD_pct_gdp_total_private_debt = pvd_all,
         GDD_pct_gdp_private_debt_loans_plus_debt_securities = pvd_ls,
         GDD_pct_gdp_total_household_debt = hh_all,
         GDD_pct_gdp_household_debt_loans_plus_debt_securities = hh_ls,
         GDD_pct_gdp_total_nfc_debt = nfc_all,
         GDD_pct_gdp_nfc_debt_loans_plus_debt_securities = nfc_ls,
         GDD_pct_gdp_central_govt_debt = cg,
         GDD_gdp_current_LCU_billions = ngdp,
         country_name = country) %>%
  select(country_name, year,
         GDD_gdp_current_LCU_billions,
         GDD_pct_gdp_total_private_debt,
         GDD_pct_gdp_private_debt_loans_plus_debt_securities,
         GDD_pct_gdp_total_household_debt,
         GDD_pct_gdp_household_debt_loans_plus_debt_securities,
         GDD_pct_gdp_total_nfc_debt,
         GDD_pct_gdp_nfc_debt_loans_plus_debt_securities,
         GDD_pct_gdp_central_govt_debt) %>%
  mutate(country_name = str_replace_all(country_name,
                                        c("China, Mainland" = "China",
                                         "Russian Federation" = "Russia",
                                         'Korea, Republic of' = 'South Korea'))) %>%
  mutate(country_code_ISO3 = countrycode(country_name, origin = 'country.name', destination = 'iso3c'))
#---------------------------------------------------------------------------
Global_Debt_Database_tidy_v2 <- Global_Debt_Database_tidy %>%
  select(-country_name)
#---------------------------------------------------------------------------
write_rds(Global_Debt_Database_tidy_v2, '1 - Data/1 - Tidy Data/Global_Debt_Database_tidy_v2.rds')
#---------------------------------------------------------------------------

















