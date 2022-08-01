#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------

WDI::WDIsearch(string = "Interest payments on external debt")
#--------------------------------------------------------------------------------------
full_OECD_country_list_df_iso2 <- (full_OECD_country_list_df$country_code_ISO2)

misc_WDI_df <- WDI(country = "all",
    indicator = c('NE.GDI.FPRV.CN',
                  'GC.NFN.TOTL.GD.ZS',
                  'SM.POP.TOTL.ZS',
                  'NY.ADJ.DKAP.CD',
                  'NY.ADJ.DKAP.GN.ZS',
                  'GC.XPN.COMP.CN',
                  '8.0.LIPI',
                  'SL.UEM.TOTL.NE.ZS',
                  'FP.CPI.TOTL.ZG',
                  'FS.AST.PRVT.GD.ZS',
                  'NY.GDP.TOTL.RT.ZS',
                  'SI.POV.UMIC'),
    start = 1965,
    end = 2022,
    extra = TRUE,
    cache = NULL,
    latest = NULL,
    language = "en") %>%
  select(-country) %>%
  rename(iso2 = iso2c,
         GFCF_private_sector_current_LCU = NE.GDI.FPRV.CN,
         pct_gdp_net_invest_nonfin_assets = GC.NFN.TOTL.GD.ZS,
         intl_migrant_pct_total_population = SM.POP.TOTL.ZS,
         consumption_fixed_capital_current_USD = NY.ADJ.DKAP.CD,
         pct_GNI_consumption_fixed_capital = NY.ADJ.DKAP.GN.ZS,
         compensation_employees_current_LCU = GC.XPN.COMP.CN,
         labor_income_poverty_idx = `8.0.LIPI`,
         unemployment_rate_national_estimate = SL.UEM.TOTL.NE.ZS,
         inflation_CPI_pct_annual = FP.CPI.TOTL.ZG,
         pct_gdp_domestic_credit_provided_to_private_sector = FS.AST.PRVT.GD.ZS,
         pct_gdp_natural_resource_rents = NY.GDP.TOTL.RT.ZS,
         poverty_headcount_5_50ppp = SI.POV.UMIC) %>%
  rename_with( ~ paste("wb", .x, sep = "_")) %>%
  rename(iso2 = wb_iso2,
         year = wb_year) %>%
  mutate(year = as.numeric(year))

misc_WDI_df_edit <- misc_WDI_df %>%
  select(-c(wb_lastupdated, wb_status))

#--------------------------------------------------------------------------------------
write_rds(misc_WDI_df_edit, "3 - Data/misc_WDI_df_edit.rds")
#--------------------------------------------------------------------------------------
WDI_full_df <- WDIbulk(timeout = 600)

WDI_full_df_edit <- WDI_full_df[["Data"]] %>%
  rename(iso3 = "Country.Code") %>%
  select(-Indicator.Code, -Country.Name) %>%
  pivot_wider(names_from = Indicator.Name, values_from = value)
#--------------------------------------------------------------------------------------
write_rds(WDI_full_df_edit, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/WDI_full_df.rds")
#--------------------------------------------------------------------------------------
names(WDI_full_df_edit)

WDI_full_df_edit_v1 <- WDI_full_df_edit[ , -c(3:17, 31:58, 62:217, 220:245, 265:285,
                       306:311, 323:392, 417:425, 434:466, 541:543,
                       558:592, 604:608, 643:663, 691:727, 736:801,
                       812, 815:846, 872:905, 918:929, 934:937, 953:986,
                       991:1000)]

names(WDI_full_df_edit_v1)

WDI_full_df_edit_v2 <- WDI_full_df_edit_v1[ , -c(418:462, 464:480, 484:580, 594:604,
                          610:621, 628:647, 664:690, 706:786)]
#--------------------------------------------------------------------------------------
write_rds(WDI_full_df_edit_v2, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/WDI_full_df_edit_v2.rds")
#--------------------------------------------------------------------------------------






