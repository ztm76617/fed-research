#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------
mega_combined_vars_df_final %>%
  select(contains("wdi")) %>%
  names()
#--------------------------------------------------------------------------------------
summary(mega_combined_vars_df_final$`wdi_Coverage of social protection and labor programs (% of population)`)
#--------------------------------------------------------------------------------------
regression_ready_df <- mega_combined_vars_df_final %>%
  filter(year >= 1980) %>%
  select(iso3, year,
         wid_pre_tax_income_share_top_0.1pct,
         wid_pre_tax_income_share_top_1pct,
         wid_pre_tax_income_share_top_10pct,
         wid_ratio_income_share_99th_50th_pctile,
         wid_ratio_income_share_99th_10th_pctile,
         wid_ratio_income_share_90th_50th_pctile,
         wid_ratio_income_share_90th_10th_pctile,
         wid_ratio_capital_share_factor_price_national_income,
         pwt_irr,
         cpds_unemployment_rate,
         cpds_labor_force_partip_rate,
         cpds_inflation_rate,
         oecd_union_density,
         pwt_gdp_pc_growth_2017_USD,
         pwt_human_capital_index,
         wid_pct_gdp_national_financial_assets,
         pwt_pct_gdp_international_trade,
         cabinet_majority_left,
         cabinet_majority_right,
         cpds_voter_turnout,
         oecd_public_socspend_wa_pct_gdp,
         wdi_pct_labor_force_advanced_education,
         wdi_pct_gdp_financial_services) %>%
  make.pbalanced()
#-----------------------------------------------------------------------------------------------
form_m1.1 <- wid_ratio_income_share_90th_10th_pctile ~
  pwt_irr +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  wdi_pct_gdp_financial_services +
  factor(year)

form_m1.2 <- wid_ratio_income_share_90th_10th_pctile ~
  pwt_irr +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  wdi_pct_gdp_financial_services +
  wid_ratio_capital_share_factor_price_national_income +
  factor(year)

form_m1.3 <- wid_ratio_income_share_90th_10th_pctile ~
  pwt_irr +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  wdi_pct_gdp_financial_services +
  wid_ratio_capital_share_factor_price_national_income +
  pwt_human_capital_index +
  pwt_pct_gdp_international_trade +
  factor(year)

form_m1.4 <- wid_ratio_income_share_90th_10th_pctile ~
  pwt_irr +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  wdi_pct_gdp_financial_services +
  wid_ratio_capital_share_factor_price_national_income +
  pwt_human_capital_index +
  pwt_pct_gdp_international_trade +
  oecd_public_socspend_wa_pct_gdp +
  factor(year)
#-----------------------------------------------------------------------------------------------
# Models
#-----------------------------------------------------------------------------------------------
lm_m1 <- lm(form_m1.1, data = regression_ready_df)
lm_m2 <- lm(form_m1.2, data = regression_ready_df)
lm_m3 <- lm(form_m1.3, data = regression_ready_df)
lm_m4 <- lm(form_m1.4, data = regression_ready_df)
#-----------------------------------------------------------------------------------------------
# Robust Standard Error Objects
#-----------------------------------------------------------------------------------------------
hc_se_lm_m1 <- sqrt(diag(vcovHC(lm_m1, type = "HC1")))
hc_se_lm_m2 <- sqrt(diag(vcovHC(lm_m2, type = "HC1")))
hc_se_lm_m3 <- sqrt(diag(vcovHC(lm_m3, type = "HC1")))
hc_se_lm_m4 <- sqrt(diag(vcovHC(lm_m4, type = "HC1")))
#-----------------------------------------------------------------------------------------------
# PCSE Standard Error Objects
#-----------------------------------------------------------------------------------------------
pcse_se_lm_m1 <- sqrt(diag(vcovPC(lm_m1, cluster = ~ iso3 + year)))
pcse_se_lm_m2 <- sqrt(diag(vcovPC(lm_m2, cluster = ~ iso3 + year)))
pcse_se_lm_m3 <- sqrt(diag(vcovPC(lm_m3, cluster = ~ iso3 + year)))
pcse_se_lm_m4 <- sqrt(diag(vcovPC(lm_m4, cluster = ~ iso3 + year)))
#-----------------------------------------------------------------------------------------------
# Creating List of LM Objects
#-----------------------------------------------------------------------------------------------
lm_group <- list(lm_m1, lm_m2, lm_m3, lm_m4)
#-----------------------------------------------------------------------------------------------
hc.se_group <- list(hc_se_lm_m1, hc_se_lm_m2, hc_se_lm_m3, hc_se_lm_m4)
#-----------------------------------------------------------------------------------------------
pcse.se_group <- list(pcse_se_lm_m1, pcse_se_lm_m2, pcse_se_lm_m3, pcse_se_lm_m4)
#-----------------------------------------------------------------------------------------------
stargazer(lm_group,
          se = c(pcse.se_group),
          type = "text",
          omit = c("factor\\(iso3\\)", "factor\\(year\\)"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          header = FALSE,
          no.space = TRUE)
#-----------------------------------------------------------------------------------------------



