#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
regression_ready_df <- mega_combined_vars_df_final %>%
  filter(year >= 1980) %>%
  select(iso3, year,
         wid_pre_tax_income_share_top_1pct,
         wid_ratio_income_share_99th_50th_pctile,
         wid_ratio_income_share_99th_10th_pctile,
         wid_ratio_income_share_90th_50th_pctile,
         wid_ratio_income_share_90th_10th_pctile,
         pwt_irr,
         cpds_unemployment_rate,
         cpds_inflation_rate,
         oecd_union_density,
         pwt_gdp_pc_growth_2017_USD,
         pwt_human_capital_index,
         wid_pct_gdp_national_financial_assets,
         pwt_pct_gdp_international_trade,
         cabinet_majority_left,
         oecd_public_socspend_wa_pct_gdp) %>%
  make.pbalanced()

write_rds(regression_ready_df, '~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/regression_ready_df.rds')
#-----------------------------------------------------------------------------------------------
form_m1.1 <- wid_pre_tax_income_share_top_1pct ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  factor(iso3)

form_m2.1 <- wid_pre_tax_income_share_top_1pct ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  pwt_human_capital_index +
  wid_pct_gdp_national_financial_assets +
  pwt_pct_gdp_international_trade +
  oecd_public_socspend_wa_pct_gdp +
  cabinet_majority_left +
  factor(iso3)

form_m1.2 <- wid_ratio_income_share_99th_50th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  factor(iso3)

form_m2.2 <- wid_ratio_income_share_99th_50th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  pwt_human_capital_index +
  wid_pct_gdp_national_financial_assets +
  pwt_pct_gdp_international_trade +
  oecd_public_socspend_wa_pct_gdp +
  cabinet_majority_left +
  factor(iso3)

form_m1.3 <- wid_ratio_income_share_99th_10th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  factor(iso3)

form_m2.3 <- wid_ratio_income_share_99th_10th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  cpds_inflation_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  pwt_human_capital_index +
  wid_pct_gdp_national_financial_assets +
  pwt_pct_gdp_international_trade +
  oecd_public_socspend_wa_pct_gdp +
  cabinet_majority_left +
  factor(iso3)
#-----------------------------------------------------------------------------------------------
# Models
#-----------------------------------------------------------------------------------------------
lm_m1 <- lm(form_m1.1, data = regression_ready_df)
lm_m2 <- lm(form_m1.2, data = regression_ready_df)
lm_m3 <- lm(form_m1.3, data = regression_ready_df)
lm_m4 <- lm(form_m2.1, data = regression_ready_df)
lm_m5 <- lm(form_m2.2, data = regression_ready_df)
lm_m6 <- lm(form_m2.3, data = regression_ready_df)
#-----------------------------------------------------------------------------------------------
# Robust Standard Error Objects
#-----------------------------------------------------------------------------------------------
hc_se_lm_m1 <- sqrt(diag(vcovHC(lm_m1, type = "HC1")))
hc_se_lm_m2 <- sqrt(diag(vcovHC(lm_m2, type = "HC1")))
hc_se_lm_m3 <- sqrt(diag(vcovHC(lm_m3, type = "HC1")))
hc_se_lm_m4 <- sqrt(diag(vcovHC(lm_m4, type = "HC1")))
hc_se_lm_m5 <- sqrt(diag(vcovHC(lm_m5, type = "HC1")))
hc_se_lm_m6 <- sqrt(diag(vcovHC(lm_m6, type = "HC1")))
#-----------------------------------------------------------------------------------------------
# PCSE Standard Error Objects
#-----------------------------------------------------------------------------------------------
pcse_se_lm_m1 <- sqrt(diag(vcovPC(lm_m1, cluster = ~ iso3 + year)))
pcse_se_lm_m2 <- sqrt(diag(vcovPC(lm_m2, cluster = ~ iso3 + year)))
pcse_se_lm_m3 <- sqrt(diag(vcovPC(lm_m3, cluster = ~ iso3 + year)))
pcse_se_lm_m4 <- sqrt(diag(vcovPC(lm_m4, cluster = ~ iso3 + year)))
pcse_se_lm_m5 <- sqrt(diag(vcovPC(lm_m5, cluster = ~ iso3 + year)))
pcse_se_lm_m6 <- sqrt(diag(vcovPC(lm_m6, cluster = ~ iso3 + year)))
#-----------------------------------------------------------------------------------------------
# Creating List of LM Objects
#-----------------------------------------------------------------------------------------------
lm_group <- list(lm_m1, lm_m2, lm_m3, lm_m4, lm_m5, lm_m6)
#-----------------------------------------------------------------------------------------------
hc.se_group <- list(hc_se_lm_m1, hc_se_lm_m2, hc_se_lm_m3, hc_se_lm_m4, hc_se_lm_m5, hc_se_lm_m6)
#-----------------------------------------------------------------------------------------------
pcse.se_group <- list(pcse_se_lm_m1, pcse_se_lm_m2, pcse_se_lm_m3, pcse_se_lm_m4, pcse_se_lm_m5, pcse_se_lm_m6)
#-----------------------------------------------------------------------------------------------
stargazer(lm_group,
          se = c(pcse.se_group),
          type = "text",
          omit = c("factor\\(iso3\\)"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.caption = "Income Inequality",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          header = FALSE,
          no.space = TRUE,
          covariate.labels = c("IRR",
                               "Unemployment",
                               "Inflation",
                               "Union Density",
                               "GDP Growth",
                               "Human Capital",
                               "Financial Assets",
                               "Trade",
                               "Welfare",
                               "Left Cabinet"))


