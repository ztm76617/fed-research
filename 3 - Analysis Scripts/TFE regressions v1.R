#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------
names(mega_combined_vars_df_final)

mega_combined_vars_df_final %>%
  select(contains("education")) %>%
  names()

summary(mega_combined_vars_df_final$oecd_union_density)
#--------------------------------------------------------------------------------------
regression_ready_df_v2 <- mega_combined_vars_df_final %>%
  filter(year >= 1980) %>%
  select(iso3, year,
         pwt_irr,
         wid_pre_tax_income_share_top_1pct,
         wid_ratio_income_share_90th_50th_pctile,
         cpds_unemployment_rate,
         cpds_inflation_rate,
         oecd_union_density,
         pwt_gdp_pc_growth_2017_USD,
         wid_ratio_net_priavte_wealth_to_net_national_income,
         wid_pct_gdp_corporate_nonfinancial_assets,
         wid_pct_gdp_corporate_financial_assets,
         wid_pct_gdp_national_financial_assets,
         pwt_pct_gdp_international_trade,
         cabinet_majority_left,
         oecd_public_socspend_wa_pct_gdp) %>%
  make.pbalanced()

write_rds(regression_ready_df_v1, '~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/regression_ready_df_v1.rds')
#-----------------------------------------------------------------------------------------------
form_m1 <- wid_ratio_income_share_90th_50th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  factor(year)

form_m2 <- wid_ratio_income_share_90th_50th_pctile ~
  pwt_irr +
  cpds_unemployment_rate +
  oecd_union_density +
  pwt_gdp_pc_growth_2017_USD +
  wid_pct_gdp_national_financial_assets +
  pwt_pct_gdp_international_trade +
  oecd_public_socspend_wa_pct_gdp +
  cabinet_majority_left +
  factor(year)
#-----------------------------------------------------------------------------------------------
# Models
#-----------------------------------------------------------------------------------------------
lm_m1 <- lm(form_m1, data = regression_ready_df_v2)
lm_m2 <- lm(form_m2, data = regression_ready_df_v2)
#-----------------------------------------------------------------------------------------------
# Robust Standard Error Objects
#-----------------------------------------------------------------------------------------------
hc_se_lm_m1 <- sqrt(diag(vcovHC(lm_m1, type = "HC1")))
hc_se_lm_m2 <- sqrt(diag(vcovHC(lm_m2, type = "HC1")))
#-----------------------------------------------------------------------------------------------
# PCSE Standard Error Objects
#-----------------------------------------------------------------------------------------------
pcse_se_lm_m1 <- sqrt(diag(vcovPC(lm_m1, cluster = ~ iso3 + year)))
pcse_se_lm_m2 <- sqrt(diag(vcovPC(lm_m2, cluster = ~ iso3 + year)))
#-----------------------------------------------------------------------------------------------
# Creating List of LM Objects
#-----------------------------------------------------------------------------------------------
lm_group <- list(lm_m1, lm_m2)
#-----------------------------------------------------------------------------------------------
hc.se_group <- list(hc_se_lm_m1, hc_se_lm_m2)
#-----------------------------------------------------------------------------------------------
pcse.se_group <- list(pcse_se_lm_m1, pcse_se_lm_m2)
#-----------------------------------------------------------------------------------------------
stargazer(lm_group,
          se = c(pcse.se_group),
          type = "text",
          omit = c("factor\\(year\\)"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          dep.var.caption = "Top 1% Income Share",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          header = FALSE,
          no.space = TRUE)


