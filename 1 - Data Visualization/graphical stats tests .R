library(tidyverse)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)
library(nlme)
library(pcse)
#-----------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  select(contains('gdp')) %>%
  names()
#-----------------------------------------------------------------------------------------------
# Fixed effects model, individual effects
lm_m1 <- lm(pwt_real_gdp_growth_10yr_moving_avg ~ marx_rop_total_economy_v1_5yr_ma,
              data = mega_combined_vars_df_SUBSET_added_vars)
#--------------------------------------
# plm_m1 graphical test
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lm_m1)
#-----------------------------------------------------------------------------------------------
# estimating panel corrected standard errors of fixed effects model w/ individual effects
pcse_plm_m1 <- coeftest(plm_m1, vcov = vcovHC, type = "HC1")
#-----------------------------------------------------------------------------------------------
# Fixed effects model, twoway fixed-effects
plm_m2 <- plm(pct_total_socspend_private_socspend ~
                marx_rop_total_economy_v1 +
                cpd_left_right_welfare_issues_wt_avg +
                union_density +
                trade_globalization_de_jure_idx_rescaled_100 +
                GDD_pct_gdp_central_govt_debt +
                pwt_human_capital_index_rescaled_100 +
                WB_WDI_unemployment_rate_national_estimate,
              data = regression_pdata,
              model = "within",
              effect = 'twoways')
#-----------------------------------------------------------------------------------------------
# estimating panel corrected standard errors of random effects (w/ time effects) model 
pcse_plm_m2 <- coeftest(plm_m2, vcov = vcovHC, type = "HC1")
#-----------------------------------------------------------------------------------------------


