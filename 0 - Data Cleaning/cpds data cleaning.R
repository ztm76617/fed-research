library(haven)
library(tidyverse)
library(data.table)
library(haven)
library(rio)
library(readxl)
library(zoo)
library(xts)
library(sandwich)
library(lmtest)
library(stargazer)
library(lubridate)
library(reldist)
library(weights)
library(scales)
library(standardize)
library(plm)       
library(car)       
library(gplots)    
library(tseries) 
library(FSA)
library(psych)
library(lme4)
library(lmerTest)
library(nlme)
library(car)
library(foreign)
library(panelAR)
library(papeR)
library(rticles)
library(smooth)
library(TTR)
library(kableExtra)
library(smooth)
library(ggrepel)
library(sjlabelled)
#-----------------------------------------------------------------------------------------
CPDS.df <- read_excel("1 - Data/Extra data/CPDS_1960-2019_Update_2021.xlsx")
#-----------------------------------------------------------------------------------------
names(CPDS.df)

CPDS.df_tidy <- CPDS.df %>%
  select(iso,
         year,
         gov_party,
         dis_abso,
         dis_rel,
         dis_gall,
         rae_ele,
         rae_leg,
         gov_gap,
         vturn,
         womenpar,
         lexe,
         lfed,
         lbic,
         lrid,
         ljud,
         lbank,
         kaopen,
         receipts,
         realgdpgr,
         inflation,
         debt,
         deficit,
         interest,
         labfopar,
         unemp,
         nld,
         strike,
         adjcov,
         socexp_t_pmp,
         educexp_gov,
         prefisc_gini,
         postfisc_gini,
         pretran_gini,
         elderly,
         pop65,
         pop15_64,
         pop,
         educexp_public,
         openc,
         gov_right1,
         gov_right2,
         gov_right3,
         gov_cent1,
         gov_cent2,
         gov_cent3,
         gov_left1,
         gov_left2,
         gov_left3,
         instcons,
         outlays) %>%
  rename(country_code_ISO3 = iso,
         economic_openness_via_trade = openc,
         absolute_disproportionality_idx = dis_abso,
         relative_disproportionality_idx = dis_rel,
         gallagher_disproportionality_idx = dis_gall,
         rae_index_electoral_fractional = rae_ele,
         rae_index_legis_fractional = rae_leg,
         ideological_gap_new_vs_old_govt = gov_gap,
         voter_turnout = vturn,
         pct_parliament_female = womenpar,
         executive_dominance_idx = lexe,
         federalism_idx = lfed,
         bicameralism_idx = lbic,
         constitutional_rigidity_idx = lrid,
         judicial_review_idx = ljud,
         central_bank_independence_idx = lbank,
         economic_openness_idx = kaopen,
         pct_gdp_govt_revenue = receipts,
         real_gdp_growth = realgdpgr,
         inflation_rate = inflation,
         pct_gdp_gross_govt_debt = debt,
         pct_gdp_govt_deficit = deficit,
         long_term_interest_rate_govt_bonds = interest,
         labor_force_partip_rate = labfopar,
         unemployment_rate = unemp,
         number_labor_disputes_strikes = nld,
         labor_strike_activity_idx = strike,
         adj_union_coverage = adjcov,
         pct_gdp_total_social_spending = socexp_t_pmp,
         pct_gdp_govt_education_spending = educexp_gov,
         gini_pre_tax_transfer_income = prefisc_gini,
         gini_post_tax_transfer_income = postfisc_gini,
         gini_pre_transfer_income = pretran_gini,
         elderly_population_thousands = pop65,
         pct_population_elderly = elderly,
         working_age_pop_thousands = pop15_64,
         total_population_thousands = pop,
         pct_gdp_govt_expenditure_public_education = educexp_public,
         central_govt_constraints_idx = instcons,
         pct_gdp_govt_expenditure = outlays,
         ideology_dominance_RL = gov_party,
         pct_govt_cabinet_right_party = gov_right1,
         relative_govt_power_right_party = gov_right2,
         legis_seat_share_right_party = gov_right3,
         pct_govt_cabinet_center_party = gov_cent1,
         relative_govt_power_center_party = gov_cent2,
         legis_seat_share_center_party = gov_cent3,
         pct_govt_cabinet_left_party = gov_left1,
         relative_govt_power_left_party = gov_left2,
         legis_seat_share_left_party = gov_left3) %>%
  arrange(country_code_ISO3, year) %>%
  rename_with( ~ paste("CPDS", .x, sep = "_")) %>%
  rename(country_code_ISO3 = CPDS_country_code_ISO3,
         year = CPDS_year) %>%
  remove_all_labels()
#-----------------------------------------------------------------------------------------
write_rds(CPDS.df_tidy, '1 - Data/CPDS.df_tidy.rds')
#-----------------------------------------------------------------------------------------





