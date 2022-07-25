library(tidyverse)
library(stargazer)
library(papeR)
library(ggrepel)
library(ggpubr)
library(gplots)   
library(kableExtra)
library(car)
library(zoo)
library(rio)
library(readxl)
library(haven)
library(lubridate)
library(scales)
library(IndexNumR)
library(wid)
library(OECD)
library(scales)
library(datawrangling)
library(stats)
library(smooth)
library(tm)
library(TTR)
library(naniar)
library(stevetemplates)
library(countrycode)
#-------------------------------------------------------------------------
vdem_full_df <- read_csv("1 - Data/Extra data/11- V-Dem/V-Dem-CY-Full+Others-v11.1.csv")

write_rds(vdem_full_df, "1 - Data/Extra data/11- V-Dem/V-Dem-CY-Full+Others-v11.1.rds")

vdem_cpd_df <- readRDS("~/Google Drive/3 - Misc. Data Research/4 - top-15-countries-project/1 - Data/V-Dem-CPD-Party-V1.rds")

vdem_full_df <- `V-Dem-CY-Full+Others-v11.1`
#-------------------------------------------------------------------------
vdem_main_df_tidy <- vdem_full_df %>%
  filter(year >= 1960) %>%
  select(country_text_id,
         COWcode,
         year,
         v2x_regime,
         v2elmulpar,
         v2elvaptrn,
         v2pscohesv,
         v2ddlexci,
         v2ddlexrf,
         v2lgdsadlo,
         v2dlengage,
         v2dlencmps,
         v2dlunivl,
         v2pepwrses,
         v2peedueq,
         v2pehealth,
         v2peapsecon,
         v2exl_legitideol,
         v2exl_legitideolcr_0,
         v2exl_legitideolcr_1,
         v2exl_legitideolcr_2,
         v2cacamps,
         v2cagenmob,
         v2catrauni,
         v2xps_party,
         v2x_divparctrl,
         v2smpolsoc,
         e_boix_regime,
         v2psnatpar,
         v2psnatpar_ord,
         v2psnatpar_osp,
         e_miurbani,
         v2x_polyarchy,
         v2x_libdem,
         v2x_accountability) %>%
  rename(regime_type = v2x_regime, 
         multipart_elections = v2elmulpar,
         vap_voter_turnout = v2elvaptrn,
         legislative_party_cohesion = v2pscohesv,
         initiatives_permitted = v2ddlexci,
         referendums_permitted = v2ddlexrf,
         representation_disadvtg_social_groups = v2lgdsadlo,
         society_pols_engagement = v2dlengage,
         particular_vs_public_goods = v2dlencmps,
         means_tested_vs_universal_policies = v2dlunivl,
         equal_resource_distribution_idx = v2pepwrses,
         educational_equality = v2peedueq,
         health_equality = v2pehealth,
         public_service_access_distribution = v2peapsecon,
         govt_ideology = v2exl_legitideol,
         govt_ideology_character_nationalist = v2exl_legitideolcr_0,
         govt_ideology_character_socialist_communist = v2exl_legitideolcr_1,
         govt_ideology_character_conservative = v2exl_legitideolcr_2,
         political_polarization = v2cacamps,
         mass_mobilization_events = v2cagenmob,
         trade_union_density = v2catrauni,
         party_institutionalization_idx = v2xps_party,
         divided_govt_control = v2x_divparctrl,
         societal_polarization = v2smpolsoc,
         democratic_govt = e_boix_regime,
         national_party_control = v2psnatpar,
         national_party_control_ordinal = v2psnatpar_ord,
         national_party_control_osp = v2psnatpar_osp,
         urbanization = e_miurbani,
         electoral_democracy_idx = v2x_polyarchy,
         liberal_democracy_idx = v2x_libdem,
         govt_accountability_idx = v2x_accountability) %>%
  mutate(country_code_ISO2 = countrycode(COWcode, "cown", "iso2c"),
         country_code_ISO3 = countrycode(country_code_ISO2, "iso2c", "iso3c")) %>%
  select('country_text_id', 'COWcode','country_code_ISO3', 'country_code_ISO2', 'year', sort(colnames(.))) %>%
  select(!c(country_text_id, COWcode, country_code_ISO2)) %>%
  arrange(country_code_ISO3, year) %>%
  rename_with( ~ paste("vdem", .x, sep = "_")) %>%
  rename(country_code_ISO3 = vdem_country_code_ISO3,
         year = vdem_year)
#-------------------------------------------------------------------------
write_rds(vdem_main_df_tidy, '1 - Data/vdem_main_df_tidy.rds')
#-------------------------------------------------------------------------------
vdem_cpd_df_tidy <- vdem_cpd_df %>%
  arrange(country_name, year) %>%
  select(country_text_id,
         year,
         v2paseatshare,
         v2pavote,
         v2pariglef_osp,
         v2pariglef_ord,
         v2pawelf_osp,
         v2pawelf_ord) %>%
  rename(country_code_ISO3 = country_text_id,
         cpd_party_seat_share = v2paseatshare,
         cpd_party_vote_share = v2pavote,
         economic_issues_LR_idx = v2pariglef_osp,
         economic_issues_LR_ord = v2pariglef_ord,
         welfare_RL_idx = v2pawelf_osp,
         welfare_RL_ord = v2pawelf_ord) %>%
  filter(year %in% c(1960:2019)) %>%
  mutate(welfare_LR_idx = (-1)*welfare_RL_idx,
         welfare_LR_ord = (-1)*welfare_RL_ord) %>%
  group_by(country_code_ISO3, year) %>%
  mutate(economic_issues_LR_idx_wt_avg = weighted.mean(economic_issues_LR_idx, cpd_party_seat_share),
         economic_issues_LR_ord_wt_avg = weighted.mean(economic_issues_LR_ord, cpd_party_seat_share),
         welfare_LR_idx_wt_avg = weighted.mean(welfare_LR_idx, cpd_party_seat_share),
         welfare_LR_ord_wt_avg = weighted.mean(welfare_LR_ord, cpd_party_seat_share)) %>%
  select(country_code_ISO3, year, contains('wt_avg')) %>%
  distinct(country_code_ISO3, year, .keep_all = TRUE) %>%
  rename_with( ~ paste("vdem", .x, sep = "_")) %>%
  rename(country_code_ISO3 = vdem_country_code_ISO3,
         year = vdem_year)
#-------------------------------------------------------------------------------
write_rds(vdem_cpd_df_tidy, '1 - Data/vdem_cpd_df_tidy.rds')
#-------------------------------------------------------------------------------
vdem_full_tidy <- vdem_main_df_tidy %>%
  left_join(vdem_cpd_df_tidy, by = c('country_code_ISO3', 'year'))
#-------------------------------------------------------------------------------
write_rds(vdem_full_tidy, '1 - Data/vdem_full_tidy.rds')
#-------------------------------------------------------------------------------

         










