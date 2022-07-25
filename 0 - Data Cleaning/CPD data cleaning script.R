library(tidyverse)
library(stargazer)
library(papeR)
library(panelAR)
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
library(plm)
#-------------------------------------------------------------------------------
vdem_cpd_df <- readRDS("~/Google Drive/3 - Misc. Data Research/4 - top-15-countries-project/1 - Data/V-Dem-CPD-Party-V1.rds")
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
  filter(country_code_ISO3 %in% c("AUS", "BEL", "BRA",
                                  "CAN", "CHE", "CHN",
                                  "DEU", "DNK", "ESP",
                                  "FIN", "FRA", "GBR",
                                  "IND", "IRL", "ITA",
                                  "JPN", "KOR", "MEX",
                                  "NLD", "NOR", "NZL",
                                  "RUS", "SWE", "USA"),
         year %in% c(1960:2019)) %>%
  mutate(welfare_LR_idx = -1*welfare_RL_idx,
         welfare_LR_ord = -1*welfare_RL_ord) %>%
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
write_rds(vdem_cpd_df_tidy, "1 - Data/vdem_cpd_df_tidy.rds")
#-------------------------------------------------------------------------------

