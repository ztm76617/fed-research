#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling); library(rticles)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(ggthemr)
library(plm); library(WDI); library(lmtest); library(sandwich); library(interactions); library(rmarkdown);
library(gtable); library(grid); library(gridExtra); library(captioner)
#--------------------------------------------------------------------------------------
# No scientific notation
#--------------------------------------------------------------------------------------
options(scipen = 100)
#--------------------------------------------------------------------------------------
# Country Groupings
#--------------------------------------------------------------------------------------
g7_countries <- c("CAN", "USA", "GBR", "FRA", "JPN", "DEU", "ITA")
#--------------------------------------------------------------------------------------
vdem_cpd_v2 <- readRDS("~/My Drive/3 - Misc. Data Research/Private-Welfare-Project/1 - Data/2 - Raw Data/V-Dem/V-Dem-CPD-Party-V2.rds")
#--------------------------------------------------------------------------------------
names(vdem_cpd_v2)

vdem_cpd_v2 %>%
  arrange(country_name, year) %>%
  select(country_text_id,
         year,
         v2paseatshare,
         v2pavote,
         v2pariglef_osp,
         v2pariglef_ord,
         v2pariglef) %>%
  rename(iso3 = country_text_id,
         cpd_party_seat_share = v2paseatshare,
         cpd_party_vote_share = v2pavote,
         economic_issues_LR = v2pariglef,
         economic_issues_LR_idx = v2pariglef_osp,
         economic_issues_LR_ord = v2pariglef_ord) %>%
  filter(year >= 1960) %>%
  group_by(iso3, year) %>%
  mutate(economic_issues_LR_idx = weighted.mean(economic_issues_LR_idx, cpd_party_seat_share),
         economic_issues_LR_ord = weighted.mean(economic_issues_LR_ord, cpd_party_seat_share),
         economic_issues_LR = weighted.mean(economic_issues_LR, cpd_party_seat_share)) %>%
  distinct(iso3, year, .keep_all = TRUE) %>%
  select(!c(cpd_party_seat_share, cpd_party_vote_share)) %>%
  filter(iso3 == "USA") %>%
  view()





