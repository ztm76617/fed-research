library(tidyverse) ; library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(OECD); library(scales); library(datawrangling)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(wid)
library(countrycode)
#------------------------------------------------------------------------
manifesto_project_df <- read_csv("~/Google Drive/3 - Misc. Data Research/2 - inequality_redistribution project/2 - Data/manifesto project database.csv")
#------------------------------------------------------------------------
manifesto_project_df_tidy <- manifesto_project_df %>%
  select(countryname, date,
         rile,
         absseat,
         totseats,
         oecdmember,
         per401,
         per402,
         per403,
         per406,
         per410,
         per414,
         per415,
         per504,
         per505,
         per705,
         per3053,
         per4011,
         per5041,
         per5061,
         per201_1) %>%
  rename(year = date,
         country_name = countryname,
         oecd_member_YN = oecdmember,
         party_right_left_idx = rile,
         party_PRO_FREE_MARKET = per401,
         party_PRO_INCENTIVES = per402,
         party_PRO_MARKET_REGULATION = per403,
         party_PRO_PROTECTIONISM = per406,
         party_PRO_ECONOMIC_GROWTH = per410,
         party_PRO_ORTHODOX_ECON = per414,
         party_PRO_MARXIST = per415,
         party_PRO_WELFARE_STATE = per504,
         party_ANTI_WELFARE = per505,
         party_PRO_MINORITY_GROUPS = per705,
         party_ANTI_COMMUNIST = per3053,
         party_PRO_PRIVATIZATION = per4011,
         party_PRO_MIXED_WELFARE_STATE = per5041,
         party_PRO_MIXED_EDUCATION = per5061,
         party_PRO_FREEDOM_INDIVIDUALISM = per201_1) %>%
  mutate(seat_share = absseat/totseats) %>%
  mutate(year = ym(year),
         year = as.numeric(format(year, format ="%Y"))) %>%
  group_by(country_name, year) %>%
  mutate(party_right_left_idx_wt_avg = weighted.mean(party_right_left_idx, seat_share),
         party_PRO_FREE_MARKET_wt_avg = weighted.mean(party_PRO_FREE_MARKET, seat_share),
         party_PRO_INCENTIVES_wt_avg = weighted.mean(party_PRO_INCENTIVES, seat_share),
         party_PRO_MARKET_REGULATION_wt_avg = weighted.mean(party_PRO_MARKET_REGULATION, seat_share),
         party_PRO_PROTECTIONISM_wt_avg = weighted.mean(party_PRO_PROTECTIONISM, seat_share),
         party_PRO_ECONOMIC_GROWTH_wt_avg = weighted.mean(party_PRO_ECONOMIC_GROWTH, seat_share),
         party_PRO_ORTHODOX_ECON_wt_avg = weighted.mean(party_PRO_ORTHODOX_ECON, seat_share),
         party_PRO_MARXIST_wt_avg = weighted.mean(party_PRO_MARXIST, seat_share),
         party_PRO_WELFARE_STATE_wt_avg = weighted.mean(party_PRO_WELFARE_STATE, seat_share),
         party_ANTI_WELFARE_wt_avg = weighted.mean(party_ANTI_WELFARE, seat_share),
         party_PRO_MINORITY_GROUPS_wt_avg = weighted.mean(party_PRO_MINORITY_GROUPS, seat_share),
         party_ANTI_COMMUNIST_wt_avg = weighted.mean(party_ANTI_COMMUNIST, seat_share),
         party_PRO_PRIVATIZATION_wt_avg = weighted.mean(party_PRO_PRIVATIZATION, seat_share),
         party_PRO_MIXED_WELFARE_STATE_wt_avg = weighted.mean(party_PRO_MIXED_WELFARE_STATE, seat_share),
         party_PRO_MIXED_EDUCATION_wt_avg = weighted.mean(party_PRO_MIXED_EDUCATION, seat_share),
         party_PRO_FREEDOM_INDIVIDUALISM_wt_avg = weighted.mean(party_PRO_FREEDOM_INDIVIDUALISM, seat_share)) %>%
  distinct(country_name, year, .keep_all = TRUE) %>%
  arrange(country_name, year) %>%
  mutate(country_code_ISO2 = countrycode(country_name, "country.name", "iso2c")) %>%
  ungroup(country_name, year) %>%
  select(!c(country_name, seat_share, absseat, totseats)) %>%
  select(country_code_ISO2, year, oecd_member_YN, contains("wt_avg")) %>%
  select('country_code_ISO2', 'year', 'oecd_member_YN', sort(colnames(.))) %>%
  rename_with( ~ paste("manif", .x, sep = "_")) %>%
  rename(country_code_ISO2 = manif_country_code_ISO2,
         year = manif_year)
#------------------------------------------------------------------------
write_rds(manifesto_project_df_tidy, "1 - Data/manifesto_project_df_tidy.rds")
#------------------------------------------------------------------------


