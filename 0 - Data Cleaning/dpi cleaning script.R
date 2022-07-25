library(tidyverse)
library(ggrepel)
library(ggpubr)
library(gplots)   
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
library(WDI)
library(scales)
library(datawrangling)
library(data.table)
#--------------------------------------------------------------------------
DPI_df <- read_dta("1 - Data/Extra data/DPI2020/DPI2020.dta") %>%
  rename(country_name = countryname,
         country_code_ISO3 = ifs)

remove(DPI_df)
#--------------------------------------------------------------------------
DPI_df_tidy <- DPI_df %>%
  select(country_code_ISO3,
         year,
         system,
         prtyin,
         execrlc,
         execnat,
         execrel,
         allhouse,
         totalseats,
         herfgov,
         herfopp,
         herftot,
         numgov,
         numvote,
         numopp,
         oppvote,
         pr,
         pluralty,
         select,
         fraud,
         tensys,
         checks,
         polariz,
         auton) %>%
  rename(DPI_gov_system = system,
         DPI_chief_exec_time_office = prtyin,
         DPI_left_right_chief_exec_party = execrlc,
         DPI_nationalist_chief_exec_party = execnat,
         DPI_religious_chief_exec_party = execrel,
         DPI_chief_exec_control_all_legis_house_Y_N = allhouse,
         DPI_total_seats_legislature = totalseats,
         DPI_herfindahl_index_govt_party = herfgov,
         DPI_herfindahl_index_opposition_party = herfopp,
         DPI_herfindahl_index_total = herftot,
         DPI_number_govt_seats = numgov,
         DPI_vote_share_govt_parties = numvote,
         DPI_number_opposition_seats = numopp,
         DPI_vote_share_opposition_parties = oppvote,
         DPI_proportionality_Y_N = pr,
         DPI_plurality_Y_N = pluralty,
         DPI_candiate_selection = select,
         DPI_election_fraud_Y_N = fraud,
         DPI_duration_govt_system = tensys,
         DPI_govt_checks = checks,
         DPI_max_polarization_exec_party_vs_legis_parties = polariz,
         DPI_autonomous_regions_Y_N = auton) %>%
  replace_with_na_at(.vars = c(3:24),
                     condition = ~.x == -999)
#----------------------------------------------------------------
write_rds(DPI_df_tidy, '1 - Data/DPI_df_tidy.rds')
#----------------------------------------------------------------

print(iso_df_24_countries_tidy$country_code_ISO3)







