library(tidyverse)
library(ISOcodes)
#-------------------------------------------------
full_OECD_country_list_df <- ISO_3166_1 %>%
  select(Name, Alpha_3, Alpha_2) %>%
  filter(Alpha_2 %in% c("AU", "AT", "BE", "CA", "FR", "DE",
                        "ES", 'FI', "FR", "GB", "GR", 'IE',
                        "IL", "IS", "IT", "JP", "KR", "LU",
                        "NL", "NZ", "NO", "PT", "SE", "CH",
                        "US")) %>%
  rename(iso2 = Alpha_2,
         iso3 = Alpha_3,
         country_name = Name) %>%
  mutate(country_name = str_replace_all(country_name,
                                        c("Korea, Republic of" = "South Korea")))
#-------------------------------------------------
write_rds(full_OECD_country_list_df, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/full_OECD_country_list_df.rds")
#-------------------------------------------------

