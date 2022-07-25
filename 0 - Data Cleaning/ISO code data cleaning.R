library(tidyverse)
library(ISOcodes)
#-------------------------------------------------
iso_df_15_countries_tidy <- ISO_3166_1 %>%
  select(Name, Alpha_3, Alpha_2) %>%
  filter(Alpha_2 %in% c("AU", "BR", "CA",
                        "CN", "FR", "DE",
                        "IN", "IT", "JP",
                        "KR", "MX", "RU",
                        "ES", "GB", "US")) %>%
  rename(country_code_ISO2 = Alpha_2,
         country_code_ISO3 = Alpha_3,
         country_name = Name) %>%
  mutate(country_name = str_replace_all(country_name,
                                        c("Korea, Republic of" = "South Korea",
                                          "Russian Federation" = "Russia")))
#-------------------------------------------------
write_rds(iso_df_15_countries_tidy, 'iso_df_15_countries_tidy.rds')
#-------------------------------------------------
iso_df_24_countries_tidy <- ISO_3166_1 %>%
  select(Name, Alpha_3, Alpha_2) %>%
  filter(Alpha_2 %in% c("AU", "BR", "CA",
                        "CN", "FR", "DE",
                        "IN", "IT", "JP",
                        "KR", "MX", "RU",
                        "ES", "GB", "US",
                        'NZ', 'NL', 'DK',
                        'BE', 'SE', 'CH',
                        'NO', 'FI', 'IE')) %>%
  rename(country_code_ISO2 = Alpha_2,
         country_code_ISO3 = Alpha_3,
         country_name = Name) %>%
  mutate(country_name = str_replace_all(country_name,
                                        c("Korea, Republic of" = "South Korea",
                                        "Russian Federation" = "Russia")))
#-------------------------------------------------
write_rds(iso_df_24_countries_tidy, 'iso_df_24_countries_tidy.rds')
#-------------------------------------------------
print(iso_df_24_countries_tidy$country_code_ISO3)
#-------------------------------------------------
oecd_top17_ISO_df <- ISO_3166_1 %>%
  select(Name, Alpha_3, Alpha_2) %>%
  filter(Alpha_2 %in% c("AU", "CA", "FR", "DE",
                        "IT", "JP", "ES", "GB",
                        "US", 'NZ', 'NL', 'DK',
                        'BE', 'SE', 'NO', 'FI',
                        'IE')) %>%
  rename(country_code_ISO2 = Alpha_2,
         country_code_ISO3 = Alpha_3,
         country_name = Name)
#-------------------------------------------------
write_rds(oecd_top17_ISO_df, "oecd_top17_ISO_df.rds")
#-------------------------------------------------
full_OECD_country_list_df <- ISO_3166_1 %>%
  select(Name, Alpha_3, Alpha_2) %>%
  filter(Alpha_2 %in% c("AU", "AT", "BE", "CA", "FR", "DE",
                        "ES", 'FI', "FR", "GB", "GR", 'IE',
                        "IL", "IS", "IT", "JP", "KR", "LU",
                        "NL", "NZ", "NO", "PT", "SE", "CH",
                        "US")) %>%
  rename(country_code_ISO2 = Alpha_2,
         country_code_ISO3 = Alpha_3,
         country_name = Name) %>%
  mutate(country_name = str_replace_all(country_name,
                                        c("Korea, Republic of" = "South Korea")))
#-------------------------------------------------
write_rds(full_OECD_country_list_df, "full_OECD_country_list_df.rds")
#-------------------------------------------------

