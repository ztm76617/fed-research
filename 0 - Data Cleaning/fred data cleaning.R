#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------
# Set API Key
#--------------------------------------------------------------------------------------
fredr_set_key("4ea646af5b37ba1c45a0890c7a04f2dc")
#--------------------------------------------------------------------------------------
# Retrieve Series Observations
#--------------------------------------------------------------------------------------
fed_funds_rate_df <- fredr(series_id = c("FEDFUNDS"),
      observation_start = as.Date("1999-01-01"),
      observation_end = as.Date("2019-12-01"),
      frequency = "m",
      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "FEDFUNDS" ~ "fed_funds_rate"))

discount_window_rate_df <- fredr(series_id = c("MPCREDIT"),
                           observation_start = as.Date("2003-02-01"),
                           observation_end = as.Date("2019-12-01"),
                           frequency = "m",
                           units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "MPCREDIT" ~ "discount_window_primary_credit_rate"))

fixed_mortgage_30yr_df <- fredr(series_id = c("MORTGAGE30US"),
      observation_start = as.Date("1975-02-01"),
      observation_end = as.Date("2019-12-01"),
      frequency = "m",
      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "MORTGAGE30US" ~ "fixed_mortgage_30yr"))

fixed_mortgage_15yr <- fredr(series_id = c("MORTGAGE15US"),
      observation_start = as.Date("1992-01-01"),
      observation_end = as.Date("2019-12-01"),
      frequency = "m",
      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "MORTGAGE15US" ~ "fixed_mortgage_15yr"))

mbs_1to5yr_df <- fredr(series_id = c("MBS1T5"),
                             observation_start = as.Date("2003-01-01"),
                             observation_end = as.Date("2019-12-01"),
                             frequency = "m",
                             units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "MBS1T5" ~ "mbs_1to5yr"))

consumer_distress_idx_df <- fredr(series_id = c("CCDIOAQ156N"),
      observation_start = as.Date("1980-01-01"),
      observation_end = as.Date("2012-12-01"),
      frequency = "q",
      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "CCDIOAQ156N" ~ "consumer_distress_idx"))
#--------------------------------------------------------------------------------------
# Retrieve Multiple Observations
#--------------------------------------------------------------------------------------
fedr_df_v1 <- map_dfr(c("SMU11000000500000003",
                        "DCUCSFRCONDOSMSAMID",
                        "LBSSA11",
                        "DDDI06USA156NWDB",
                        "TOTRA",
                        "LTOTL",
                        "LNCFRBNC",
                        "RAIAIL",
                        "RAFDICS"),
                      fredr,
                      frequency = "a",
                      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "SMU11000000500000003" ~ "avg_hourly_earnings",
                               series_id == "DCUCSFRCONDOSMSAMID" ~ "zillow_home_value_idx",
                               series_id == "LBSSA11" ~ "labor_particip_rate",
                               series_id == "DDDI06USA156NWDB" ~ "pct_gdp_central_bank_assets",
                               series_id == "TOTRA" ~ "fedr_total_assets_resources_millions_USD",
                               series_id == "LTOTL" ~ "fedr_total_liabilities_millions_USD",
                               series_id == "LNCFRBNC" ~ "fedr_liabilities_circ_notes_millions_USD",
                               series_id == "RAIAIL" ~ "fedr_industrial_loans_advances_millions_USD",
                               series_id == "RAFDICS" ~ "fedr_FDIC_stock_millions_USD"),
         year = as.numeric(format(date,'%Y')))
#--------------------------------------------------------------------------------------
# Retrieve Multiple Observations 2
#--------------------------------------------------------------------------------------
fedr_df_v2 <- map_dfr(c("FEDFUNDS",
                        "MBS1T5",
                        "MORTGAGE15US",
                        "MORTGAGE30US",
                        "MPCREDIT"),
                      fredr,
                      frequency = "m",
                      units = "lin") %>%
  select(date, series_id, value) %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "FEDFUNDS" ~ "fed_funds_rate",
                               series_id == "MBS1T5" ~ "mbs_1to5yr",
                               series_id == "MORTGAGE30US" ~ "fixed_mortgage_30yr",
                               series_id == "MPCREDIT" ~ "discount_window_primary_credit_rate",
                               series_id == "MORTGAGE15US" ~ "fixed_mortgage_15yr"),
         year = as.numeric(format(date,'%Y')))

write_rds(fedr_df_v2, "3 - Data/fedr_df_v2.rds")
#--------------------------------------------------------------------------------------
fedr_df_v2_copy <- fedr_df_v2

fedr_df_v2_copy$series_id <- fct_recode(fedr_df_v2_copy$series_id,
                                        "15-Year Fixed Mortage" = "fixed_mortgage_15yr",
                                        "30-Year Fixed Mortage" = "fixed_mortgage_30yr",
                                        "Discount Window Primary Credit Rate" = "discount_window_primary_credit_rate",
                                        "Federal Funds Rate" = "fed_funds_rate",
                                        "Mortage Backed Securities (1yr to 5yr)" = "mbs_1to5yr")

write_rds(fedr_df_v2_copy, "3 - Data/fedr_df_v2_copy.rds")
#--------------------------------------------------------------------------------------
fedr_df_v1 %>%
  pivot_wider(names_from = series_id, values_from = value) %>%
  arrange(year) %>%
  view()

fedr_df_v2 %>%
  pivot_wider(names_from = series_id, values_from = value) %>%
  arrange(year) %>%
  view()

#--------------------------------------------------------------------------------------
# fedr ggplot
#--------------------------------------------------------------------------------------
map_dfr(c("SMU11000000500000003",
          "DCUCSFRCONDOSMSAMID",
          "LBSSA11"),
        fredr,
        frequency = "m",
        units = "pch") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "SMU11000000500000003" ~ "Average Hourly Earnings",
                               series_id == "DCUCSFRCONDOSMSAMID" ~ "Zillow Home Value Index",
                               series_id == "LBSSA11" ~ "Labor Force Participation Rate")) %>%
  filter(date >= "2019-01-01") %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Percent Change (%)",
       title = "Washington, DC Area",
       subtitle = "Monthly Economic Data (2019 - 2022)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "4 month") +
  facet_wrap(vars(series_id))
#--------------------------------------------------------------------------------------
# fedr ggplot
#--------------------------------------------------------------------------------------
map_dfr(c("DCBPPRIVSA"),
        fredr,
        frequency = "m",
        units = "pch") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "DCBPPRIVSA" ~ "New Housing Permits")) %>%
  filter(date >= "2019-01-01") %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Monthly Change",
       title = "Washington, DC Area",
       subtitle = "Monthly Economic Data (2019 - 2022)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "4 month") +
  scale_y_continuous(limits = c(-900, 2850),
                     labels = label_percent(big.mark = ",")) +
  facet_wrap(vars(series_id))
#--------------------------------------------------------------------------------------
map_dfr(c("SMS11000009091000001",
          "DCLEIH",
          "SMU11000006056130001SA"),
        fredr,
        frequency = "m",
        units = "lin") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "SMS11000009091000001" ~ "Federal Governmentt",
                               series_id == "DCLEIH" ~ "Leisure/Hospitality",
                               series_id == "SMU11000006056130001SA" ~ "Business/Professional Services")) %>%
  filter(date == "2022-05-01") %>%
  ggplot(aes(y=value, x=series_id)) + 
  geom_bar(position="dodge", stat="identity", width = 0.4) +
  theme_bw() +
  labs(x = "",
       y = "Number of Employees (Thousands)",
       title = "Washington, DC Area (May 2022)") +
  theme(text = element_text(face = 'bold'))
#--------------------------------------------------------------------------------------
table(fedr_df_v2$series_id)

fedr_df_v2_copy <- fedr_df_v2

fedr_df_v2_copy$series_id <- fct_recode(fedr_df_v2_copy$series_id,
           "15-Year Fixed Mortage" = "fixed_mortgage_15yr",
           "30-Year Fixed Mortage" = "fixed_mortgage_30yr",
           "Discount Window Primary Credit Rate" = "discount_window_primary_credit_rate",
           "Federal Funds Rate" = "fed_funds_rate",
           "Mortage Backed Securities (1yr to 5yr)" = "mbs_1to5yr")


fedr_df_v2_copy %>%
  filter(year >= 1999) %>%
  filter(series_id %in% c('15-Year Fixed Mortage', '30-Year Fixed Mortage')) %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Interest Rate (%)",
       subtitle = "Monthly Economic Data (1999 - 2019)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "24 month") +
  facet_wrap(vars(series_id))
#--------------------------------------------------------------------------------------
fedr_df_v2_copy %>%
  filter(year >= 1970) %>%
  filter(series_id %in% c('Federal Funds Rate')) %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Federal Rate (%)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        axis.title.x = element_text(vjust = -1),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "48 month") +
  geom_vline(xintercept=as.numeric(fedr_df_v2_copy$date[299]), linetype="longdash", color = "red") +
  geom_label(label="Volcker Shock",
             x = as.numeric(fedr_df_v2_copy$date[237]),
             y = 18.5,
             label.padding = unit(0.3, "lines"),
             label.size = 0.15,
             color = "black",
             fill = "#69b3a2")


    
