#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(fredr)
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
#--------------------------------------------------------------------------------------
fedr_df_final <- map_dfr(c("FEDFUNDS",
                        "MBS1T5",
                        "MORTGAGE15US",
                        "MORTGAGE30US",
                        "MPCREDIT",
                        "USSLIND",
                        "NFCI",
                        "AAA",
                        "T10Y2Y",
                        "AAA10Y",
                        "BAA10Y",
                        "JHDUSRGDPBR",
                        "JHGDPBRINDX",
                        "OECDNMERECDM",
                        "SMU11000000500000003",
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
         series_id = case_when(series_id == "FEDFUNDS" ~ "fed_funds_rate",
                               series_id == "MBS1T5" ~ "mbs_1to5yr",
                               series_id == "MORTGAGE30US" ~ "fixed_mortgage_30yr",
                               series_id == "MPCREDIT" ~ "discount_window_primary_credit_rate",
                               series_id == "MORTGAGE15US" ~ "fixed_mortgage_15yr",
                               series_id == "USSLIND" ~ "leading_idx",
                               series_id == "NFCI" ~ "chic_fedr_financial_conditions_idx",
                               series_id == "AAA" ~ "moodys_aaa_bond_yield_pct",
                               series_id == "T10Y2Y" ~ "ten_yr_treasury_bond_minus_2yr_constant_maturity",
                               series_id == "AAA10Y" ~ "spread_moodys_aaa_bond_vs_10yr_treasury",
                               series_id == "BAA10Y" ~ "spread_moodys_baa_bond_vs_10yr_treasury",
                               series_id == "JHDUSRGDPBR" ~ "us_recession_01_gdp_based",
                               series_id == "JHGDPBRINDX" ~ "us_recession_idx",
                               series_id == "OECDNMERECDM" ~ "global_recession_01_gdp_based",
                               series_id == "SMU11000000500000003" ~ "avg_hourly_earnings",
                               series_id == "DCUCSFRCONDOSMSAMID" ~ "zillow_home_value_idx",
                               series_id == "LBSSA11" ~ "labor_particip_rate",
                               series_id == "DDDI06USA156NWDB" ~ "pct_gdp_central_bank_assets",
                               series_id == "TOTRA" ~ "fedr_total_assets_resources_millions_USD",
                               series_id == "LTOTL" ~ "fedr_total_liabilities_millions_USD",
                               series_id == "LNCFRBNC" ~ "fedr_liabilities_circ_notes_millions_USD",
                               series_id == "RAIAIL" ~ "fedr_industrial_loans_advances_millions_USD",
                               series_id == "RAFDICS" ~ "fedr_FDIC_stock_millions_USD"),
         year = as.numeric(format(date,'%Y'))) %>%
  pivot_wider(names_from = series_id, values_from = value) %>%
  arrange(year)
#--------------------------------------------------------------------------------------
write_rds(fedr_df_final, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/fedr_df_final.rds")
#--------------------------------------------------------------------------------------
fedr_df_final_long <- fedr_df_final %>%
  select(-date) %>%
  pivot_longer(!year, names_to = "var", values_to = "value") %>%
  arrange(var, year)
#--------------------------------------------------------------------------------------
write_rds(fedr_df_final_long, "~/Google Drive/My Drive/3 - Misc. Data Research/Edited Data/fedr_df_final_long.rds")
#--------------------------------------------------------------------------------------
fedr_df_final %>%
  filter(year >= 1980) %>%
ggplot(mapping = aes(x = year)) +
  geom_line(aes(y = fed_funds_rate)) +
  geom_line(aes(y = spread_moodys_aaa_bond_vs_10yr_treasury), linetype = "dashed") +
  geom_line(aes(y = spread_moodys_baa_bond_vs_10yr_treasury), linetype = "dotted") +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
#--------------------------------------------------------------------------------------
fedr_df_final %>%
  filter(year >= 1980) %>%
  select(year, leading_idx, spread_moodys_aaa_bond_vs_10yr_treasury) %>%
  pivot_longer(!year, names_to = "var", values_to = "value") %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_line(aes(linetype = var)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        text = element_text(face = 'bold'),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_linetype_discrete(labels=c('Leading Index', "AAA Bond vs. Treasury Bond Spread"))
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
#--------------------------------------------------------------------------------------



    
