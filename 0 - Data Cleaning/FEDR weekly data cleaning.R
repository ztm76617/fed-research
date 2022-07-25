#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------
fedr_weekly_df <- read_csv("3 - Data/fedr weekly data 1914-2022 (clean-ish).csv")
#--------------------------------------------------------------------------------------
fedr_weekly_df_v1 <- fedr_weekly_df %>%
  mutate_at(c(2:5398), as.numeric) %>%
  pivot_longer(!vars, names_to = "date", values_to = "value")
#--------------------------------------------------------------------------------------
fedr_weekly_df_v2 <- fedr_weekly_df_v1 %>%
  mutate(date = ymd(date)) %>%
  pivot_wider(names_from = vars, values_from = value) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate_if(is.numeric, mean) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(-date) %>%
  select(year, sort(names(.)))
#--------------------------------------------------------------------------------------



