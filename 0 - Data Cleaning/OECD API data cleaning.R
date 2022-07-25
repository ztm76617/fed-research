#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(OECD)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
#--------------------------------------------------------------------------------------
get_dataset("EPL_OV",
            filter = list(c("DEU", "FRA"),
                          c("EPRC_V1", "EPRC_V2")),
            start_time = 2008, end_time = 2010) %>%
  rename(iso3 = COUNTRY,
         value = ObsValue,
         variable = SERIES,
         date = Time,
         date_format = TIME_FORMAT)
