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
#--------------------------------------------------------------
KOFGI_df <- read_excel("~/My Drive/3 - Misc. Data Research/Misc. Data/KOFGI/KOFGI_2021_public.xlsx")
#--------------------------------------------------------------
KOFGI_df_tidy <- KOFGI_df %>%
  rename(iso3 = code,
         trade_globalization_de_facto_idx = KOFTrGIdf,
         trade_globalization_de_jure_idx = KOFTrGIdj,
         financial_globalization_de_factor_idx = KOFFiGIdf,
         financial_globalization_de_jure_idx = KOFFiGIdj,
         interpersonal_globalization_de_facto_idx = KOFIpGIdf,
         interpersonal_globalization_de_jure_idx = KOFIpGIdj,
         informational_globalization_de_facto_idx = KOFInGIdf,
         informational_globalization_de_jure_idx = KOFInGIdj,
         cultural_globalization_de_facto_idx = KOFCuGIdf,
         cultural_globalization_de_jure_idx = KOFCuGIdj,
         political_globalization_de_facto_idx = KOFPoGIdf,
         political_globalization_de_jure_idx = KOFPoGIdj) %>%
  select(iso3, year, contains("idx")) %>%
  rename_at(vars(-iso3, -year), function(x) paste0("kofgi_", x))
#--------------------------------------------------------------
write_rds(KOFGI_df_tidy, "1 - Data/1 - Tidy Data/KOFGI_df_tidy.rds")
#--------------------------------------------------------------




