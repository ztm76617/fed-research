#--------------------------------------------------------------------------------------
# Load packages
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggpubr); library(gplots); library(gridExtra)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(scales); library(IndexNumR); library(wid); library(scales); library(datawrangling); library(rticles)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(stevetemplates); library(grid)
library(plm); library(WDI); library(lmtest); library(sandwich); library(interactions); library(rmarkdown); library(gtable)
#--------------------------------------------------------------------------------------
wb_income_share_df <- read_excel("~/My Drive/3 - Misc. Data Research/Misc. Data/ICPSR/Data and Code for- Why Is Europe More Equal Than the United States?/macro-data/wb-tax-data/WB-tax-income-share.xls", skip = 2)
wb_tax_revenue_df <- read_excel("~/My Drive/3 - Misc. Data Research/Misc. Data/ICPSR/Data and Code for- Why Is Europe More Equal Than the United States?/macro-data/wb-tax-data/WB-tax-revenue-gdp.xls", skip = 2)
revealing_hidden_welfare_df <- read_excel("~/My Drive/3 - Misc. Data Research/Misc. Data/Dataverse/Revealing Hidden Welfare State/revealing-hidden-welfare-data.xlsx")
#--------------------------------------------------------------------------------------
table(wb_income_share_df$`Indicator Name`)
table(wb_tax_revenue_df$`Indicator Name`)
table(oecd_sna_df$Transaction)
