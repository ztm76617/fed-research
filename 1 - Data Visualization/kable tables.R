library(tidyverse)
library(stargazer)
library(kableExtra)
#-------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars <- read_csv("mega_combined_vars_df_SUBSET_added_vars.csv")
#-------------------------------------------------------------
# % GDP of random vars table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_GFCF_pct_gdp,
         pwt_employee_labor_share_gdp) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "RUS", "ESP", "GBR", "USA")) %>%
  rename('Fixed Capital Investment' = pwt_GFCF_pct_gdp,
         'Labor Income' = pwt_employee_labor_share_gdp,
         Country = country_code_ISO3) %>%
  filter(year == 2018) %>%
  select(!year) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Percent of 2018 GDP (%)" = 2)) %>%
  column_spec(2:3, width = '2in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-------------------------------------------------------------
# income inequality table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         WID_share_top_0.1pct_pre_tax_income_joint,
         WID_share_top_1pct_pre_tax_income_joint,
         WID_share_bottom_50pct_pre_tax_income_joint) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "RUS", "ESP", "GBR", "USA")) %>%
  arrange(WID_share_top_0.1pct_pre_tax_income_joint) %>%
  rename('Top 0.1%' = WID_share_top_0.1pct_pre_tax_income_joint,
         'Top 1%' = WID_share_top_1pct_pre_tax_income_joint,
         'Bottom 50%' = WID_share_bottom_50pct_pre_tax_income_joint,
         Country = country_code_ISO3) %>%
  filter(year == 2018) %>%
  select(!year) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 15) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Share of National Income (2018)" = 3)) %>%
  column_spec(2:4, width = '1.25in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-------------------------------------------------------------
# ROP vs IRR table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         marx_rop_total_economy_v1,
         pwt_internal_rate_return) %>%
  mutate(pwt_internal_rate_return = pwt_internal_rate_return*100) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "RUS", "ESP", "GBR", "USA")) %>%
  rename('Marxist ROP' = marx_rop_total_economy_v1,
         'IRR' = pwt_internal_rate_return,
         Country = country_code_ISO3) %>%
  filter(year %in% c(1970, 1985, 2000, 2015)) %>%
  select(!year) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Average Return on Capital Investment (2015)" = 2)) %>%
  column_spec(2:3, width = '1.75in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")

#-----------------------------------------------------------------------
# ROP over time table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         marx_rop_total_economy_v1) %>%
  filter(country_code_ISO3 %in% c("CAN", "FRA", "DEU", "JPN", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1985, 2000, 2015)) %>%
  pivot_wider(names_from = year, values_from = marx_rop_total_economy_v1) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Marxist Rate of Profit (%)" = 4)) %>%
  column_spec(2:5, width = '1in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-----------------------------------------------------------------------
# IRR over time table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_internal_rate_return_number) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1985, 2000, 2015)) %>%
  pivot_wider(names_from = year, values_from = pwt_internal_rate_return_number) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Internal Rate of Return (%)" = 4)) %>%
  column_spec(2:5, width = '1in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-----------------------------------------------------------------------
# Labor share of income over time table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_labor_share_gdp_number) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1985, 2000, 2015)) %>%
  pivot_wider(names_from = year, values_from = pwt_labor_share_gdp_number) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Labor Income (% GDP)" = 4)) %>%
  column_spec(2:5, width = '1in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-----------------------------------------------------------------------
# Labor share of income over time table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_labor_share_gdp_number) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  
  filter(year %in% c(1970, 1975, 1980, 1985, 1990,
                     1995, 2000, 2005, 2010, 2015)) %>%
  pivot_wider(names_from = year, values_from = pwt_labor_share_gdp_number) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = T,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Labor Income (% GDP)" = 10)) %>%
  column_spec(2:11, width = '1in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-----------------------------------------------------------------------
# IRR over time table
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_internal_rate_return_number) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1975, 1980, 1985, 1990,
                     1995, 2000, 2005, 2010, 2015)) %>%
  pivot_wider(names_from = year, values_from = pwt_internal_rate_return_number) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Internal Rate of Return (%)" = 10)) %>%
  column_spec(2:11, width = '1in') %>%
  footnote(general = "Data Source(s): Penn World Tables & World Inequality Database")
#-----------------------------------------------------------------------
# IRR: 1970 vs. 2019 comparison
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_internal_rate_return_number) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "DEU", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 2019)) %>%
  group_by(country_code_ISO3) %>%
  mutate(irr_pct_change_1970_to_2019 = pct_change_function(pwt_internal_rate_return_number)) %>%
  filter(year == 2019) %>%
  select(country_code_ISO3, irr_pct_change_1970_to_2019)
#-----------------------------------------------------------------------
# Left-Right position re: Economic Issues over time
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         cpd_left_right_economic_issues_wt_avg) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>%
  pivot_wider(names_from = year, values_from = cpd_left_right_economic_issues_wt_avg) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Left-Right Position (Economic Issues)" = 6)) %>%
  column_spec(2:7, width = '1in') %>%
  footnote(general = "Data Source(s): Comparative Politics Database")
#-----------------------------------------------------------------------
# Left-Right position re: Welfare Issues over time
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         cpd_left_right_welfare_issues_wt_avg) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>%
  pivot_wider(names_from = year, values_from = cpd_left_right_welfare_issues_wt_avg) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Left-Right Position (Welfare Issues)" = 6)) %>%
  column_spec(2:7, width = '1in') %>%
  footnote(general = "Data Source(s): Comparative Politics Database")
#-----------------------------------------------------------------------
# GFCF % GDP (40 year window)
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_GFCF_pct_gdp) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(2019, 1979)) %>%
  pivot_wider(names_from = year, values_from = pwt_GFCF_pct_gdp) %>%
  select(country_code_ISO3, '1979', '2019') %>%
  arrange(desc(`2019`)) %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Gross Fixed Capital Formation (% GDP)" = 2)) %>%
  column_spec(2:3, width = '1.5in') %>%
  footnote(general = "Data Source(s): Penn World Tables")
#-----------------------------------------------------------------------
# GFCF % GDP (40 year window)
sorted_table_df <- mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_GFCF_pct_gdp,
         pwt_employee_labor_share_gdp) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(2019, 1979)) %>%
  mutate(pwt_GFCF_pct_gdp = pct_change_function(pwt_GFCF_pct_gdp),
         pwt_employee_labor_share_gdp= pct_change_function(pwt_employee_labor_share_gdp)) %>%
  pivot_wider(names_from = year, values_from = c(pwt_GFCF_pct_gdp, pwt_employee_labor_share_gdp))

sorted_table_df[order(sorted_table_df$`2019`, decreasing = TRUE),] %>%
  rename(Country = country_code_ISO3,
         '% Change from 1979 to 2019' = '2019') %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Gross Fixed Capital Formation (% GDP)" = 1)) %>%
  column_spec(1:2, width = '1.25in') %>%
  footnote(general = "Data Source(s): Penn World Tables")
#-----------------------------------------------------------------------
# HCI (40 year window)
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_human_capital_index) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(2019, 1999, 1979)) %>%
  arrange(desc(pwt_human_capital_index)) %>%
  pivot_wider(names_from = year, values_from = pwt_human_capital_index) %>%
  select(country_code_ISO3, '1979', '1999', '2019') %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Human Capital Index" = 3)) %>%
  column_spec(2:4, width = '1.25in') %>%
  footnote(general = "Data Source(s): Penn World Tables")
#-----------------------------------------------------------------------
# Human Capital Index (% change every 20 years)
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(country_code_ISO3) %>%
  arrange(country_code_ISO3, year) %>%
  select(country_code_ISO3, year,
         pwt_human_capital_index) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(2019, 1999, 1979)) %>%
  mutate(pct_change_HCI = pct_change_function(pwt_human_capital_index)) %>%
  filter(year %in% c(2019, 1999)) %>%
  select(-pwt_human_capital_index) %>%
  arrange(desc(pct_change_HCI)) %>%
  pivot_wider(names_from = year, values_from = pct_change_HCI) %>%
  select(country_code_ISO3, '1999', '2019') %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Human Capital Index (% Growth from 20 Years Prior)" = 2)) %>%
  column_spec(2:3, width = '2in') %>%
  footnote(general = "Data Source(s): Penn World Tables")
#-----------------------------------------------------------------------
# Life expectancy (% change every 20 years)
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(country_code_ISO3) %>%
  arrange(country_code_ISO3, year) %>%
  select(country_code_ISO3, year,
         life_expectancy_total) %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year %in% c(2017, 1997, 1977)) %>%
  mutate(pct_change_life_expectancy_total = pct_change_function(life_expectancy_total)) %>%
  filter(year %in% c(2017, 1997)) %>%
  select(-life_expectancy_total) %>%
  arrange(desc(pct_change_life_expectancy_total)) %>%
  pivot_wider(names_from = year, values_from = pct_change_life_expectancy_total) %>%
  select(country_code_ISO3, '1997', '2017') %>%
  rename(Country = country_code_ISO3) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Life Expectancy (% Growth from 20 Years Prior)" = 2)) %>%
  column_spec(2:3, width = '2in') %>%
  footnote(general = "Data Source(s): OECD General Statistics")
#-----------------------------------------------------------------------
# GFCF % GDP (40 year window)
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year == 2019) %>%
  select(country_code_ISO3,
         pwt_ratio_GFCF_vs_NFCF,
         pwt_GFCF_pct_gdp,
         pwt_pct_gdp_net_fixed_capital_formation_number) %>%
  arrange(pwt_ratio_GFCF_vs_NFCF) %>%
  rename(Country = country_code_ISO3,
         'Ratio: GFCF vs. NFCF' = pwt_ratio_GFCF_vs_NFCF,
         'Gross Investment' = pwt_GFCF_pct_gdp,
         'Net Investment' = pwt_pct_gdp_net_fixed_capital_formation_number) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries (2019)",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Fixed Capital Investment (% GDP)" = 3)) %>%
  column_spec(2:4, width = '1.5in') %>%
  footnote(general = "Data Source(s): Penn World Tables")
#-----------------------------------------------------------------------
# ICOR_v2 table (2019)
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "CHN", "FRA", "JPN",
                                  "MEX", "ESP", "GBR", "USA")) %>%
  filter(year == 2019) %>%
  select(country_code_ISO3,
         pwt_icor_v2) %>%
  arrange(pwt_icor_v2) %>%
  rename(Country = country_code_ISO3,
         'ICOR Number' = pwt_icor_v2) %>%
  kbl(caption = "Comparing Economic Coniditons Across Countries (2019)",
      booktabs = T,
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_classic(full_width = F,
                html_font = "Cambria") %>%
  footnote(general = "Data Source(s): Penn World Tables")
#---------------------------------------------------------------------

socspend_table_df_2018 <- regression_ready_df %>%
  select(country_name, year,
         pct_gdp_private_socspend_fullpop,
         pct_gdp_public_socspend_fullpop,
         pct_gdp_total_socspend_fullpop) %>%
  filter(year == 2018) %>%
  na.omit()

num_1_12 <- c(1:12)

socspend_table_df_2018_rdy <- socspend_table_df_2018 %>%
  select(country_name,
         pct_gdp_public_socspend_fullpop,
         pct_gdp_private_socspend_fullpop,
         pct_gdp_total_socspend_fullpop) %>%
  arrange(desc(pct_gdp_public_socspend_fullpop)) %>%
  cbind(num_1_12) %>%
  rename("Public Rank" = num_1_12) %>%
  arrange(desc(pct_gdp_private_socspend_fullpop)) %>%
  cbind(num_1_12) %>%
  rename("Private Rank" = num_1_12) %>%
  arrange(desc(pct_gdp_total_socspend_fullpop)) %>%
  cbind(num_1_12) %>%
  rename("Total Rank" = num_1_12) %>%
  rename(Country = country_name,
         Public = pct_gdp_public_socspend_fullpop,
         Private = pct_gdp_private_socspend_fullpop,
         Total = pct_gdp_total_socspend_fullpop) %>%
  arrange(desc(Public))

kbl(socspend_table_df_2018_rdy, booktabs = T,
    caption = "Social Spending Type by Country",
    digits = 2) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scaledown"))  %>%
  add_header_above(c("", "Percent of GDP (2018)" = 3, "", "", ""))
#---------------------------------------------------------------------
socspend_table_df_2017 <- regression_ready_df %>%
  select(country_name, iso3, year,
         pct_gdp_private_welfare,
         pct_gdp_public_welfare,
         pct_gdp_total_welfare) %>%
  filter(year == 2017) %>%
  na.omit() %>%
  filter(iso3 %in% c("CAN", "CHE", "DEU", "FIN", "FRA",
                     "GBR", "NLD", "NOR", "SWE", "USA"))
#---------------------------------------------------------------------
num_1_10 <- c(1:10)
#---------------------------------------------------------------------
socspend_table_df_2017_rdy <- socspend_table_df_2017 %>%
  select(country_name,
         pct_gdp_public_welfare,
         pct_gdp_private_welfare,
         pct_gdp_total_welfare) %>%
  arrange(desc(pct_gdp_public_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Public Rank" = num_1_10) %>%
  arrange(desc(pct_gdp_private_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Private Rank" = num_1_10) %>%
  arrange(desc(pct_gdp_total_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Total Rank" = num_1_10) %>%
  rename(Country = country_name,
         Public = pct_gdp_public_welfare,
         Private = pct_gdp_private_welfare,
         Total = pct_gdp_total_welfare) %>%
  arrange(desc(Public))
#---------------------------------------------------------------------
kbl(socspend_table_df_2017_rdy, booktabs = T, digits = 2, caption = "Cross-National Levels ofn Social Spending") %>%
  kable_styling(latex_options = c("striped", "bordered", "condensed", "HOLD_position")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Percent of GDP (2017)" = 3, "", "", "")) %>%
  footnote(general = "OECD Social Protection Database",
           general_title = "Data Source(s):",
           footnote_as_chunk = T, title_format = c("italic"))
#---------------------------------------------------------------------
wa_socspend_table_df_2017 <- regression_ready_df %>%
  select(country_name, iso3, year,
         pct_gdp_total_wa_welfare,
         pct_gdp_wa_private_welfare,
         pct_gdp_wa_public_welfare) %>%
  filter(year == 2017) %>%
  na.omit() %>%
  filter(iso3 %in% c("CAN", "CHE", "DEU", "FIN", "FRA",
                     "GBR", "NLD", "NOR", "SWE", "USA"))
#---------------------------------------------------------------------
num_1_10 <- c(1:10)
#---------------------------------------------------------------------
wa_socspend_table_df_2017_rdy <- wa_socspend_table_df_2017 %>%
  select(country_name,
         pct_gdp_wa_private_welfare,
         pct_gdp_wa_public_welfare,
         pct_gdp_total_wa_welfare) %>%
  arrange(desc(pct_gdp_wa_private_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Private Rank" = num_1_10) %>%
  arrange(desc(pct_gdp_wa_public_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Public Rank" = num_1_10) %>%
  arrange(desc(pct_gdp_total_wa_welfare)) %>%
  cbind(num_1_10) %>%
  rename("Total Rank" = num_1_10) %>%
  rename(Country = country_name,
         Public = pct_gdp_wa_public_welfare,
         Private = pct_gdp_wa_private_welfare,
         Total = pct_gdp_total_wa_welfare) %>%
  arrange(desc(Public))
#---------------------------------------------------------------------
kbl(wa_socspend_table_df_2017_rdy, booktabs = T, digits = 2, caption = "Cross-National Working Age Social Spending") %>%
  kable_styling(latex_options = c("striped", "bordered", "condensed", "HOLD_position")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Percent of GDP (2017)" = 3, "", "", "")) %>%
  footnote(general = "OECD Social Protection Database",
           general_title = "Data Source(s):",
           footnote_as_chunk = T, title_format = c("italic"))
#---------------------------------------------------------------------
