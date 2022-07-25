#-----------------------------------------------------------
# Custom ggplot theme 1
#-----------------------------------------------------------
theme_ben <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}
#-----------------------------------------------------------
# Custom ggplot theme FACET
#-----------------------------------------------------------
ggplot_theme_FACET <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(text = element_text(face = 'bold'),
          axis.text.x = element_text(size = rel(1), angle = 35),
          panel.background = element_rect(colour = "black"),
          strip.text.x = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = rel(0.9)),
          plot.caption.position = "plot",
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    )
}

ggplot1 + ggplot_theme_FACET()
#-----------------------------------------------------------
# Custom ggplot theme NO FACET
#-----------------------------------------------------------
ggplot_theme_NON_FACET <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(text = element_text(face = 'bold'),
          axis.text.x = element_text(size = rel(1)),
          panel.background = element_rect(colour = "black"),
          strip.text.x = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = rel(0.9)),
          plot.caption.position = "plot",
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    )
}

ggplot2 + ggplot_theme_NON_FACET()
#-----------------------------------------------------------
# Round function
round_function <- function(x){
  round(x, digits = 2)
}
#-----------------------------------------------------------
# Times 100
x100_func <- function(x){
  (x*100)
}
#-----------------------------------------------------------
# annual growth function
pct_change_function <- function(x){
  ((x - dplyr::lag(x))/x) * 100
}
#-----------------------------------------------------------
# 4 year moving average function
four_yr_moving_avg_func <- function(x){
  rollmean(x, k = 4, fill = NA)
}
#-----------------------------------------------------------
# 5 year moving average function
five_yr_moving_avg_func <- function(x){
  rollmean(x, k = 5, fill = NA)
}
#-----------------------------------------------------------
# 8 year moving average function
eight_yr_moving_avg_func <- function(x){
  rollmean(x, k = 8, fill = NA)
}
#-----------------------------------------------------------
# 10 year moving average function
ten_yr_moving_avg_func <- function(x){
  rollmean(x, k = 10, fill = NA)
  }
#------------------------------------------------------------
# times a million (usually for OECD data)
times_one_million_function <- function(x){(x*1e6)}
#-----------------------------------------------------------------------------------
# Deflate LCU at current prices and then convert to 2019 PPPs
deflate_plus_ppp_convert_func <- function(x){
  (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019
}
#-----------------------------------------------------------------------------------
# Deflate LCU at current prices and then convert to USD using 2019 market exchange rates
deflate_plus_market_exch_convert_func <- function(x){
  (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019
}
#-----------------------------------------------------------------------------------
# Convert LCU at constant prices to USD using 2019 market exchange rates
market_exch_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------
# Convert LCU at constant prices to USD using 2019 PPP conversion rates
ppp_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------
# cumulative average function
cumulative_avg_func <- function(x){
  cumsum(x)/seq_along(x) 
}
#-----------------------------------------------------------------------------------
base_2019_convert_plus_deflate_function <- function(data,
                                                    x,
                                                    market_exch_rate_2019,
                                                    ppp_conversion_rate_2019,
                                                    deflator_2019,
                                                    x2){
  data %>%
    mutate(x_2019_price_2019_USD = (x/market_exch_rate_2019)/deflator_2019,
           x_2019_price_2019_ppp = (x/ppp_conversion_rate_2019)/deflator_2019) %>%
    rename_at(c('x_2019_price_2019_USD', 'x_2019_price_2019_ppp'), funs(str_replace_all(., 'x_', x2)))
}
# Example -----------------
base_2019_convert_plus_deflate_function(mega_combined_vars_df_SUBSET_added_vars,
                              mega_combined_vars_df_SUBSET_added_vars$pwt_gdp_current_LCU,
                              mega_combined_vars_df_SUBSET_added_vars$WID_market_exchange_rate_LCU_per_USD_2019,
                              mega_combined_vars_df_SUBSET_added_vars$WID_ppp_LCU_per_USD_2019,
                              mega_combined_vars_df_SUBSET_added_vars$WID_national_income_price_index,
                              'gdp_') %>%
  view()
#-----------------------------------------------------------------------------------
base_2017_convert_plus_deflate_function <- function(data,
                                                    x,
                                                    market_exch_rate_2017,
                                                    ppp_conversion_rate_2017,
                                                    deflator_2017,
                                                    x2){
  data %>%
    mutate(x_2017_price_2017_USD = (x/market_exch_rate_2017)/deflator_2017,
           x_2017_price_2017_ppp = (x/ppp_conversion_rate_2017)/deflator_2017) %>%
    rename_at(c('x_2017_price_2017_USD', 'x_2017_price_2017_ppp'), funs(str_replace_all(., 'x_', x2)))
}
#-----------------------------------------------------------------------------------
hc1_function <- function(x){
  cov <- vcovHC(x, type = "HC1")
  sqrt(diag(cov))
}
#-----------------------------------------------------------------------------------
pcse_function <- function(x){
  cov <- vcovPC(x, cluster = ~iso3)
  sqrt(diag(cov))
}
#-----------------------------------------------------------------------------------



































