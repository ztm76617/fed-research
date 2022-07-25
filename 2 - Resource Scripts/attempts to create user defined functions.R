#-----------------------------------------------------------------------------------
deflate_plus_ppp_convert_func <- function(x){
    (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019
}
#-----------------------------------------------------------------------------------
deflate_plus_market_exch_convert_func <- function(x){
  (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019
 }
#-----------------------------------------------------------------------------------

market_exch_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------

ppp_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------
# example function
coef.of.var <- function(x){
  meanval <- mean(x,na.rm=TRUE)
  sdval   <- sd(x,na.rm=TRUE)
  return(sdval/meanval)
  }

custom_currency_func <- function(df, x, y, z1, z2){
  df <- df %>%
    mutate(constant_2019_ppp_2019 = (x/y)/z1,
           constant_2019_USD_2019 = (x/y)/z2)
  return(df)
}


x <- mega_combined_vars_df_edited %>%
  select(contains('current_LCU'),
         contains('current_price_LCU'))

y <- mega_combined_vars_df_edited %>%
  select(WID_national_income_price_index)

z1 <- mega_combined_vars_df_edited %>%
  select(WID_ppp_LCU_per_USD_2019)

z2 <- mega_combined_vars_df_edited %>%
  select(WID_market_exchange_rate_LCU_per_USD_2019)



custom_currency_func(mega_combined_vars_df_edited,
                     x, y, z1, z2)








