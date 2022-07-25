install.packages('naniar')
library(naniar)

# Predicting Values w/ linear models

### Example ----------------------------------------
# Load the data
data("cars", package = "datasets")
# Build the model
model <- lm(dist ~ speed, data = cars)

model

# new inputs
new.speeds <- data.frame(
  speed = c(12, 19, 24)
)

# apply 'predict' function
predict(model, newdata = new.speeds)

# apply 'predict' function w/ 99% CI
predict(model, newdata = new.speeds, interval = "confidence")
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  names()
#--------------------------------------------------------------------------------
rop_predict_model <- lm(pct_growth_real_gdp_LCU ~ marx_rop_total_economy_v1,
                        data = mega_combined_vars_df_SUBSET_added_vars)

marx_rop_total_economy_v1_omits <- as.list(mega_combined_vars_df_SUBSET_added_vars %>%
  mutate(marx_rop_total_economy_v1_omits = sample(marx_rop_total_economy_v1), NA) %>%
  select(marx_rop_total_economy_v1_omits))

# apply 'predict' function w/ 95% CI
predict(rop_predict_model, newdata = new.speeds, interval = "confidence")







  