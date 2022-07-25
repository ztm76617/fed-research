# Fitting the Regression Line and its Residuals

library(tidyverse)

d <- mtcars

fit <- lm(mpg ~ wt, data = d) # fit the model

d$predicted <- predict(fit)   # Save the predicted values

d$residuals <- residuals(fit) # Save the residual values

ggplot(d, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

summary(fit) # Summary of the fit 

# Residuals vs Fitted Plot
plot(fit,
     which = 1,
     col = c("blue"))

# Q-Q Plot
plot(fit,
     which = 2,
     col = c("red"))  

# Scale-Location Plot
plot(fit,
     which = 3,
     col = c("blue"))

# Residuals vs Leverage
plot(fit,
     which = 5,
     col = c("blue"))
