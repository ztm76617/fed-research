#-------------------------------------------------------------------------------
# Outlier Removal Example 1 -----------------------------------------------------

# Test data and test linear model
DF<-data.frame(X=rnorm(200), Y=rnorm(200), Z=rnorm(200))

LM <- lm(X~Y+Z, data=DF)

# Store the residuals as a new column in DF
DF$Resid<-resid(LM)

# Find out what 2 standard deviations is and save it to SD2
SD2 <- 2*sd(resid(LM))

# Make DF$Outs 1 if a residual is 2 st. deviations from the mean, 0 otherwise
DF$Outs<-ifelse(abs(DF$Resid)>SD2, 1, 0)

# Plot this, note that DF$Outs is used to set the color of the points.
plot(DF$Resid,
     col=DF$Outs+1,
     pch=16,
     ylim=c(-3,3))

#Make a new data.frame with no outliers
DF2<-DF[!DF$Outs,]

nrow(DF2)

# Plot new data
plot(DF2$Resid,
     col=DF2$Outs+1,
     pch=16,
     ylim=c(-3,3))
#-------------------------------------------------------------------------------
# Outlier Removal Example 2 -----------------------------------------------------

# Test data and test linear model
DF<-data.frame(X=rnorm(200), Y=rnorm(200), Z=rnorm(200))

LM <- lm(X~Y+Z, data=DF)

# Store the residuals as a new column in DF
DF$Resid <- resid(LM)

# Find out what 1.5 standard deviations is and save it to SD1.5
SD1.5 <- 1.5*sd(resid(LM))

# Make DF$Outs 1 if a residual is 1.5 st. deviations from the mean, 0 otherwise
DF$OutsSD1.5 <- ifelse(abs(DF$Resid) > SD1.5, 1, 0)

# Plot this, note that DF$Outs is used to set the color of the points.
plot(DF$Resid,
     col = DF$OutsSD1.5+1, # sets color of 2 sd outliers
     pch=16,
     ylim=c(-3, 3))

# Make a new data.frame with no outliers
DF_no_outliers <- DF[!DF$OutsSD1.5,]

nrow(DF_no_outliers)

# Plot new data
plot(DF_no_outliers$Resid,
     col = DF3$OutsSD1.5+1,
     pch=16,
     ylim=c(-3,3))
#-------------------------------------------------------------------------------
# Outlier Removal Example 3 -----------------------------------------------------

# Test data and test linear model
DF<-data.frame(X=rnorm(200), Y=rnorm(200), Z=rnorm(200))

LM <- lm(X~Y+Z, data=DF)

# Store the residuals as a new column in DF
DF$Resid <- resid(LM)

# Find out what 1 standard deviations is and save it to SD1
SD1 <- 1*sd(resid(LM))

# Make DF$Outs 1 if a residual is 1.5 st. deviations from the mean, 0 otherwise
DF$OutsSD1 <- ifelse(abs(DF$Resid) > SD1, 1, 0)

# Plot this, note that DF$Outs is used to set the color of the points.
plot(DF$Resid,
     col = DF$OutsSD1+1, # sets color of 2 sd outliers
     pch=16,
     ylim=c(-3, 3))

# Make a new data.frame with no outliers
DF_no_outliers <- DF[!DF$OutsSD1,]

nrow(DF_no_outliers)

# Plot new data
plot(DF_no_outliers$Resid,
     col = DF_no_outliers$OutsSD1+1,
     pch=16,
     ylim=c(-3, 3))




