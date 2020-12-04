library(tidyverse)
library(ggplot2)
library(car)
library(psych)
library(leaps)

# load data
data <- read.csv('./dallas_cowboys_season_data.csv')
mydata <- data[c("MoV", "PtsScoredRank", "PtsAllowedRank", "YdsGainedRank", "YdsAllowedRank")]

# center the predictors
mydata <- mydata %>%
          mutate(PtsScoredRank.c = PtsScoredRank - mean(PtsScoredRank),
                 PtsAllowedRank.c = PtsAllowedRank - mean(PtsAllowedRank),
                 YdsGainedRank.c = YdsGainedRank - mean(YdsGainedRank),
                 YdsAllowedRank.c = YdsAllowedRank - mean(YdsAllowedRank))

# fit the regression model with centered predictors
reg <- lm(MoV ~ PtsScoredRank.c + PtsAllowedRank.c + YdsAllowedRank.c + PtsAllowedRank.c * YdsAllowedRank.c, mydata)

summary(reg)


# residuals vs. fitted values
mydata$resids <- residuals(reg)
mydata$predicted <- predict(reg)
ggplot(mydata, aes(x = predicted, y = resids)) +
       geom_point() +
       geom_hline(yintercept = 0, color = "blue") +
       labs(title ="Residuals vs. Fitted Values",
            x = "Fitted Values",
            y = "Residuals")

# normal probability plot
ggplot(mydata, aes(sample = resids)) +
       stat_qq() +
       stat_qq_line() +
       labs(title = "Normal Probability Plot",
            x = "Theoretical Percentiles",
            y = "Sample Percentiles")

# check for multicollinearity
vif(reg)


# create a new observation to predict MoV for ranking first in all 4 categories
rankedFirst <- data.frame(PtsScoredRank.c = 1 - mean(mydata$PtsScoredRank),
                         PtsAllowedRank.c = 1 - mean(mydata$PtsAllowedRank),
                         YdsGainedRank.c = 1 - mean(mydata$YdsGainedRank),
                         YdsAllowedRank.c = 1 - mean(mydata$YdsAllowedRank))

# calculate the corresponding prediction
predict(reg, rankedFirst, interval = "prediction")


# create a new observation to predict mean MoV for ranking last in all 4 categories
rankedLast <- data.frame(PtsScoredRank.c = 32 - mean(mydata$PtsScoredRank),
                      PtsAllowedRank.c = 32 - mean(mydata$PtsAllowedRank),
                      YdsGainedRank.c = 32 - mean(mydata$YdsGainedRank),
                      YdsAllowedRank.c = 32 - mean(mydata$YdsAllowedRank))

# calculate the corresponding prediction
predict(reg, rankedLast, interval = "confidence")

# plot relationship between points scored and yards gained
ggplot(mydata, aes(x = PtsScoredRank, y = YdsGainedRank)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# plot relationship between points allowed and yards allowed
ggplot(mydata, aes(x = PtsAllowedRank, y = YdsAllowedRank)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)