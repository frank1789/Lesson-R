rm(list = ls())
#load data from github page, using url
df <- read.table("https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/diet.dat", header = T)

#build the LINEAR MODEL
df.lm <- lm(cTime~diet, data = df)
#contents of an object
attributes(df.lm)
df.lm$residuals # print information by standard order

#NORMALITY CHECK
#quantile-quantile
qqnorm(df.lm$residuals)
qqline(df.lm$residuals) #teh value in plot are closed because the time using only integers
                        #resolution is not high enough
shapiro.test(df.lm$residuals)

#TRENDS/PATTERNS
plot(df.lm$residuals, xlab = "Index", ylab = "Residuals")
plot(df.lm$fitted, df.lm$residuals, xlab = "Fitted Values", ylab = "Residuals")

#ANOVA ANALYSIS
anova(df.lm)
#CHART
boxplot(cTime~diet, data = df)

#alternative interface:
df.aov <- aov(cTime~diet, data = df)
summary(df.aov)

#TUKEY'S TEST
TukeyHSD(df.aov)
#plot the Tukey's Test
plot(TukeyHSD(df.aov))