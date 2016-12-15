#https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/rats.dat

rm(list = ls())
#load data from github page, using url
df <- read.table("https://raw.githubusercontent.com/pbosetti/DCPP-rstudio/master/rats.dat", header = T)
df
boxplot(Life~Poison, data = df, xlab = "Poison", ylab = "Life")
boxplot(Life~Treat, data = df, xlab = "Poison", ylab = "Life")

#better to view detail among factor
interaction.plot(df$Poison, df$Treat, df$Life)
interaction.plot(df$Treat, df$Poison, df$Life)

#build the LINEAR MODEL
df.lm <- lm(Life~Treat*Poison, data = df)
#contents of an object
attributes(df.lm)
df.lm$residuals # print information by standard order

#ANOVA ANALYSIS
anova(df.lm)

#Model  adequacy check:
#NORMALITY CHECK
#quantile-quantile
qqnorm(df.lm$residuals)
qqline(df.lm$residuals) 
shapiro.test(df.lm$residuals)
plot(df.lm$fitted.values, df.lm$residuals) #the trend depends by the fitted model

#log convertion
df.lm1 <- lm(log(Life)~Treat*Poison, data = df)
qqnorm(df.lm1$residuals)
qqline(df.lm1$residuals) 
shapiro.test(df.lm1$residuals)
plot(df.lm1$fitted.values, df.lm1$residuals) #the trend depends by the fitted model

#inverse transform
df.lm2 <- lm(1/(Life)~Treat*Poison, data = df)
#Model  adequacy check:
#NORMALITY CHECK
#quantile-quantile
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals) 
shapiro.test(df.lm2$residuals)
plot(df.lm2$fitted.values, df.lm2$residuals) #the trend depends by the fitted model

#ANOVA ANALYSIS
anova(df.lm2)

#inverse transform
df.lm3 <- lm(1/(Life)~Treat + Poison, data = df) #without interaction
#ANOVA ANALYSIS
anova(df.lm3)

#Model  adequacy check:
#NORMALITY CHECK
#quantile-quantile
qqnorm(df.lm3$residuals)
qqline(df.lm3$residuals) 
shapiro.test(df.lm3$residuals)
plot(df.lm3$fitted.values, df.lm3$residuals) #the trend depends by the fitted model

