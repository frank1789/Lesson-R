#CHEMICAL REACTION PLANT EXAMPLE  

rm(list = ls())

#Factors:
#A: Temperature
#B: Pressure
#C: Concentration of reactant
#D: Stirring rate
## Yield: filtration rate

#2^4 unreplicated FP

lvl <- c("-", "+")

#generating design matrix
(df <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl))

df$Y <- c(
  45, 71, 48, 65,
  68, 60, 80, 65,
  43, 100, 45, 104,
  75, 86, 70, 96
)

df
sum(df$Y)

#full linear model
df.lm <- lm(Y~A*B*C*D, data = df)

anova(df.lm)
df.lm$residuals

#Daniel's method
n <- length(df.lm$effects)
effects <- as.vector(df.lm$effects)[2:n]
qn <- qqnorm(effects, datax = T)
text(qn$x, qn$y, labels = names(df.lm$effects)[2:n], pos = 1)
qqline(df.lm$effects, datax = T)

#From QQ plot it look like only significant effects
#are A, B, C, D, A:C and A:D
df.lm1 <- lm(Y~A*C+A*D+B+A:B:D, data = df)
anova(df.lm1)

df.lm2 <- lm(Y~A*C+A*D, data = df)
qqnorm(df.lm2$residuals)
qqline(df.lm2$residuals)
plot(df.lm2$residuals)
plot(df.lm2$residuals, df.lm2$fitted.values)

