#Mineral oil drilling plant
#A:Drill load
#B:Flow Rate
#C: Rotatotion speed
#D: type drill
#Y: Drill advance rate

#2^4 unreplicated FP

rm(list = ls())

lvl <- c("-", "+")

#generating design matrix
(df <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl))

df$Y <- c(
  1.68, 1.98, 4.98, 5.70,
  3.24, 3.44, 9.97, 9.07,
  2.07, 2.44, 7.77, 9.43,
  4.09, 4.53, 11.75, 16.30
)

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

#conservative model:
df.lm <- lm(Y~B*C*D+A, data = df)
anova(df.lm)

df.lm <- lm(Y~B*C+B*D, data = df)
anova(df.lm)

plot(df.lm$effects, df.lm$residuals)
#USE BOX_COX transorm
library(MASS)
boxcox(Y~B*C+B*D, data = df)