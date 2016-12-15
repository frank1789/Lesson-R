#agmuntations with central point
#2^2 FP, A = reaction time

lvl <- c("+1", "-1")
df <- expand.grid(A = lvl, b = lvl)
df[5:9,] = 0
df$Y <- c(38.3, 40.9, 40, 41.5,
          40.3, 40.5, 40.7, 40.2, 40.6)
anova(lm(Y ~ A * B + poly(B, 2), data = df)
anova(lm(Y ~ A * B + poly(A, 2), data = df)
anova(lm(Y ~ A + B, data = df))