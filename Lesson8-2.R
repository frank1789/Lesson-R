#agmuntations with central point 
#2^2 FP, A = reaction time

Lvl <- c("+1", "-1")
Df <- expand.grid(A = lvl, b = lvl)
Df[5:9, ] = 0
Df$Y <- C(
  38.3, 40.9, 40, 41.5,
  40.3, 40.5, 40.7,40.2, 40.6
)

Anova(lm(Y~A*B+poly(B,2), data = df)
Anova(lm(Y~A*B+poly(A,2), data = df)
            
Anova(lm(Y~A+B, data = df))