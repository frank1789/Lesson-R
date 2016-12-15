#battery life

rm(list = ls())

expand.grid(A = c(-1, 1), B = c(-1, 0, +1)) #list of test condidition in matrix
                                            #all possible combination in standard order

#function to create a table 
prepare <- function(factors = list(A = 1:3, B = 1:3), k = 4, runorder = TRUE, file = "name_file"){
  #df = data.frame()
  factors$Rep = 1:k
  df.g = expand.grid(factors)
  n    = nrow(df.g)
  key  = data.frame(r = runif(n, 0, 1), s = 1:n)
  key  = key[order(key$r),]$s
  df.h = data.frame(
    RunOrder = key,
    StdOrder = 1:n
  )
  df.t = data.frame(Yield = rep(NA, n))
  df =cbind(df.h, df.g, df.t)
  if(runorder){
    df = df[order(df$RunOrder),]
  }
  if(file != ""){
    write.table(df, file, col.names = T, row.names = F, quote = F, sep = "\t")
  }
  return(df)
}
#prepare table
prepare(list(A = c(-1, +1), B = c(-1, +1), C = c(-1, +1)), k=2, runorder = F, "Lesson7data.txt")
#do experiment
df <- read.table("Lesson7data.txt", h = T)
df <- df[order(df$StdOrder),]


df <- prepare(list(Temp = c(15, 70, 125), Mat = c("1", "2", "3")), k=4, runorder = F, "Lesson7data2.txt")
df$Yield = c(
  130, 34, 20, 150,
  136, 25, 138, 174,
  96, 155, 40, 70,
  188, 122, 70, 110,
  120, 104, 74, 80,
  82, 159, 106, 58,
  168, 150, 82, 100,
  75, 58, 126, 115,
  45, 160, 139, 60
)

#check residuals
df.lm <- lm(Yield~Temp*Mat, data = df)
qqnorm(df.lm$residuals)
qqline(df.lm$residuals)

plot(df$Run, df.lm$residuals, ylab = "Residuals", xlab = "Run Order")
plot(df.lm$fitted.values, df.lm$residuals, ylab = "residuals", xlab = "fitted values")
plot(df$Temp, df.lm$residuals)
plot(as.numeric(df$Mat), df.lm$residuals)

anova(df.lm)

df.lm1 <- lm(Yield~Mat + Temp, data = df)

qqnorm(df.lm1$residuals)
qqline(df.lm1$residuals)

plot(df$Run, df.lm1$residuals, ylab = "Residuals", xlab = "Run Order")
plot(df.lm$fitted.values, df.lm1$residuals, ylab = "residuals", xlab = "fitted values")
plot(df$Temp, df.lm1$residuals)
plot(as.numeric(df$Mat), df.lm1$residuals)

anova(df.lm1)
