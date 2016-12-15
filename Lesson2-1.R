# godness fit example
rm(list = ls()) #remove varialble in enviroment

count <- 200
data  <- rnorm(count, 15, 2)
(data.m <- mean(data))
(data.s <- sd(data))

(data.h <- hist(data, freq = F)) #very useful function to generate histogram object

data.b <- data.h$breaks
data.b[1] <- -Inf             #first element substitute with -infinity
data.b[length(data.b)] <- Inf #last element substitute with +infinity
ei <- diff(pnorm(data.b, data.m, data.s)) * count

plot(ei)
points(data.h$counts, col = "red")

(chi <- sum((data.h$counts - ei)**2 / ei))
pchisq(chi, df = 7)

#Kolmogrov-Smirnov Test
ks.test(data, "pnorm", mean = data.m, sd = data.s)
#Shapiro-Wilk test
shapiro.test(data)
