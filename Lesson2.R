# t test example
rm(list = ls()) #remove varialble in enviroment

n1 <- 10
n2 <- 12
n <- n1 + n2 - 2

s1 <- rnorm(n1, 5, 0.1)
s2 <- rnorm(n2, 5.3, 0.1)
sp <- ((n1 - 1)*var(s1) + (n2 - 1) * var(s2)) / n
t0 <- (mean(s1) - mean(s2))/ (sqrt(sp * (1 / n1 + 1 / n2)))

pt(abs(t0), n, low = F) * 2

#first case
(test <- t.test(s1, s2, alt = "two.sided", var.equal = F, conf.level = 0.99))

#second case
(test <- t.test(s1, s2, alt = "two.sided", var.equal = T, conf.level = 0.99))
