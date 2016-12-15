n1 <- 10
n2 <- 12
n <- (n1 + n2 -2)
s1 <- rnorm(n1, 5, 0.1)
s2 <- rnorm(n2, 5, 0.1)

mean(s1)
mean(s2)

np <- ((n1 - 1) * sd(s1) + (n2 - 1) * sd(s2)) / (n)

t0 <- (mean(s1) - mean(s2)) / (np * sqrt(1 / (n)))
t0

pt(t0, n, low=F)