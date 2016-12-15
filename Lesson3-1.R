rm(list = ls())

cuts <- qnorm(seq(0,1,0.2), m = 0, s = 1)
cuts

#elegant way number of cut = 8
rm(list = ls())

m <- 0
s <- 1
nc <-8

(cuts <- qnorm(seq(0,1, len = nc +1), m = m, s = s))
curve(pnorm(x, m = m, s = s), m - 3 * s, m + 3 * s)
curve(dnorm(x, m = m, s = s), m - 3 * s, m + 3 * s)
#abline(v = m)
abline(v = cuts, col = "green")

#other example
rm(list = ls())

N <- 100
m <- 5.04
s <- 0.08
nc <-8

(cuts <- qnorm(seq(0,1, len = nc +1), m = m, s = s))
curve(pnorm(x, m = m, s = s), m - 3 * s, m + 3 * s)
curve(dnorm(x, m = m, s = s), m - 3 * s, m + 3 * s)
#abline(v = m)
abline(v = cuts, col = "green")

Oi <- c(12, 14, 12 , 13, 12, 11, 12, 14)
Ei <- rep((N / nc), nc)

(chisq <- sum((Oi - Ei)**2 / Ei))
pchisq(chisq, df = nc - 2 - 1, lower.tail = F)
