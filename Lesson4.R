rm(list = ls())

u.a <-  0.1
u.b <- -0.1

s.a <- 0.1
s.b <- 0.15

n.1 <- 15
n.2 <- 13
n.3 <- 12
n.4 <- 10

#dof ns4-1

s.1 <- rnorm(n.1, u.a, s.a)
s.2 <- rnorm(n.2, u.a, s.b)
s.3 <- rnorm(n.3, u.b, s.a)
s.4 <- rnorm(n.4, u.b, s.b)

# Test 1
# s.4 aganist  distribution with u.b under  the hypotesis they have " the same" variance
t.0.1 <- (mean(s.4) - u.b) / (sd(s.4) / sqrt(n.4))
(pt(t.0.1, n.4-1, lower.tail = F) * 2)

(t.tt.1 <- t.test(s.4, alternative = "two.sided", mu = u.b, paired =  F))

#function that evaluates t value:
my.t.test <- function(s1, s2, n1, n2){
  n  <- n1 + n2 - 2
  sp <- sqrt(((n1 - 1) * var(s1) + (n2 - 1) * var(s2)) / n)
  t0 <- (mean(s1) - mean(s2)) / (sp * sqrt(1 / n1 + 1 / n2))
  return(t0)
}

t.0.2 <- my.t.test(s.1, s.2, length(s.1), length(s.2))
t.0.2

(t.tt.2 <- t.test(s.1, y = s.2, alt = "two.sided", paired = F, var.equal = T))

#t.tt.3 <- t.test(s.1, y = s.3, alt = "two.sided", paired = F, var.equal = T)
#t.tt.3

t.tt.3 <- t.test(s.1, y = s.3, alt = "less", paired = F, var.equal = T)
t.tt.3

t.tt.4 <- t.test(s.2, y = s.4, alt = "greater", paired = F, var.equal = T)
t.tt.4