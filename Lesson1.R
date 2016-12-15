# this is a comment
#
a = 1 #assignemen, change in function 
a <- 2 #assignemnt type 2, change globally
3 -> a
#vector creator as
c(1,2,3,4)
data.frame(A=c(1,2,3), B=c(3,4,5))
data.frame("A 1"=c(1,2,3), B=c(3,4,5))
data.frame("A 1"=c(1,2,3), B=c(3,4,5)) -> df
df$A
df$B
df$B[1]
df[1,1]
df[1,]
df[,1]
0:9
0:9 -> C
C[1]
C[3:5]
TRUE
FALSE
T
F
#C[C(F,F,T)]
df$A+df$B
df$A*df$B
#function
myfun <- function(x) x^2
myfun(df$A)
rnorm(10)
rnorm(10) -> v
mean(v)
sd(v)
plot(1:10)
x <- 0:9
plot(x, x*10)
x <- -3:3
plot(x, dnorm(x))
plot(x, dnorm(x), type = "b")
x <- seq(-3,3,0.1)
x
plot(x, dnorm(x))
plot(x, dnorm(x), type = "b") #ball
plot(x, dnorm(x), type = "l") #line
lines(x, dt(x, 10), col = "red")
plot(x, pnorm(x), type = "l")
plot(x, pt(x,10), type = "l")
plot(x, pt(x,10, lower.tail = T), type = "l")
plot(x, pt(x,10, lower.tail = F), type = "l")
pt(2,10, lower.tail = F)
plot(seq(0,1,0.1), qt(seq(0,1,0.1), 10, lower.tail = F), type = "l")
plot(seq(0,1,0.1), qt(seq(0,1,0.1), 10, lower.tail = F), type = "b")
#######################################################################







