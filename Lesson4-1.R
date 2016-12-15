rm(list = lm())

#Let' say we have a dice and we throw the dice for 2000 times
dice.throws <- 2000
dice.faces  <-    6
dice.result <- c(
  388, #face 1
  322, #face 2
  314, #face 3
  316, #face 4
  344, #face 5
  316  #face 6
)

dice.E <- dice.throws/dice.faces

dice.dof <- length(dice.result) - 0 - 1

#we create our chi squared test
chi <- 0
for(r in dice.result)
  chi <- ((r - dice.E)** 2 / dice.E)

#extract from our chi.squared probability function for 5 dof
dice.alpha <- 0.05
(pchisq(chi, df = dice.dof))
(chi)


(1 - dice.alpha)
(qchisq(1 - dice.alpha, df = dice.dof))