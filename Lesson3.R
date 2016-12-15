rm(list = ls())

library(MASS)
head(survey)

#write the file
write.table(survey, file = "survey.dat", row.names = F, sep = "\t")
setwd("/Users/Francesco/Documents/R Project")

#read the file
read.table("survey.dat", header = T, sep = "\t") -> data
head(data)

#contingency table
(tbl <- table(survey$Smoke, survey$Exer))
chisq.test(tbl)

(ctbl <- cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]))
chisq.test(ctbl)