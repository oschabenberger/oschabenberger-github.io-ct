
# #####################################################
# Correlation analysis for College data
# #####################################################

library(ISLR2)
data(College)
attach(College)

cor(Grad.Rate, Outstate)

cor(College[,-1], Grad.Rate)

round(cor(College[,-1]),3)

#install.packages("corrplot")

library(corrplot)
cormat <- cor(College[,-1])
corrplot(cormat, diag=FALSE, method="color")

