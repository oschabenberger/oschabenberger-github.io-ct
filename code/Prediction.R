
# #####################################################
# R code to go along with the chapter "Making Predictions"
# #####################################################


# #####################################################
# The simulated data set
# #####################################################

set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 * cos(x)
X <- rnorm(n, sd = 2)
Y <- m(X) + eps
xGrid <- seq(-5, 6, l = 250)

plot(X, Y, las=1, bty="l")
rug(X, side = 1); 
rug(Y, side = 2)


# #####################################################
# Three Models trained on the initial data set
# #####################################################

l_01 <- loess(Y ~ X,degree=1, span=0.075)
l_03 <- loess(Y ~ X,degree=1, span=0.3)
l_10 <- loess(Y ~ X,degree=1, span=1)

plot(X,Y,las=1, bty="l")
lines(xGrid,
      predict(l_01, newdata=data.frame("X"=xGrid)), 
      col="darkgreen", 
      lty="dashed",
      lwd=2)
lines(xGrid,
      predict(l_03, newdata=data.frame("X"=xGrid)), 
      col="red", 
      lty="dotted",
      lwd=2)
lines(xGrid,
      predict(l_10, newdata=data.frame("X"=xGrid)), 
      col="blue", 
      lty="solid",
      lwd=2)

# #####################################################
# Two more data sets
# #####################################################

m <- function(x) x^2 * cos(x)
eps2 <- rnorm(n, sd = 2)
X2 <- rnorm(n, sd = 2)
Y2 <- m(X) + eps2

eps3 <- rnorm(n, sd = 2)
X3 <- rnorm(n, sd = 2)
Y3 <- m(X) + eps3

plot(X, Y, las=1, bty="l", col="black", pch=1., ylim=c(-16,18))
points(X2,Y2, col="red", pch=2)
points(X3,Y3, col="blue", pch=3)


# #####################################################
# Fitting the "blue" model to the three data sets
# #####################################################

l_10 <- loess(Y ~ X,degree=1, span=1)
l_102 <- loess(Y2 ~ X2,degree=1, span=1)
l_103 <- loess(Y3 ~ X3,degree=1, span=1)

plot(x=xGrid,
     y=predict(l_10, newdata=data.frame("X"=xGrid)), 
     xlab="X",
     ylab="Y",
     type="l",
     las=1,
     bty="l",
     col="blue", 
     lty="solid",
     lwd=2,
     ylim=c(-16,18))
lines(xGrid,
      predict(l_103, newdata=data.frame("X3"=xGrid)), 
      col="blue", 
      lty="solid",
      lwd=2)
lines(xGrid,
      predict(l_102, newdata=data.frame("X2"=xGrid)), 
      col="blue", 
      lty="solid",
      lwd=2)


# #####################################################
# Cross-validation
# #####################################################

simData <- data.frame(X=X, Y=Y)

library(caret)          

cv_results <- train(Y ~ X,                   
                    data     =simData,       
                    method   ="gamSpline",   
                    tuneGrid =data.frame(df=seq(2, 25, by=1)),      
                    trControl=trainControl(method="cv", number=10)) 


print(cv_results)

plot(cv_results)

# #####################################################
# Training the final model
# #####################################################

final_model <- train(Y ~ X,                   
                     data     =simData,       
                     method   ="gamSpline",   
                     tuneGrid =data.frame(df=cv_results$bestTune[1]),
                     trControl=trainControl(method="none"))

# #####################################################
# Making predictions
# #####################################################

xGrid <- data.frame(X=seq(-5, 6, length.out = 250))  # <1>
predvals <- predict(final_model,newdata=xGrid)       # <2>

predvals[1:10]

plot(simData$X, simData$Y, 
     type="p", 
     las =1,
     bty ="l",
     xlab="X",
     ylab="Y")
lines(xGrid$X,predvals,col="red",lwd=2)
lines(xGrid$X,m(xGrid$X),col="black",lwd=2,lty="dashed")
legend("topleft",
       legend=c("Cross-validated","True"),
       lty   =c("solid","dashed"),
       col   =c("red","black"),
       lwd   =2)

# #####################################################
# Quantifying Uncertainty
# #####################################################

library(gam) 

final <- gam(Y ~ s(X,df=14), data=simData)

predvals <- predict(final,se.fit=TRUE)

round(predvals$fit[1:10],4)
round(predvals$se.fit[1:10],4)  

pred_df <- data.frame(X=simData$X, 
                      pred=predvals$fit,
                      se=predvals$se.fit)
pred_df_sorted <- pred_df[order(pred_df$X),]
pred_df_sorted$ci_up <- pred_df_sorted$pred + qnorm(0.975)*pred_df_sorted$se
pred_df_sorted$ci_lo <- pred_df_sorted$pred - qnorm(0.975)*pred_df_sorted$se

plot(simData$X, simData$Y, 
     type="p", 
     las =1,
     bty ="l",
     xlab="X",
     ylab="Y")
lines(xGrid$X,m(xGrid$X),col="black",lwd=2,lty="dashed")
lines(pred_df_sorted$X,
      pred_df_sorted$pred,col="red",lwd=2)
lines(pred_df_sorted$X,
      pred_df_sorted$ci_up,col="blue",lty="dotted",lwd=1.5)
lines(pred_df_sorted$X,
      pred_df_sorted$ci_lo,col="blue",lty="dotted",lwd=1.5)
