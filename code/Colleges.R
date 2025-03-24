
# #####################################################
# Analysis of US News and World Report College Rankings
# Data for the 1995 rankings are provided by the ISLR2
# library.
# https://www.kaggle.com/code/aonursert/k-means-clustering-project
# #####################################################

library(ISLR2)
data(College)
attach(College)

head(College)

# Compute simple summary statistics for all variables in the data frame
summary(College)

apply(College[,-1],2,mean)
apply(College[,-1],2,sd)

x1 = rep(1,10)
x1
sd(x1)

sd(rep(1,10))
x2 <- seq(1,10)
x2
sd(x2)

# attach() attaches a data frame to the search path of the environment
# After attaching, you can refer directly to the variables in the data
# frame. For example, to compute the average graduation rate without
# attaching the command would be mean(College$Grad.Rate), after attaching
# the command is simpler: mean(Grad.Rate)

mean(Grad.Rate)

# Exploratory Data Analysis (EDA)

# Which college has the highest cost for room and board?
College[which.max(Room.Board),]

# Which college has the highest graduation rate?
College[which.max(Grad.Rate),]

# Which colleges have graduation rates above 95%?
College[which(Grad.Rate >= 100),]

# Are there public universities with a graduation rate above 95%?
College[which((Grad.Rate > 95) & (Private=="No")),]

#Create a scatterplot of graduation rate versus costs for room and board with 
#data points colored by public/private state of the college.
plot(x=Room.Board, y=Grad.Rate , col=Private, type="p")
legend("topleft", legend=c("Public", "Private"), col=c(1,2), pch=1)

#Create a scatterplot of full time undergraduate enrollment versus out-of-state 
#tuition. Color the data points according to the public/private state of the college.
plot(x=Outstate, y=F.Undergrad, col=Private, type="p")
legend("topright", legend=c("Public", "Private"), col=c(1,2), pch=1)

# Can you find the private school that has over 25,000 full time undergraduates
# and out-of-state tuition of less than $2,500?
College[which(F.Undergrad > 25000 & Private=="Yes"),]


# Create histograms of the out-of-state tuition and compare private and public 
# colleges.

hist(Outstate[Private=="Yes"], col="powderblue")
hist(Outstate[Private=="No"], col="pink", add=TRUE)

par(mfrow=c(1,2))
hist(Outstate[Private=="Yes"], col="powderblue", main="Private")
hist(Outstate[Private=="No"] , col="pink"      , main="Public" )

par(mfrow=c(1,2))
hist(Outstate[Private=="Yes"], 
     col ="powderblue", 
     main="Private",
     xlab="Out-of-state tuition",
     xlim=c(min(Outstate),max(Outstate)))
hist(Outstate[Private=="No"] , 
     col ="pink", 
     main="Public",
     xlab="Out-of-state tuition",
     xlim=c(min(Outstate),max(Outstate)))

# Switch to probabilities rather than absolute frequency because there are more
# than twice as many private colleges

par(mfrow=c(1,2))
hist(Outstate[Private=="Yes"], 
     col ="powderblue",
     freq=FALSE,
     main="Private",
     xlab="Out-of-state tuition",
     xlim=c(min(Outstate),max(Outstate)))
hist(Outstate[Private=="No"] , 
     col ="pink",
     freq=FALSE,
     main="Public",
     xlab="Out-of-state tuition",
     xlim=c(min(Outstate),max(Outstate)))



# Repeat the histogram construction for the graduation rate.
par(mfrow=c(1,2))
hist(Grad.Rate[Private=="Yes"], 
     col ="powderblue", 
     main="Private",
     xlab="Graduation rate",
     xlim=c(min(Grad.Rate),max(Grad.Rate)))
hist(Grad.Rate[Private=="No"] , 
     col ="pink", 
     main="Public",
     xlab="Graduation rate",
     xlim=c(min(Grad.Rate),max(Grad.Rate)))

# What should we do with the outlier?
# Go ahead and google the college.
College[which((Grad.Rate > 100) & (Private=="Yes")),]

# We could overwrite the graduation rate with the largest possible value
College[which((Grad.Rate > 100) & (Private=="Yes")),]$Grad.Rate <- 100

# We could set the value to a "missing value", marking it as unobserved
College[which((Grad.Rate > 100) & (Private=="Yes")),]$Grad.Rate <- NA


# #####################################################
# Box plots to visualize the distribution of variables

# All variables
boxplot(College)

# Single variables
boxplot(College$Expend)

boxplot(Outstate,ylab="Outstate")
# Formula expression to get separate boxplots
boxplot(Grad.Rate ~ Private)
boxplot(Expend ~ Private)
