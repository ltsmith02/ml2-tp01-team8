library(ISLR2) 
library(e1071) # that how u use the svm functions
library(caret)
data <- Auto 
data$y <- as.factor(ifelse(data$mpg > median(data$mpg), 1, 0)) #making new column in the dataset called, that will assign you either to 1 or 0 depending on how you do compare to median in miles per hour
data <- data[,-1] # we are getting rid of 1st column called mpg so that the model cant just cheat the answer

set.seed(1693)
svm1 <- tune(svm , y~., data=data, kernel = "linear", ranges=list(cost=c(0.1,1,10,100,1000)))
             #we are tuning svm model, by passing it through 5 different costs, to see which one performs better
##
summary(svm1)
#summary tells us which parameters provided best performance. in our case best cost was 0.1 with error rate 0.09 (%)
svm1_best <- svm1$best.model #this line just saves our new best model. so that we work with it later on

set.seed(1693)
svm2 <- tune(svm , y~., data=data, ranges=list(cost=c(0.1,1,10,100,1000), kernel = "radial",  gamma = c(0.5,1,2,3,4))) 
             ##we now add gamma as tuning parameter to see which one performs better
summary(svm2)
# the best combination was cost 1 gamma 1 with an error rate 0.076
svm2_best <- svm2$best.model

set.seed(1693)
svm3 <- tune(svm , y~., data=data, ranges=list(cost=c(0.1,1,10,100,1000), kernel = "polynomial", gamma = c(0.5,1,2,3,4), 
                                              degrees = c(2, 3, 4, 5))) # we will pass through degrees as new tunning parameter because we are using polonomyial kernel
summary(svm3)
#1 cost 0.5 gamma 2 degrees with error rate 0.08
svm3_best <- svm3$best.model


plot(svm1_best, data, displacement~acceleration)

plot(svm2_best, data, horsepower~acceleration)

plot(svm3_best, data, horsepower~acceleration)
 ##different colours of x and o represent diffrerent classes that we are trying to predict,the x's are the datapoints that were used in decision boundary
## 0's werent relavent to the model. background colour is supposed to represent the svm boundary tho it doesnt always line up with actual plot


