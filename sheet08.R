### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 6. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

## Please write below your (and your teammates) name, matriculation number. 
## Name: 1. H T M A Riyadh, 2. Maria Francis
## Matriculation number: 1. 2577735, 2. 2573627

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########
library(ggplot2)

# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.
data <- read.table("kidiq.txt")
summary(data) 


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.

ggplot(data, aes(x = mom_iq, y = kid_score)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  ggtitle("kid_score vs mom_iq") + 
  xlab("mom_iq") + 
  ylab("kid_score")




# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.
simple_rmodel1 <- lm(data = data, kid_score ~ mom_hs)
simple_rmodel1

#Estimated regression line equation can be written as follow: kid_score = 77.55 + 11.77*mom_hs
#The intercept is 77.55. It can be interpret that if mom_hs has no high school degree, 
#then the kid_score would be 77.55. And slope is 11.77. It can be interpret that if mom_hs 
#increases one unit then the kid_score will be increased by 11.77 times.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.
simple_rmodel2<-lm(data = data, kid_score ~ mom_hs + mom_iq)
simple_rmodel2

#For interpretation:

#If mom_hs has no high school degree and mom_iq = 0, then the kid_score would be 25.7315. On the other hand, 
#if mom_hs increases one unit then the kid_score will be increased by 5.9501 and if mom_iq increases one unit 
#then the kid_score will be increased by 0.5939

#summary(simple_rmodel2)
#summary(simple_rmodel2)$coefficient
#It can be seen that one of the p-value of the F-statistic is < 2.2e-16 
#it means that, at least, one of the predictor variables is significantly related to the outcome variable.
#Comparing both models: It can besaid that, mom_hs has more significantly related to the kids_score raher then 
#combining mom_iq and mom_hs.


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))

pred = data.frame(mom_iq = data$mom_iq, mom_hs = data$mom_hs, kid_score = data$kid_score, kid_score_pred = fitted(simple_rmodel2))
summary(pred)

ggplot(data = pred, aes(x = kid_score, y = mom_iq, col = factor(mom_hs))) +
  geom_point() +
  geom_line(aes(x = kid_score_pred, y = mom_iq))


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.

simple_rmodel3<-lm(kid_score ~ mom_hs * mom_iq, data)
simple_rmodel3
#interpretation
#Intercept has minus value, it means when mom_hs and mom_iq has no influence, kid_score will be decreased by 11.4820

# g) Next, let's plot the results of this model.
ggplot(data,aes(y=kid_score, x = mom_iq, color=factor(mom_hs)))+
  geom_point()+ 
  geom_smooth(method="lm",se=TRUE)


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

new_data <- data.frame(mom_iq=100, mom_hs=1)
pred_int<-predict(simple_rmodel3, new_data , interval = "confidence", level=0.95)
pred_int


# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?
ggplot(data, aes(x = mom_iq, y = kid_score, title)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  ggtitle("kid_score vs mom_iq") + 
  xlab("mom_iq") + 
  ylab("kid_score")


#the gray shade depicts confidence interval of 0.95. it tells the probability of the model of kid_score with 
#mom_iq.if we replicate the same dataset multiple times with different random samples then 95% of the 
#confidence intervals of mean kid_score would contain the true slope of the regression line.


# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

par(mfcol=c(2,3))
plot(lm(kid_score ~ mom_iq * mom_hs, data = data), which=seq(1,6))

#Residuals vs Fitted: 
#we can observe there are almost equally spreaded residuals around the horizontal line to x axis. So we can say that
#there is no pattern. Thus we can conclude that our model has non liniarity relationship.

#Scale-Location: 
#From this plot, we can interpret that the residuals are spreaded equally along the ranges predictiors, the data 
#points are spreaded equally on the both sides of horizontal line. In this case the variance of residual points 
#are decreasing wrt the fitted outcome variable.

#Residuals vs Leverage: 
#By observing this plot, we can easily identify if there any influential outliers. 
#the plot describes three extreme points 286, 213 , 111.

#Normal Q_Q: 
#This plot indicates whether the residuals are normally distributed or not. From the plot, we observe that
#it is noramlly distributed.

#Cook's distance: 
#This plot indcates whether there is any influential data points.
# In our data points, values 111 , 213, 286 are highly influential in the data.

#Cook's Dist vs Leverage:
# This plot also is to find whether there are any highly leveraged observation which also have high influence in the
# model.In our case, three points which appear to be influential and all other points lie towards left together.
