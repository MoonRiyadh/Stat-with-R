### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 16. Write the code below the questions. 
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

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 
?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.
data <- amis

str(data)
summary(data)
hist(amis$speed)

ggplot(amis, aes(x = as.factor(period), y = speed)) + 
  geom_boxplot()

lm(speed ~ period, data = amis)

ggplot(data, aes(x = period, y = speed))+ 
  geom_point()+
  geom_abline(intercept = 36.5464, slope=0.6375)


# c) All our columns have numeric type. Convert the categorial columns to factors.
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)
#str(data)

# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)
ggplot(amis, aes(x = period, y = speed)) +
  geom_boxplot(aes(group = period)) +  
  facet_grid(. ~ warning, labeller=label_both)

# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?

#We can conclude that, there are many outliers in both of the Warnings. Comparing warning 1 and 2, we find 
#that warning 2 has some outlier that exceeds the speed sign limit (>60)
#Warning 1:- 
          #before: drivers are intended to increase the speed. some outliers > 51 speed.
          #immediately after: drivers slightly reduce the speed
          #after some time: the speed is increased after some time the waring placed.

#Warning 2:-
          #before: same as in warning 1, speed is increased by driver
          #immediately after: not much increment of the speed in this period. But some drivers exceeding the
          #speed (outliers)
          #after some time: drivers are significantly speed up the vehicles.
 

# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

#I think this is done to observe the human behaviour what they have done in under rules (placed warnings)
#and without any rules (no warning sign). This study also helps us to understand the variation of speed.
#from the warning 2 plot, we see that immediately after the signal placed, some driver speeded up and 
#exceeding their limit


#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.
amis_data<-subset(amis, warning==1)
casted_data <- cast(amis_data, period + pair ~., fun.aggregate = 'mean', value = 'speed', na.rm = TRUE)
colnames(casted_data)[colnames(casted_data)=="(all)"] <- "AvgSpeed"
summary(casted_data)

# b) Build boxplots of the average speed depending on "period".

ggplot(data = casted_data, aes(x = factor(period), y = AvgSpeed)) + 
  geom_boxplot()


# c) Looking at the boxplots, is there a difference between the periods?
#There is outlier in period 3. And there is difference in the Average speed. Period 3 thas the heighst 
#average speed and period period 2 has the minimum avg. speed


# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.
?aov
anova_assump<-aov(AvgSpeed ~ period, data = casted_data)
anova_assump

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

#T-test can check this independence assumption. Because, one-way ANOVA can be treated as an independent 
#T-test and independent t test distributes within each group for dependent variable.
t.test(casted_data$AvgSpeed, casted_data$period)
#data:  casted_data$AvgSpeed and casted_data$period
#t = 61.415, df = 45.404, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0

#We can reject the null hypothesis because of the p value 
#is less then 0.05. And true difference in means is not equal to 0. So that means are independent.

# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

#You can do a Shapiro-Wilk test or look at a Q-Q plot. 
#Since there is a sample size of over 5000 and test.shapiro(x) only works for 
#sample size n | 3 <= n <= 5000, I will look at a normal Q-Q plot.

qqnorm(as.numeric(casted_data$AvgSpeed))
#Judging by the Q-Q plot, the residuals seem to be normally distributed.


# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
#This can be tested with Levene's test.
#install.packages("car")
library(car)
leveneTest(casted_data$AvgSpeed, as.factor(casted_data$period), center = mean)
#Df F value Pr(>F)
#group  2  0.3697 0.6933
#39             
# The bigger Pr value indicate that the result is not significant.Thus we can conclude that the assumption 
#of homogeneity of variance of residuals 

# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.
?aov
onewayanova <- aov(casted_data$AvgSpeed ~ casted_data$period)
summary(onewayanova)
#P value: 0.405 (pr value corresponds to the p value)
#p(0.405) > 0.05, So, we can not reject the null hypothesis. Thus we can say that there is no 
#significant differences between the groups. 


# h) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().
?pairwise.t.test()
pairwise.t.test(casted_data$AvgSpeed, casted_data$period)

# i) Report the pairwise p-values and interpret the result in detail.
#data:  casted_data$AvgSpeed and casted_data$period 

#1    2   
#2 0.81 -   
#  3 0.81 0.51
#P value adjustment method: holm 
#All the valuses > 0.05, 
#Thus we can say that, there is no significant differences between the groups

# j) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change?
pairwise.t.test(x = casted_data$AvgSpeed, g = casted_data$period, p.adjust.method = "none")
#data:  casted_data$AvgSpeed and casted_data$period 

#1    2   
#2 0.59 -   
#3 0.40 0.17

pairwise.t.test(x = casted_data$AvgSpeed, g = casted_data$period, p.adjust.method = "bonferroni")
#1    2   
#2 1.00 -   
#3 1.00 0.51

#P value adjustment method: bonferroni 
#Yes, the result changes. The result increase when "bonferroni" is used.


#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.
casted_data2<-cast(amis, pair + warning +  period ~., fun.aggregate = mean, value = "speed", na.rm = TRUE)
colnames(casted_data2)[colnames(casted_data2)=="(all)"] <- "AverageSpeed"
summary(casted_data2)

# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.
pair_mean <- aggregate(AverageSpeed ~ period + warning, casted_data2, mean)
pair_mean

# c) Do you think there is a significant difference between some of the groups?
#Second element of each pair is greater then first element. i.e. P1: W1 = AvgSpeed (36.51000) < P1:W2 = AvgSpeed (38.21857)
#There is no significant difference between some of the groups

# d) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail.
two_way_anova <- aov(AverageSpeed ~ period * warning, casted_data2)
summary(two_way_anova)
two_way_anova
#P value of period: 0.18345, is greater than 0.05. 
#P value of warning: 0.00463, is less then 0.05. We can conclude that, there is a significance relationship with them. 
#and p value between period and warning: 0.82348, This indecates that these two entities are statistically effected by warnings


# e) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
#The p value for interaction is 0.8234 which is greater than 0.05 , 
#So we can say that there is no significant interaction of period and warning with speed of vehicles.

