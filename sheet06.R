### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates') name, matriculation number. 
## Name: 1. H T M A Riyadh, 2. Abdallah Bashir, 3. Maria Francis
## Matriculation number: 1. 2577735, 2. 2577831, 3. 2573627

## Change the name of the file by adding your matriculation numbers
## (sheet06_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)
library(ggplot2)
library(cowplot)
#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.
data <- ratings

# b) Take a look at the data frame.
head(data)

# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).
ggplot(data, aes(x = Length, y = Frequency)) +
  geom_point()

# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?

# Looking at the graph, it does not look like they are corelated to each other.

# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
#help("cor")
cor(x = data$Length, y = data$Frequency, use = "complete.obs",  method = "pearson")


# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

#[1] -0.4281462
#The answer is near to -.5. so it suggested a medium effect.
#the direction of the effect is negative

# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Length, data$Frequency,use="complete.obs", method = "kendall")

#[1] -0.316297

#Pearson coefficien is a measure of the linear correlation between two variables
#and Kendall's method is a measure of rank correlation

# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?

# Significance:- correlation coefficient value changed using kendall's method.
cor.test(data$Length, data$Frequency,  method = "kendall")

#data:  data$Length and data$Frequency
#z = -3.9186, p-value = 8.907e-05
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#-0.316297
#Conclusion: p-value is less then 0.5, thus we cannot reject the null hypothesis 


# i) Finally, also calculate Spearman's rank correlation for the same data.
cor(data$Length, data$Frequency, use="complete.obs", method = "spearman")


#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
linear_rm<-lm(Frequency ~ Length, data = data)

# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.

#Output:- 
#(Intercept)       Length  
#6.5015      -0.2943
# Negative relationship between the two variables.
ggplot(data, aes(x = Length, y = Frequency))+ 
  geom_point()+
  geom_abline(intercept = 6.5015, slope=-0.2943)

# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data, aes(x = Length, y = Frequency, label = rownames(data) ))+
  geom_abline(intercept = 6.5015, slope=-0.2943)+
  geom_text()

#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable.
# You can download this data frame from the material of week 6: t-test and friends. 
dat <- read.csv("digsym_clean.csv")
str(dat)
#summary(dat)

# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.

linear_rm_dat<-lm(correct_RT_2.5sd ~ Age, data = dat)

cast <- cast(dat, Subject + Age ~., fun.aggregate = mean, value = "correct_RT_2.5sd", na.rm = TRUE)
colnames(cast)[colnames(cast)=="(all)"] <- "RTavg"
head(cast)


# c) Now fit the regression model.
linear_rm_cast <- lm(RTavg ~ Age, data = cast)

# d) Let's go over the output - what's in there?
# How do you interpret the output?
linear_rm_cast
#output:
#Coefficients:
#  (Intercept)          Age  
#   637.93            21.22
#Here the slope is positive. It means a positive relation between the input and 
#the output


# e) Plot the data points and the regression line. 
ggplot(cast, aes(x = Age, y = RTavg))+ 
  geom_point() +
  geom_abline(intercept = 637.93, slope=21.22)

# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?

ggplot(data=cast, aes(x = residuals(linear_rm_cast))) + 
  geom_histogram() 

ggplot(data = cast, aes(sample = residuals(linear_rm_cast))) + 
  stat_qq()
#Histogram distribution looks like normally distributed but qq-plot is not nornally
#distributed for the residuals


# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.
cooks_dist <- cooks.distance(linear_rm_cast)
ggplot(cast, aes(x = Age, y = cooks_dist )) +
  geom_point() 

# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.

#observation 37 for subject 40 seems like an outlier
ggplot(cast, aes(x = Age, y = cooks_dist, label = Subject)) + 
  geom_point() + 
  geom_text()



# i) Make a subset of "cast" by excluding the influential subject and name it cast2.
#dim(cast)
#37  3
cast2 <- subset(cast, Subject!= 40)


# j) Fit the model from c) again, using cast2, and take a good look at the output.
linear_rm_cast2<-lm(RTavg ~ Age, data = cast2)
linear_rm_cast2

# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?
#Coefficients:
#  (Intercept)          Age  
#862.05                 11.98
#As we removed the outlier from the data, thus our model is not affected by it. As a 
#result, we got new slop value (11.98) comparing to the previous result (21.22)



# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?
ggplot(cast2, aes(x = Age, y = RTavg))+ 
  geom_point() +
  geom_abline(intercept = 862.05, slope = 11.98)

# m) Display the two plots side by side to better see what's going on.
plot1 <- ggplot(cast, aes(x = Age, y = RTavg))+ 
  geom_point() +
  geom_abline(intercept = 637.93, slope=21.22)+
  ggtitle("With outlier")


plot2 <- ggplot(cast2, aes(x = Age, y = RTavg))+ 
  geom_point() +
  geom_abline(intercept = 862.05, slope = 11.98)+
  ggtitle("Without outlier")

plot_grid(plot1, plot2)
  
  

# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.
summary(linear_rm_cast2)$r.squared

# o) How do you interpret R Squared?
#0.03493231
# We get the r squierd value 0.03493231. that means 3% of error rate we have. The smaller the number is, 
#the better fit (closer) to the regression line.  
 

