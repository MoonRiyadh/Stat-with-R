##########################
#Week 11: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: 1. H T M A Riyadh, 2. Maria Francis
## Matriculation number: 1. 2577735, 2. 2573627

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################
library(ggplot2)

####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.
speed_dating <- read.csv(file = "Speed Dating Data.csv")
#head(speed_dating)
#str(speed_dating)



#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.
regression_model <- glm(dec ~ fun + attr  + sinc + intel , data = speed_dating, family="binomial")
regression_model
#Output interpretation:
#Increasing one unit in fun will increase 0.33711 times of a partner selection while other things considered as constant.

ggplot(data = speed_dating , aes(x= factor(gender), y = fun_o )) + 
  geom_boxplot()

ggplot(data = speed_dating , aes(x= factor(gender), y = intel_o  )) + 
  geom_boxplot()
#Gender 0 gives less importance then gender 1

ggplot(data = speed_dating, aes(x= factor(gender), y = sinc_o )) + 
  geom_boxplot()
#We find both of the box plots are same. that means both gender 0 and 1 give equal importance to sincereity.

ggplot(data = speed_dating , aes(x= factor(gender), y = attr_o )) + 
  geom_boxplot()
#Females(Gender 0) gives more importance to attractiveness than males (Gender 1)

ggplot(data = speed_dating , aes(x= factor(gender), y = amb_o )) + 
  geom_boxplot()
#Females(Gender 0) give more importance to Ambitions than males.

ggplot(data = speed_dating , aes(x= factor(gender), y = shar_o )) + 
  geom_boxplot()
# We find both of the box plots are same, that means  both male and female give equal importance for sharing hobbies


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

expand_model1 <- glm(dec ~ fun + attr + intel + sinc + id, data = speed_dating , family="binomial")
summary(expand_model1)
#Id number of partners has an influence on Decision and this decision is done by based on fun, attr, sync values.
#We also can reject the null hypothesis because this model has significance lavel 0.05

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.

expand_model2 <- glm(dec ~ attr  + sinc + intel + fun + id + pid, data = speed_dating, family="binomial")
summary(expand_model2)
#We can reject the null hypothesis because this model has significance lavel 0.05. We also see that 
#decsion between the partners is also affected by id no but not by pid
 

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

fit_model1 <- glm(dec ~ fun + id , data = speed_dating, family="binomial")
summary(fit_model1) 

fit_model2 <- glm(dec ~ fun  + attr + id, data = speed_dating, family="binomial")
summary(fit_model2) 

fit_model3 <- glm(dec ~ fun + attr + sinc + id, data = speed_dating, family="binomial")
summary(fit_model3) 

fit_model4 <- glm(dec~fun + attr + sinc + intel + fun + 
                    (1 + attr + sinc + intel + fun|pid) + 
                    (1+ attr + sinc + intel + fun|iid), 
                  data = speed_dating, family = "binomial")
summary(fit_model4) 

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?

summary(fit_model1) 
summary(fit_model2) 
summary(fit_model3) 
summary(fit_model4) 

#different model has different ACI values, so the models I designed affected my conclusions


####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(p)
})
summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(p, aes(x = math, y = num_awards, color = prog)) +
  stat_summary(fun.y="mean", geom="bar")
#Students how have Academic program type and get high score in Math, gets more Awards


#(7) Run a generalized linear model to test for significance of effects.

glm_mmodel1 <- lm(num_awards ~ math + prog , data = p)
summary(glm_mmodel1)

#Intercept
#if there is no program type and a student get zero in math, then the number of awards will decrease by 2.19550.
#slope
#Increasing math score by one unit can increase the number of award by 0.047889 times and considering other things constant.



#(8) Do model comparisons do find out whether the predictors significantly improve model fit.
glm_mmodel2 <- glm(num_awards ~ math, data = p)
summary(glm_mmodel1) 
summary(glm_mmodel2) 
# If we comapre R square values of  both these models, glm_mmodel1 has high R square value of  0.2773  
#This concludes that if we include predictor prog,  model glm_mmodel1 significantly improves by 27.73%


#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.
lm_model1<-lm(num_awards ~ math + prog, data = p, family = "gaussian")
summary(lm_model1)

