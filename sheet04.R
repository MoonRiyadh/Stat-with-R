### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorical Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 25th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates) name, matriculation number. 
## Name: 1. H T M A Riyadh, 2. Abdallah Bashir, 3. Maria Francis
## Matriculation number: 1. 2577735, 2. 2577831, 3. 2573627

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

dbinom(4, size = 12, prob = 0.2)
#0.1328756

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
dbinom(0, size = 12, prob = 0.2) +
  dbinom(1, size = 12, prob = 0.2) +
  dbinom(2, size = 12, prob = 0.2) +
  dbinom(3, size = 12, prob = 0.2) +
  dbinom(4, size = 12, prob = 0.2)

#0.9274445
# or we can do it using pbinom
pbinom(4, 12, 0.2)
#0.9274445

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(languageR)
summary(dutchSpeakersDistMeta)
lapply(dutchSpeakersDistMeta, class)
# All the variables are Factor

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
table(dutchSpeakersDistMeta$AgeGroup)
table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)



##    Visualize your data with a single bar plot (use ggplot) that represents the counts with 
##    respect to each age group and each sex.
library(ggplot2)
group_by_sex <- table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)
data_ <- data.frame(group_by_sex)
colnames(data_) <- c("Age_Group", "Sex", "Age")

ggplot(data_, aes(x=Age_Group, y=Age, fill = Sex)) + 
  geom_bar(position="dodge", stat="identity")

## c) Inspect the table you created in b). Does it look like there could be a significant 
##    difference between the sexes?

# In age group 35 to 44, male is greater than female but in other age group, female is significantly 
# greater than male 

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group using the function chisq.test. 
##    Look at the help of this function. 
##    Then use the  function to calculate whether there's a difference in our table from b). 
##    Is there a significant difference in age group?

chisq.test(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)
#X-squared = 3.2785, df = 4, p-value = 0.5124
# As the p-value 0.5124 is greater than the .05 significance level, 
# we do not reject the null hypothesis. So there's a difference between males and females 
# regarding their age group

## e) What are the degrees of freedom for our data? How are they derived?
# Age Groups are degrees of freedom of our data and they are derived from Age

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll consider a paper on therapeutic touch 
##    (google it, if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    Several practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

#The null hypothesis is that therapeutic touch is not a real phenomenon, and that you cannot identify
#whether someone's hand is over your own.
#People should essentially be guessing which hand is under the experimenter's hand, so it should be a 50% chance
#of guessing correctly, or 140 people should guess correctly.


## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 


#Degrees of freedom:  n - 1 = 2 - 1 = 1
#'a = 0.05 -> rejection at chi >= 3.84
#x^2 = ((123-140)^2/140) * 2 = 4.13
#So we can reject the null hypothesis, but we can only conclude that therapeutic touch 
#does the opposite of what you want it to do, because actually the chi score is only
#that high because significantly few people actually felt the therapeutic touch.


## c) Now calculate significance using the binomial test as we used it in exercise 1.


#solve with R:
pbinom(123, 280, 0.5)
#[1] 0.02420056

#This means, if we do a one-sided test with 'a = 0.05 as in b), we reject the null hypothesis.

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?



##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

#Suppose our data set contains the Red Blood Cell Count (RBC) readings of 104 diabetic patients 
# and that were assessed by the Private Clinic and the hospital laboratory. 
# Is the rate of out-of-range readings different for the two assessors? In this situation, I would choose
# McNemar's test. 
# Here, I am not comparing a sample from two different places' patients. All patients have readings 
# from both assessors, so the data are paired and it did not come from contingency tables.
#Choosing the normal ChiSquare test in place of McNemar's test, it will misinterpreate the data and gives 
# wrong prediction. 

