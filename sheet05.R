### Stats with R Exercise sheet 5

##########################
#Week 6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 2. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates') name, matriculation number. 
## Name: 1. H T M A Riyadh, 2. Abdallah Bashir, 3. Maria Francis
## Matriculation number: 1. 2577735, 2. 2577831, 3. 2573627

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)
library(dplyr)
library(reshape)
library(ggplot2)

# 1. Download the data file "digsym_clean.csv" from the moodle and save it in your 
# working directory. 

#getwd()

# 2. Read in the data into a variable called "data".
data <- read.csv("E:/UdS Study/WiSe 2019/Stat mit R/worksheet/05/digsym_clean.csv", header=TRUE)
#data <- read.csv("digsym_clean.csv")

# 3. Get rid of the column "X"
data <- select(data, -c(X))
#colnames(data)

# Say you're interested in whether people respond with different accuracy to 
# right vs. wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate 
  #             standard deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# 4. Apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function above for the arguments description).
#glimpse(data)
new_summarySE <- summarySE(data, "accuracy", "condition", na.rm=FALSE, conf.interval=.95)


# 5. Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
ggplot(new_summarySE, aes(x=condition, y = accuracy, fill = condition)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(x=condition, ymin = accuracy - new_summarySE$se, ymax = accuracy + new_summarySE$se), width = .25)

# 6. Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: Which assumption is violated?
t.test(accuracy ~ condition, data = data, var.equal = TRUE, conf.level = 0.95)


# 7. We need to reshape the data to only one observation (average accuracy) per subject 
# and right/wrong condition. Here we will use cast() which we discussed in the tutorial
# for sheet 2. 
# Collapse the data, 
# using cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T).
# Store the result in a new variable called "cdata". 
## Check ?cast or https://www.statmethods.net/management/reshape.html for more infos on 
## cast(). 

cdata <- cast(data, Subject + condition ~ ., mean, value = "accuracy" , na.rm = T)

# 8. Create histograms of the accuracy data depending on the right and wrong 
# condition and display them side by side.
ggplot(data=cdata, aes(x=cdata$`(all)` , fill=condition)) + 
  geom_histogram(position="dodge")

# 9. Display the same data in density plots. 
ggplot(data=cdata, aes(x=cdata$`(all)`, color=condition)) + 
  geom_density()

# 10. Based on the histograms and the density plots - are these data normally 
# distibuted?
# these data are not normally distributed. 

# 11. Create boxplots of the accuracy data.
ggplot(data=cdata, aes(x=condition, y=cdata$`(all)`)) + 
  geom_boxplot()

# 12. Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?
t.test(cdata$`(all)` ~ cdata$condition, paired = TRUE)

# 13. What does the output tell you? What conclusions do you draw?

#data:  cdata$`(all)` by cdata$condition
#t = 3.7691, df = 36, p-value = 0.000588
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.01303888 0.04341757
#sample estimates:
#  mean of the differences 
#0.02822823
# Conclusion: p value is < 0.05 so that we can reject the null hypothesis.


# 14. Compute the effect size using CohensD.
x <- cohensD(cdata$`(all)` ~ cdata$condition, method="paired")

# 15. Which effect size do we get? How do you interpret this result?
#0.6196291

# 16. In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format (this is the format we have been using in 
# class examples.)
# Let's do a transformation of our data set (cdata) to see what it would look like in a wide 
# format.
# Use spread() from the tidyr package.
spread_dat <- spread(cdata, key = cdata$condition, value = cdata$`(all)`)

# 17. Compute the t-test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.


# 18. Compare the t-test results from the wide-format and the long-format data. 
# What do you notice?


# 19. Compute CohensD on the wide format data. What do you notice?



# 20. Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the original data, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T).
# Store the result in a new variable called "cdat"
#head(data, 10)
cdat <- cast(data, StimulDS1.CRESP + Gender ~., fun.aggregate = mean, value = "correct_RT", na.rm = T)

# 21. Take a look at cdat using head().
head(cdat)

# 22. Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?
t.test(cdat$`(all)` ~ cdat$Gender)
#I choose independent test because accuracy is compared to Gender and there is no relation between male and female here.
#t = -2.8135, df = 1.0432, p-value = 0.209
#alternative hypothesis: true difference in means is not equal to 0
