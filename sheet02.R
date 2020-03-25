###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 



# 2. Read in the data into a variable called "dat".
#dat <- read.csv("/home/abdallah/Documents/uni/statr/assignment2/digsym.csv", header=TRUE)
dat <- read.csv("E:/UdS Study/WiSe 2019/Stat mit R/worksheet/02/digsym.csv", header=TRUE)
# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
nrow(dat)
#no. of rows = 3701
ncol(dat)
#no. of colums = 11

# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.
head(dat,n= 20)
tail(dat, n=20)

# 7. Is there any missing data in any of the columns?

#is.na(dat)
any(is.na(dat))

#There are some missing data in the columns
# 8. Get rid of the row number column.
dat <- dat[,-1] #??
ncol(dat)

# 9. Put the Sub_Age column second.

dat <- dat %>% select ("ExperimentName", "Sub_Age", "Group", "Gender", 
                       "List", "SubTrial", "StimulDS1.CRESP", "StimulDS1.RESP", 
                       "StimulDS1.RT", "File")

head(dat$ExperimentName)

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
#names(dat)
dat$ExperimentName <- factor("Kopie")

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- subset(dat, List != "Trial:1")
dat <- data2
dat <- subset(dat, List != "Trial:1")
rm(data2)


  # 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat <- separate(dat, "Sub_Age", into = c("Subject", "Age"))

# 13. Make subject a factor.
#y <- dat
dat$Subject <- as.factor(dat$Subject)



# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".

#dat$File <- gsub("\\d\\ | _$","", dat$File)
dat$File <- sub("_", "", dat$File)
dat$File <- gsub("[[:digit:]]", "", dat$File)
head(dat$File)

  # 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).

?str_pad
dat$File <- str_pad(dat$File,8, side = "right", pad = "0")
head(dat$File)

# 16. Remove the column "List".
dat <- dat[ , !(names(dat) %in% 'List')]
# 17. Change the data type of "Age" to integer.
dat$Age <- as.integer(dat$Age)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?
any(is.na(dat))
dim(dat)
sum(is.na(dat))
# we don't hace NA values 
# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.
dat$accuracy <- ifelse(dat$StimulDS1.CRESP == dat$StimulDS1.RESP, 1, 0)
head(dat)

# 20. How many wrong answers do we have in total?
wrong <- nrow(dat) - sum(dat$accuracy)

#We have 185 wrong answers.

# 21. What's the percentage of wrong responses?
per <- wrong / nrow(dat)

#There are approx. 0.056 = 5.6% wrong answers. 

# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 

correctResponses <- subset(dat, dat$accuracy == 1)

# 23. Create a boxplot of StimulDS1.RT - any outliers?

boxplot(dat$StimulDS1.RT)
#Yes, there are actually many outliers, all higher than the mean

# 24. Create a histogram of StimulDS1.RT with bins set to 50.

hist(dat$StimulDS1.RT, nclass = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?
summary(dat$StimulDS1.RT)

#The distribution is positively skewed.
#There are several large values and outliers, but the highest value (13852) is suspiciously high,
#especially judging by the 3rd Quantile (1399, 10 times lower)

# 26. View summary of correct_RT.


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".
outliers <- boxplot(dat$StimulDS1.RT, plot=FALSE)$out
cleaned <- dat[-which(dat$StimulDS1.RT %in% outliers),]
hist(cleaned$StimulDS1.RT)
## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now we want to define a cutoff value for the StimulDS1.RT variable in the correctResp dataset.
# Values should not differ more than 2.5 standard deviations from the grand mean of this variable.
# This condition should be applied in a new variable called "correct_RT_2.5sd", which prints NA 
# if an RT value is below/above the cutoff. 


# 29. Take a look at the outlier observations.
# Any subjects who performed especially poorly?


# 30. How many RT outliers are there in total?


# 31. Plot a histogram and boxplot of the correct_RT_2.5sd column again - nice and clean eh?


# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 33. Sort in ascending order or plot of the average accuracies per subject.


# 34. Would you exclude any subjects, based on their avrg_accuracy performance?


# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created 
# into a new file called "digsym_clean.csv".