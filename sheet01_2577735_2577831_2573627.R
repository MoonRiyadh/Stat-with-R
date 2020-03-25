### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 4. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name: 1. H T M A Riyadh, 2. Abdallah Bashir, 3. Maria Francis
## Matriculation number: 1. 2577735, 2. 2577831, 3. 2573627

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.

getwd()

## b) Get help with this function.
?getwd()

## c) Change your working directory to another directory.

setwd("/home/")
getwd()


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.

install.packages('languageR')
#installed.packages() 
#to check whether languageR was installed currectly
library(languageR)


## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)

# Answer start-------------
#  The head(dutchSpeakersDistMeta) function returns first six entries of "dutchSpeakersDistMeta". 
#  The tail(dutchSpeakersDistMeta) function returns last six entries of "dutchSpeakersDistMeta".
#summary shows the summary of the whole dataset, analysis of every column entry 
#Answer end----------------



## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
dim(dutchSpeakersDistMeta)
nrow(dutchSpeakersDistMeta)

#165 row


## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.


# boxplot(dutchSpeakersDistMeta$AgeYear,
#         main = "box plot of sex",
#         xlab = "Year",
#         ylab = "sex",
#         col = "orange",
#         border = "brown",
#         horizontal = TRUE,
#         notch = TRUE)

boxplot(AgeYear~Sex,
        data = dutchSpeakersDistMeta,
        main = "Boxplot for Sex and AgeYear", 
        xlab = "Sex",
        ylab = "AgeYear",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE)

## e) Does it seem as if either of the two groups has more variability in age?

#Ans: It seems female has more variability then male

## f) Do you see any outliers in either of the two groups?

#Ans: Male group has some outliers


## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?

# prepare a subset of male based on AgeYear
male_subset <- subset(dutchSpeakersDistMeta, Sex=='male' )
male_AgeYear <- male_subset$AgeYear

#mean
mean(male_AgeYear)
#1967.301

#standard deviation
sd(male_AgeYear)
#14.66258

# preparing a subset of female based on AgeYear
female_subset <- subset(dutchSpeakersDistMeta, Sex == 'female')
female_AgeYear <- female_subset$AgeYear
mean(female_AgeYear)
#1966.889
sd(female_AgeYear)
#15.87411
# from the mean and standard deviation of the AgeYear per group, it seems that they do not differ much in age 



## h) What do the whiskers of a boxplot mean?
#The whiskers represent the highest and lowest observations of a dataset within the minimum and maximum (minimum = q1-1.5*IQR, Maximum = Q3 = 1.5*IQR
#where Q1 25% pecentile,  Q3 75% percentile.


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

#Answer: 
#It is ratio scale (measuring 'and then' occurance time, natural zero). It is Discerete, 


## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

#Answer: Because dataframe can contain multiple  datatypes but matrix can only store one datatype, 
#also Data frames are more convenient if you frequently refer to its columns by name 



## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps <- c(1:25)


## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)


## e) Create a dataframe for this data. Assign this to 'stories'. 

stories <- data.frame(
        participantID = pps,
        observations = obs
)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?

summary(stories)
#class(stories$participantID)
str(stories)

# variable 'pps' is the integer class

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?

stories$participantID <- factor(1:25)
#class(pps)
#factors can easily use to order the data. and also it is prefered asa a datatype in categories

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

hist(stories$observations, breaks = 8)


## i) Create a kernel density plot using density().
#kd_obs <- density(obs)
density(obs)
plot(density(obs))


## j) What is the difference between a histogram and a kernel density plot?

#Answer: 
# Kernel density plot gives the smooth and more accurate view of distribution. But as 
# histogram depands on block, it may give the overview of the distribution but not exactly.


## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

hist(obs, breaks = 8,
     col = "grey",
     prob = TRUE, 
     main = "Histogram with the kernel density plot")
lines(density(obs), col = "red")



###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.

x <- seq(from = -5, to = 5, by = 0.1)


## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
?dnorm()
y <- dnorm(x, mean = 0, sd = 1, log = FALSE)


## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(scale(x, center = TRUE, scale = TRUE),y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(scale(x, center = TRUE, scale = TRUE), y, ylim =c(0, 0.8), type = "l")


## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
?abline()
abline(v = median(x), lty = 2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
beaver1
b1temp <- beaver1$temp


## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.

mean_beaver1 <- mean(b1temp)
sd_beaver1 <- sd(b1temp)
normal_dist <- dnorm(b1temp, mean = mean_beaver1, sd = sd_beaver1, log = FALSE)
plot(normal_dist)
#plot(dnorm(b1temp, mean = mean_beaver1, sd = sd_beaver1, log = FALSE))



## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?

pnorm(36.91, sd = sd_beaver1, mean = mean_beaver1)
pnorm(38.13, sd = sd_beaver1, mean = mean_beaver1)


## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?

r_norm1 <- rnorm(20, mean = mean_beaver1, sd = sd_beaver1)
r_norm2 <- rnorm(20, mean = mean_beaver1, sd = sd_beaver1)
r_norm3 <- rnorm(20, mean = mean_beaver1, sd = sd_beaver1)
r_norm4 <- rnorm(20, mean = mean_beaver1, sd = sd_beaver1)
r_norm5 <- rnorm(20, mean = mean_beaver1, sd = sd_beaver1)


hist(r_norm1)
hist(r_norm2)
hist(r_norm3)
hist(r_norm4)
hist(r_norm5)
#plot(hist(r_norm))

#yes they are differenet because it is random sampling


