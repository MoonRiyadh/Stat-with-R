### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 13. Write the code below the questions. 
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
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
# a)There are 3 datasets on moodle, you can choose one of them to work with on this
#   assignment.  
#   Read in the data file of your choice (gender.Rdata, sem.Rdata OR relclause.Rdata) 
#   and assign it to a variable called "dat". 
#   See a description of the items in the datasets below.

getwd()
dat <- read.table("gender.Rdata.txt")

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German'):

# Christina schießt / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "shoot", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consisten or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

# b) Inspect "dat" and provide 2 plots. 
#    The first plot should provide insights about the relationship between WORD_TIME 
#    and ITEM_TYPE. 
#    For the second plot you should first subset the data using only RELWDINDEX == 0 and
#    then plot the WORD_TIME for the different conditions (ITEM_TYPE).

str(dat)
summary(dat)
#first plot
#boxplot(WORD_TIME~ITEM_TYPE, data = dat)
ggplot(dat, aes(x = ITEM_TYPE, y = WORD_TIME, group = PARTICIPANT, color =PARTICIPANT)) + 
  geom_boxplot() 


#second plot
dat_sub <- subset(dat, RELWDINDEX == 0)
ggplot(dat_sub, aes(x = ITEM_TYPE, y = WORD_TIME, group = PARTICIPANT, color =PARTICIPANT)) + 
  geom_boxplot()


# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation). 
#    Note that we are evaluating WORD_TIME as our reponse variable. 
#    What time intervals make sense for such an experiment?

#Based on the above boxplots, we decide to eliminate points whose word time exceeds 2000. 
#because these points are considered as outliers.
dat_sub2 <- subset(dat_sub, WORD_TIME < 2000)
ggplot(dat_sub2, aes(x = ITEM_TYPE, y = WORD_TIME, group = PARTICIPANT, color =PARTICIPANT)) + 
  geom_boxplot()

# d) Make a scatter plot where for each index word as the sentence progresses (RELWDINDEX),
#    the average reading time is shown for each of the two conditions (ITEM_TYPE).
#    Please use two different colours for the different conditions.
#ggplot(data = dat, aes(x = RELWDINDEX, y = ITEM_TYPE, color =ITEM_TYPE)) + 
#  geom_point() 
GB <- subset(dat, ITEM_TYPE=="GB")
GG <- subset(dat, ITEM_TYPE=="GG")


ggplot(GB, aes(x=factor(EXPWORD), y=WORD_TIME, group = ITEM_ID), fun.y="mean", geom="point", stat="identity") +
  geom_point()+
  ggtitle("scatter plot for GB")


ggplot(GG, aes(x=factor(EXPWORD), y=WORD_TIME, group = ITEM_ID), fun.y="mean", geom="point", stat="identity") +
  geom_point()+
  ggtitle("scatter plot for GG")



# e) You do not need to use ggplot here, just follow the example below.
#    The code is a plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?

print(xyplot(RELWDINDEX ~ WORD_TIME  | PARTICIPANT , dat, aspect = "xy",
             layout = c(4,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "RELWDINDEX",
             ylab = "Wordtime"))
#We can see that different particepant has different wordtime, some has higher value.

# f) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).
linear_model1 = lmer(WORD_TIME ~ RELWDINDEX + (1|PARTICIPANT), dat)
linear_mode1
#Random effects: Here, Std. Dev is higher, meaning that the data points are spread out over a wider range of values which is 143.0

#Fixed Effects:
#RELWDINDEX coefficient indicates the slope for the effectiveness of individual word. That means, if we increase one unit
#of the RELWDINDEX, the WORD TIME  will be increased by 6.483.

linear_mode2 = lmer(WORD_TIME ~ RELWDINDEX + (1|PARTICIPANT) + (1|ITEM_ID), dat)
linear_mode2

#random effects
#Here, Std. Dev is higher, meaning that the data points are spread out over a wider range of values which is 143.0, item id 28.46.

#Fixed Effects:
#RELWDINDEX coefficient indicates the slope for the effectiveness of individual word. That means, if we increase one unit
#of the RELWDINDEX, the WORD TIME  will be increased by 7.118. This time this value is much bigger then previous model because of 
#consideration of ITEM_ID

linear_mode3 = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|PARTICIPANT), dat)
linear_mode3

#random effects
#Here, Std. Dev is higher, meaning that the data points are spread out over a wider range of values which is 141.40, slope value is
#10.63 means that one unit increases 10.63 times in participent value.

#Fixed Effects:
#RELWDINDEX coefficient indicates the slope for the effectiveness of individual word. That means, if we increase one unit
#of the RELWDINDEX, the WORD TIME  will be increased by 6.519.


# g) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])


model2 = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|PARTICIPANT), dat)
print(dotplot(ranef(model2,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])

#From the graph, we can interprate word time for relwdindex of a participant.















#
