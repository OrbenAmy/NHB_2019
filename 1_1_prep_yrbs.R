##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 1.1: YRBS clean data and make variables
##########################################################################################
setwd(".../1_prep")

#######################################################
# Load libraries
#######################################################
library("foreign")
library("tidyverse")

#######################################################
# Read YRBS Dataset
#######################################################

# String indicating data directory
# Should lead to the folder containing the YRBS data
folder_data <- ".../2_datasets/"

# load data which is in one data file
# the data file was modified in SPSS to make all variables 'scale' instead of 'factor' or 'ordinal'
# the data filed can be obtained directly from the YRBS after signing  he relevant agreement
data <-
  read.spss(
    paste(folder_data, "yrbs_data.sav", sep = ""),
    use.value.labels = FALSE,
    to.data.frame = TRUE,
    use.missings = TRUE
  )

# change names of columns to lowercase
names(data) <- tolower(names(data))

#######################################################
# Choose appropriate years: 2007-2015
#######################################################
data <- data %>% filter(year > 2006)

#######################################################
# Recode Variables
# Make variables numeric, continuous
# Make variables dichotomous
#######################################################
# Recoded technology use variables:
#1 = none, 2 = <1hr a day, 3 = 1 hr a day, 4 = 2 hrs per day, 5 = 3 hrs per day, 6 = 4 hrs per day, 7 = 5 or more hrs per day
#On an average school day, how many hours do you watch TV?
data$q81_n <- as.numeric(data$q81) 
#On an average school day, how many hours do you play video or computer games or use a computer for something that is not school work? 
#(Count time spent on things such as Xbox, PlayStation, an iPod, an iPad or other tablet, a smartphone, YouTube, Facebook or other social networking tools, and the Internet.)
data$q82_n <- as.numeric(data$q82)

# Mean technology use variable
data$tech <-
  rowMeans(subset(data, select = c("q81_n", "q82_n")), na.rm = FALSE) 

# Make recoded suicide related outcome variables
#During the past 12 months, did you ever feel so sad or hopeless almost every day for two weeks or more in a row that you stopped doing some usual activities?
#1 = yes, 2 = no
data$q26_n <- ifelse(data$q26 == 1, 0, 1)
#During the past 12 months, did you ever seriously consider attempting suicide?
data$q27_n <- ifelse(data$q27 == 1, 0, 1)
#During the past 12 months, did you make a plan about how you would attempt suicide?
data$q28_n <- ifelse(data$q28 == 1, 0, 1)

# Create dichotomous suicide measure for questions 30 and 29
#During the past 12 months, how many times did you actually attempt suicide?
#1 = 0 times, 1 = 1 time, 2 = 2 or 3 times, 3 = 4 or 5 times, 5 = 6 or more times
# code those as 1 who have not attempted suicide, everyone else 0
data$q29_nd <- as.numeric(ifelse(data$q29 == 1, 1,
                                 ifelse(
                                   data$q29 == 2, 0,
                                   ifelse(data$q29 == 3, 0,
                                          ifelse(data$q29 == 4, 0,
                                                 ifelse(data$q29 == 5, 0, NA)))
                                 )))

#If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
#1 = I did not attempt suicide in last 12 months, 2 = yes, 3 = No
# Code those who did not attempt suicide or did not need to see a doctor as 1, those who had to see doctor as 0
data$q30_nd <- as.numeric(ifelse(data$q30 == 1, 1,
                                 ifelse(data$q30 == 2, 0,
                                        ifelse(data$q30 == 3, 1, NA))))

# Recode age as continuous
# recode "12 years old or younger" to 12 
# recode "18 years old or older" to 18
data$age_n <- ifelse(data$age == 1, 12,
                     ifelse(data$age == 2, 13,
                            ifelse(
                              data$age == 3, 14,
                              ifelse(data$age == 4, 15,
                                     ifelse(
                                       data$age == 5, 16,
                                       ifelse(data$age == 6, 17,
                                              ifelse(data$age == 7, 18, NA))
                                     ))
                            )))

# Recode sex
# 1 = male, 0 = female
data$sex_n <- ifelse(data$sex == 1, 0, ifelse(data$sex == 2, 1, NA))

# Recode grade as continuous
data$grade_n <- ifelse(data$grade == 1, 9,
                       ifelse(data$grade == 2, 10,
                              ifelse(
                                data$grade == 3, 11,
                                ifelse(data$grade == 4, 12, NA)
                              )))


# Create dichotomous race variable
# 1 = white, 0 = non-white
data$race_di <- ifelse(data$race4 == 1, 1, 0)

# Reverse "soda drinking" so 7 = never drink soda, 1 = drink 4 or more times per day
data$q77 <- 8 - data$q77

# Code that 1 = (cyber)bullied at school, 0 = not (cyber)bullied at school
#Bullying at school
data$q24_n <- ifelse(data$q24 == 1, 1, 0)
#Electronic bullying
data$q25_n <- ifelse(data$q25 == 1, 1, 0)

####################################################################################
# Look at what tech and well-being questions were answered together 
# question matrix
####################################################################################
vars <- c("q81", "q82", "q29", "q26", "q27", "q28")
vars_data <- subset(data, select = vars)
correlate <- psych::corr.test(vars_data)
#knitr::kable(correlate$n)

####################################################################################
# Calculate number of participants
####################################################################################
table(data$year)
table(data$sex_n)
table(data$age_n)
median(data$age_n, na.rm = TRUE)
sd(data$age_n, na.rm = TRUE)

####################################################################################
# Save as CSV for running large code on computer cluster
####################################################################################
write.csv(file = "1_1_prep_yrbs_data.csv", data)

