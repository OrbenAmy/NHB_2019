##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 1.2: MTF clean data and make variables
##########################################################################################
setwd(".../1_prep")

#######################################################
# Load libraries
#######################################################
library("foreign")
library("tidyverse")

#######################################################
# Read MTF Dataset
#######################################################

# String indicating data directory
# Should lead to the folder containing the YRBS data
folder_data <- ".../2_datasets/"

# load data which is in one data file
# the data file was merged manually from separate data files
# these data files can be obtained directly from the mtf after signing the relevant agreement
data <-
  read.spss(
    paste(folder_data, "mtf_merged_data.sav", sep = ""),
    use.value.labels = FALSE,
    to.data.frame = TRUE,
    use.missings = TRUE
  )

# change names of columns to lowercase
names(data) <- tolower(names(data))

#######################################################
# Choose appropriate years: 2008 - 2016
#######################################################
data <- data %>% filter(v1 > 2007)

#######################################################
# Recode variables
# Make variables numeric, continuous 
#######################################################
# Recode grade level variables as coding not consistent
# Grade 8 = 0, Grade 10 = 1
data$v501 <-
  (ifelse(data$v501 == "-9", NA, 
          ifelse(
            data$v501 == "2", 0,
            ifelse(data$v501 == "4", 1,
                   ifelse(
                     data$v501 == "8", 0,
                     ifelse(data$v501 == "10", 1, NA)
                   ))
          )))


# Recode gender
# Female = 0, Male = 1
data$v7202n <- ifelse(data$v7202 == 1, 1, 0)

# Reverse of negative well-being variables
data$v8503rev <- 6 - data$v8503 #I feel I do not have much to be proud of
data$v8502rev <- 6 - data$v8502 #Life often seems meaningless
data$v8511rev <- 6 - data$v8511 #I feel that I can't do anything right
data$v8513rev <- 6 - data$v8513 #I feel that my life is not very useful
data$v7502rev <- 6 - data$v7502 #There is always someone I can turn to if I need help
data$v8509rev <- 6 - data$v8509 #The future often seems hopeless

# Reverse of "I often sleep less than I should" so that higher number means more sleep
data$v8531rev <- 7 - data$v8531

# Recode ethnicity
# white = 1, other = 0
data$v1070r <-
  ifelse(data$v1070 == 2, 1, ifelse(data$v1070 == 1, 0, 0))

# Make mean technology use variables (one for each subset)
data$tech <-
  rowMeans(subset(
    data,
    select = c("v7326", "v7325", "v7381", "v7552"),
    na.rm = FALSE
  ))

data$tech2 <-
  rowMeans(subset(
    data,
    select = c("v7544", "v7551", "v7553", "v7589", "v7590", "v7562", "v7563"),
    na.rm = FALSE
  ))

####################################################################################
# Make question matrix
# To show how different questions are asked in different questionnaires.
# Important as some correlations cannot be done.
####################################################################################
vars_x <- c(
  "v7326",
  "v7325",
  "v7381",
  "v7552",
  "v7544",
  "v7551",
  "v7553",
  "v7589",
  "v7590",
  "v7562",
  "v7563"
)

vars_y <- c(
  "v7255",
  "v7302",
  "v8512",
  "v8502",
  "v8505",
  "v8509",
  "v8514",
  "v8536",
  "v8504",
  "v8501",
  "v8508",
  "v8503",
  "v8507",
  "v8511",
  "v8513",
  "v7501",
  "v7502",
  "v7504",
  "v7505",
  "v7507",
  "v7508"
)

vars_xy <- c(vars_x, vars_y)
names_xy <- c(
  "TV_wknd",
  "TV_wk",
  "Int_news",
  "SM_use",
  "Cmp_schl",
  "internet",
  "Games",
  "SNS_Use",
  "Video",
  "Texting",
  "Calling",
  "Talk_to",
  "Happy",
  "SE1",
  "DS1",
  "DS2",
  "DS3",
  "DS4",
  "Outlook",
  "SE2",
  "SE3",
  "SE4",
  "SE5",
  "SE6",
  "SE7",
  "SE8",
  "LO1",
  "LO2",
  "LO3",
  "LO4",
  "LO5",
  "LO6"
)

vars_data <- subset(data, select = vars_xy)
names(vars_data) <- names_xy
correlate <- psych::corr.test(vars_data)
#write.csv(correlate$n, file = "MTF_Correlate.csv")
knitr::kable(correlate$n)

####################################################################################
# Calculate number of participants
####################################################################################
table(data$v1)
table(data$v7202n)

####################################################################################
# Save as CSV for running large code on computer cluster
####################################################################################
write.csv(file = "1_2_prep_mtf_data.csv", data)
