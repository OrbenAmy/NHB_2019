##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 1.3: MCS clean data and make variables
##########################################################################################
setwd(".../1_prep")

#######################################################
# Load libraries
#######################################################
library("foreign")
library("tidyverse")
library("psych")

#######################################################
# Read MCS Dataset
#######################################################

# String indicating data directory
# Should lead to the folder containing the YRBS data
folder_data <- ".../2_datasets/mcs_raw_data/"

# load data which is in separate data files
# these data files can be access directly from the mcs once a data sharing agreement is agreed on
data1 <-
  read.csv(paste(folder_data, "mcs6_cm_assessment.csv", sep = ""))
data2 <-
  read.csv(paste(folder_data, "mcs6_cm_derived.csv", sep = ""))
data3 <-
  read.csv(paste(folder_data, "mcs6_cm_interview.csv", sep = ""))
data4 <-
  read.csv(paste(folder_data, "mcs6_cm_measurement.csv", sep = ""))
data5 <-
  read.csv(paste(folder_data, "mcs6_family_derived.csv", sep = ""))
data6 <-
  read.csv(paste(folder_data, "mcs6_parent_cm_interview.csv", sep = ""))
data7 <-
  read.csv(paste(folder_data, "mcs6_parent_assessment.csv", sep = ""))
data8 <-
  read.csv(paste(folder_data, "mcs6_parent_derived.csv", sep = ""))
data9 <-
  read.csv(paste(folder_data, "mcs6_parent_interview.csv", sep = ""))
data10 <-
  read.csv(paste(folder_data, "mcs6_proxy_partner_interview.csv", sep = ""))
data11 <-
  read.csv(paste(folder_data, "mcs6_hhgrid.csv", sep = "")) 

# change names of columns to lowercase
names(data1) <- tolower(names(data1))
names(data2) <- tolower(names(data2))
names(data3) <- tolower(names(data3))
names(data4) <- tolower(names(data4))
names(data5) <- tolower(names(data5))
names(data6) <- tolower(names(data6))
names(data7) <- tolower(names(data7))
names(data8) <- tolower(names(data8))
names(data9) <- tolower(names(data9))
names(data10) <- tolower(names(data10))
names(data11) <- tolower(names(data11))

#######################################################
# Combine different datasets
# We combine the datasets by making identification 
# variables for each child. We cannot use the family
# identification variables because they obfuscate 
# twins/triplets.
#######################################################

###################
# Cohort Members
###################
data1$mcsid1 <-
  ifelse(
    data1$fcnum00 == 1,
    paste(as.character(data1$mcsid), "_1", sep = ""),
    ifelse(
      data1$fcnum00 == 2,
      paste(as.character(data1$mcsid), "_2", sep = ""),
      ifelse(data1$fcnum00 == 3, paste(as.character(data1$mcsid), "_3", sep = ""), NA)
    )
  )
data1$mcsid <- as.character(data1$mcsid)
data2$mcsid2 <-
  ifelse(
    data2$fcnum00 == 1,
    paste(as.character(data2$mcsid), "_1", sep = ""),
    ifelse(
      data2$fcnum00 == 2,
      paste(as.character(data2$mcsid), "_2", sep = ""),
      ifelse(data2$fcnum00 == 3, paste(as.character(data2$mcsid), "_3", sep = ""), NA)
    )
  )
data3$mcsid3 <-
  ifelse(
    data3$fcnum00 == 1,
    paste(as.character(data3$mcsid), "_1", sep = ""),
    ifelse(
      data3$fcnum00 == 2,
      paste(as.character(data3$mcsid), "_2", sep = ""),
      ifelse(data3$fcnum00 == 3, paste(as.character(data3$mcsid), "_3", sep = ""), NA)
    )
  )
data4$mcsid4 <-
  ifelse(
    data4$fcnum00 == 1,
    paste(as.character(data4$mcsid), "_1", sep = ""),
    ifelse(
      data4$fcnum00 == 2,
      paste(as.character(data4$mcsid), "_2", sep = ""),
      ifelse(data4$fcnum00 == 3, paste(as.character(data4$mcsid), "_3", sep = ""), NA)
    )
  )
data5$mcsid5 <- as.character(data5$mcsid)

###################
# Parents
###################
data6$mcsid6 <-
  paste(as.character(data6$mcsid),
        as.character(data6$fpnum00),
        sep = "_")
data7$mcsid7 <-
  paste(as.character(data7$mcsid),
        as.character(data7$fpnum00),
        sep = "_")
data8$mcsid8 <-
  paste(as.character(data8$mcsid),
        as.character(data8$fpnum00),
        sep = "_")
data9$mcsid9 <-
  paste(as.character(data9$mcsid),
        as.character(data9$fpnum00),
        sep = "_")
data10$mcsid10 <-
  paste(as.character(data10$mcsid),
        as.character(data10$fpnum00),
        sep = "_")

###################
# Merge
###################
# Cohort Members
data_cm <-
  dplyr::left_join(data1, data2[, is.na(match(names(data2), names(data1)))], by = c("mcsid1" = "mcsid2"))
data_cm <-
  dplyr::left_join(data_cm, data3[, is.na(match(names(data3), names(data_cm)))], by = c("mcsid1" = "mcsid3"))
data_cm <-
  dplyr::left_join(data_cm, data4[, is.na(match(names(data4), names(data_cm)))], by = c("mcsid1" = "mcsid4"))
data_cm <-
  dplyr::left_join(data_cm, data5[, is.na(match(names(data5), names(data_cm)))], by = c("mcsid" = "mcsid5"))

# Parents
data_pa <-
  dplyr::left_join(data6, data7[, is.na(match(names(data7), names(data6)))], by = c("mcsid6" = "mcsid7"))
data_pa <-
  dplyr::left_join(data_pa, data8[, is.na(match(names(data8), names(data_pa)))], by = c("mcsid6" = "mcsid8"))
data_pa <-
  dplyr::left_join(data_pa, data9[, is.na(match(names(data9), names(data_pa)))], by = c("mcsid6" = "mcsid9"))

data_pa$mcsid1_r <-
  ifelse(
    data_pa$fcnum00 == 1,
    paste(as.character(data_pa$mcsid), "_1", sep = ""),
    ifelse(
      data_pa$fcnum00 == 2,
      paste(as.character(data_pa$mcsid), "_2", sep = ""),
      ifelse(data_pa$fcnum00 == 3, paste(
        as.character(data_pa$mcsid), "_3", sep = ""
      ), NA)
    )
  )
data_pa$fpnum00_r <- ifelse(data_pa$fpnum00 == 1, 1, 0)
data_pa_1 <- data_pa %>% filter(fpnum00_r == 1)

# Merge cohort members and parents, not merging duplicate rows
data <-
  dplyr::left_join(data_cm, data_pa_1[, is.na(match(names(data_pa_1), names(data_cm)))], by = c("mcsid1" = "mcsid1_r"))

###################
# Remove datasets
###################
rm(data1)
rm(data2)
rm(data3)
rm(data4)
rm(data5)
rm(data6)
rm(data7)
rm(data8)
rm(data9)
rm(data10)
rm(data11)
gc()

#######################################################
# Set missing values: any negative numbers
#######################################################
is.na(data[, ]) = data[, ] < 0

#######################################################
# Recode Well-being Measures: Cohort Member
# Change measures to 10 point scales 
# Reverse scales if necessary
#######################################################
# Well-being measures to a 10 point scale and reverse
data$fcscwk00r <- (10 - 1) * (data$fcscwk00 - 1) / (7 - 1) + 1
data$fcwylk00r <- (10 - 1) * (data$fcwylk00 - 1) / (7 - 1) + 1
data$fcfmly00r <- (10 - 1) * (data$fcfmly00 - 1) / (7 - 1) + 1
data$fcfrns00r <- (10 - 1) * (data$fcfrns00 - 1) / (7 - 1) + 1
data$fcschl00r <- (10 - 1) * (data$fcschl00 - 1) / (7 - 1) + 1
data$fclife00r <- (10 - 1) * (data$fclife00 - 1) / (7 - 1) + 1

data$fcsati00r <- (10 - 1) * (data$fcsati00 - 1) / (4 - 1) + 1
data$fcgdql00r <- (10 - 1) * (data$fcgdql00 - 1) / (4 - 1) + 1
data$fcdowl00r <- (10 - 1) * (data$fcdowl00 - 1) / (4 - 1) + 1
data$fcvalu00r <- (10 - 1) * (data$fcvalu00 - 1) / (4 - 1) + 1
data$fcgdsf00r <- (10 - 1) * (data$fcgdsf00 - 1) / (4 - 1) + 1

data$fcmdsa00r <- (10 - 1) * (data$fcmdsa00 - 1) / (3 - 1) + 1
data$fcmdsb00r <- (10 - 1) * (data$fcmdsb00 - 1) / (3 - 1) + 1
data$fcmdsc00r <- (10 - 1) * (data$fcmdsc00 - 1) / (3 - 1) + 1
data$fcmdsd00r <- (10 - 1) * (data$fcmdsd00 - 1) / (3 - 1) + 1
data$fcmdse00r <- (10 - 1) * (data$fcmdse00 - 1) / (3 - 1) + 1
data$fcmdsf00r <- (10 - 1) * (data$fcmdsf00 - 1) / (3 - 1) + 1
data$fcmdsg00r <- (10 - 1) * (data$fcmdsg00 - 1) / (3 - 1) + 1
data$fcmdsh00r <- (10 - 1) * (data$fcmdsh00 - 1) / (3 - 1) + 1
data$fcmdsi00r <- (10 - 1) * (data$fcmdsi00 - 1) / (3 - 1) + 1
data$fcmdsj00r <- (10 - 1) * (data$fcmdsj00 - 1) / (3 - 1) + 1
data$fcmdsk00r <- (10 - 1) * (data$fcmdsk00 - 1) / (3 - 1) + 1
data$fcmdsl00r <- (10 - 1) * (data$fcmdsl00 - 1) / (3 - 1) + 1
data$fcmdsm00r <- (10 - 1) * (data$fcmdsm00 - 1) / (3 - 1) + 1

data$fcscwk00r <- 11 - data$fcscwk00r
data$fcwylk00r <- 11 - data$fcwylk00r
data$fcfmly00r <- 11 - data$fcfmly00r
data$fcfrns00r <- 11 - data$fcfrns00r
data$fcschl00r <- 11 - data$fcschl00r
data$fclife00r <- 11 - data$fclife00r

data$fcsati00r <- 11 - data$fcsati00r
data$fcgdql00r <- 11 - data$fcgdql00r
data$fcdowl00r <- 11 - data$fcdowl00r
data$fcvalu00r <- 11 - data$fcvalu00r
data$fcgdsf00r <- 11 - data$fcgdsf00r

data$fcmdsa00r <- 11 - data$fcmdsa00r
data$fcmdsb00r <- 11 - data$fcmdsb00r
data$fcmdsc00r <- 11 - data$fcmdsc00r
data$fcmdsd00r <- 11 - data$fcmdsd00r
data$fcmdse00r <- 11 - data$fcmdse00r
data$fcmdsf00r <- 11 - data$fcmdsf00r
data$fcmdsg00r <- 11 - data$fcmdsg00r
data$fcmdsh00r <- 11 - data$fcmdsh00r
data$fcmdsi00r <- 11 - data$fcmdsi00r
data$fcmdsj00r <- 11 - data$fcmdsj00r
data$fcmdsk00r <- 11 - data$fcmdsk00r
data$fcmdsl00r <- 11 - data$fcmdsl00r
data$fcmdsm00r <- 11 - data$fcmdsm00r

###################
# Check correlations
###################
ds <-
  data[c(
    "fcmdsa00r",
    "fcmdsb00r",
    "fcmdsc00r",
    "fcmdsd00r",
    "fcmdse00r",
    "fcmdsf00r",
    "fcmdsg00r",
    "fcmdsh00r",
    "fcmdsi00r",
    "fcmdsj00r",
    "fcmdsk00r",
    "fcmdsl00r",
    "fcmdsm00r",
    "fcsati00r",
    "fcgdql00r",
    "fcdowl00r",
    "fcvalu00r",
    "fcgdsf00r",
    "fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r"
  )]
correlate <- psych::corr.test(ds)
correlate$r
rm(ds)

#######################################################
# Recode Well-being measures: Parent
# Reverse measures
#######################################################
data$fpsdro00 <- 2 - data$fpsdro00
data$fpsdhs00 <- 2 - data$fpsdhs00
data$fpsdtt00 <- 2 - data$fpsdtt00
data$fpsdsp00 <- 2 - data$fpsdsp00
data$fpsdmw00 <- 2 - data$fpsdmw00
data$fpsdfs00 <- 2 - data$fpsdfs00
data$fpsdfb00 <- 2 - data$fpsdfb00
data$fpsdud00 <- 2 - data$fpsdud00
data$fpsddc00 <- 2 - data$fpsddc00
data$fpsdnc00 <- 2 - data$fpsdnc00
data$fpsdoa00 <- 2 - data$fpsdoa00
data$fpsdpb00 <- 2 - data$fpsdpb00
data$fpsdcs00 <- 2 - data$fpsdcs00
data$fpsdgb00 <- 2 - data$fpsdgb00
data$fpsdfe00 <- 2 - data$fpsdfe00

data$fconduct <- 11 - data$fconduct
data$fhyper <- 11 - data$fhyper
data$fpeer <- 11 - data$fpeer
data$femotion <- 11 - data$femotion
data$febdtot <- 41 - data$febdtot

###################
# Check correlations
###################
ds <-
  data[c(
    "fpsdpf00",
    "fpsdro00",
    "fpsdhs00",
    "fpsdsr00",
    "fpsdtt00",
    "fpsdsp00",
    "fpsdor00",
    "fpsdmw00",
    "fpsdhu00",
    "fpsdfs00",
    "fpsdgf00",
    "fpsdfb00",
    "fpsdud00",
    "fpsdlc00",
    "fpsddc00",
    "fpsdnc00",
    "fpsdky00",
    "fpsdoa00",
    "fpsdpb00",
    "fpsdvh00",
    "fpsdst00",
    "fpsdcs00",
    "fpsdgb00",
    "fpsdfe00",
    "fpsdte00", 
    "fconduct", 
    "fhyper",
    "femotion",
    "fpeer", 
    "fprosoc",
    "febdtot"
  )]
correlate <- psych::corr.test(ds)
correlate$r
rm(ds)

#######################################################
# Recode Digital Technology Use measures
#######################################################
# Reverse computer ownership measure
data$fccmex00r <- 3 - data$fccmex00

# Convert measures to 10 point scale
data$fctvho00r <- (10 - 1) * (data$fctvho00 - 1) / (8 - 1) + 1
data$fccomh00r <- (10 - 1) * (data$fccomh00 - 1) / (8 - 1) + 1
data$fcinth00r <- (10 - 1) * (data$fcinth00 - 1) / (8 - 1) + 1
data$fcsome00r <- (10 - 1) * (data$fcsome00 - 1) / (8 - 1) + 1
data$fccmex00r <- (10 - 1) * (data$fccmex00r - 1) / (2 - 1) + 1

# Make technology use measure
data$tech <-
  rowMeans(subset(
    data,
    select = c(
      "fctvho00r",
      "fccomh00r",
      "fccmex00r",
      "fcinth00r",
      "fcsome00r"
    )
  ), na.rm = FALSE)

###################
# Check correlations
###################
tu <-
  data[c("fctvho00r",
         "fccomh00r",
         "fccmex00r",
         "fcinth00r",
         "fcsome00r",
         "tech")]
correlate <- psych::corr.test(tu)
correlate$r
rm(tu)

#######################################################
# Recode Control measures
#######################################################
# Make ethnicity, majority vs minority
# white = 1, other = 0
data$fd06e00 <- ifelse(data$fd06e00 == 1, 1, 0)

# Mean of educational motivation
data$fcscbe00r <- 6 - data$fcscbe00
data$fcsint00r <- 6 - data$fcsint00
data$edumot <-
  rowMeans(subset(
    data,
    select = c(
      "fcscbe00r",
      "fcsint00r",
      "fcsunh00",
      "fcstir00",
      "fcscwa00",
      "fcmnwo00"
    )
  ), na.rm = FALSE)

# Mean of closeness to parents
is.na(data[,c("fcrlqm00", "fcrlqf00")]) = data[, c("fcrlqm00", "fcrlqf00")] == 5 # make NA if do not have father/mother, therefore exclude those with no parents 
data$clpar <-
  rowMeans(subset(data, select = c(
    "fcrlqm00", "fcrlqf00", "fcquam00", "fcquaf00"
  )), na.rm = FALSE)

# Mean of sleep quality
data$fcsltr00r <- 7 - data$fcsltr00 #reverse sleep difficulties question
data$sldif <-
  rowMeans(subset(data, select = c("fcslln00", "fcsltr00r")), na.rm = FALSE)

# Recode sex
# 1 = male, 0 = female
data$fccsex00r <- 2 - data$fccsex00

#######################################################
# Recode Comparison Variables
#######################################################
# Calculate time spent sleeping on a school night
# What time do you go to sleep at night (before 9 - up to after midnight)
data$fcslwk00r <- ifelse(data$fcslwk00 == 1,
                         12 - 8.5,
                         ifelse(
                           data$fcslwk00 == 2,
                           12 - 9.5,
                           ifelse(
                             data$fcslwk00 == 3,
                             12 - 10.5,
                             ifelse(data$fcslwk00 == 4, 12 - 11.5, 12 -
                                      12.5)
                           )
                         ))

# What time do you wake up before 6 up to after 9)
data$fcwuwk00r <- ifelse(data$fcwuwk00 == 1, 5.5,
                         ifelse(data$fcwuwk00 == 2, 6.5,
                                ifelse(
                                  data$fcwuwk00 == 3, 7.5,
                                  ifelse(data$fcwuwk00 == 4, 8.5, 9.5)
                                )))
data$sleeptime <- data$fcslwk00r + data$fcwuwk00r

# Recode Handedness
# 0 - right, 1 - either, 2 - left 
data$hand <-
  ifelse(data$fchand00 == 1, 0, ifelse(data$fchand00 == 3, 1, 2))

# Reverse appropriate measures
data$fccycf00r <- 9 - data$fccycf00
data$fcglas00r <- 2 - data$fcglas00
data$fcares00r <- 2 - data$fcares00
data$fccybu00r <- 7 - data$fccybu00
data$fchurt00r <- 7 - data$fchurt00
data$fccanb00r <- 2 - data$fccanb00
data$fcalfv00r <- 2 - data$fcalfv00

####################################################################################
# Calculate number of participants
####################################################################################
table(data$fccsex00r)
table(data$fccage00)
mean(data$fccage00, na.rm = TRUE)
sd(data$fccage00, na.rm = TRUE)
nrow(data %>% filter(!is.na(felig00) & felig00 == 1))

####################################################################################
# Save as CSV for running large code on computer cluster
# remove all symbols not recognised by linux
####################################################################################
n <- names(data)
n <- gsub("mcsid", "mcsid", n)
names(data) <- n

write.csv(file = "1_3_prep_mcs_data.csv", data)
