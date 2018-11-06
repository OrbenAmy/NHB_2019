#!/usr/bin/env Rscript
##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 4.2.1: Making SCA MTF permutations
# 
# This is not a very helpful exercise as they are all very significant, but significance
# testing is part of the SCA procedure. Furthermore this script takes a long time to run
# We therefore run it over the computer cluster using the run script also available on the 
# OSF and the command sbatch --array=1-50 4_2_per_mtf.run

##########################################################################################
library(tidyr)
library(dplyr)

vars <- Sys.getenv(c("HOME","SLURM_ARRAY_JOB_ID","SLURM_ARRAY_TASK_ID"))
data <- read.csv(file=paste0(vars["HOME"],"/1_2_prep_mtf_data.csv"), header=TRUE, sep = ",")

# We run 2 bootstraps with each script and then merge them together in 4_2_2_per_mtf.R
bootstraps <- 1

####################################################################################
# Function "resultsframe"
# takes character vector of outcome variables as an input
# returns the results vector of all specifications
####################################################################################
resultsframe <- function(x_var, y_var) {
  
  # Setup Specification Names
  levels_x <- x_var
  levels_y <- y_var
  levels_c <- c("No Controls", "Controls")
  
  # Calculate Number of Combinations/Analyses to run
  combinations <- length(levels_x) * length(levels_y) * length(levels_c)
  
  # Setup results frame
  results_frame <- data.frame(matrix(NA, nrow=combinations, ncol=7))
  colnames(results_frame) <- c("x_variable", "y_variable", "controls", "effect", "t_value", "p_value", "standard_error")
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each=nrow(results_frame)/length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each=nrow(results_frame)/(length(levels_x)*length(levels_y))), times=length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_c, each=nrow(results_frame)/(length(levels_x)*length(levels_y)*length(levels_c))), times=length(levels_x)), times=length(levels_y))
  
  return(results_frame)
}

##########################################################################################
# Define measures
##########################################################################################

#######################################################
# X Variables
#######################################################
x_variables <- list("v7326", "v7325", "v7381", "v7552", "tech")
x_names <-
  c("TV Weekday", "TV Weekend", "Internet for News", "Social Media Use", "tech")

#######################################################
# Y Variables
#######################################################
y <-
  c(
    "v7302",
    "v8502rev",
    "v8505",
    "v8509rev",
    "v8514",
    "v8504",
    "v8501",
    "v8508",
    "v8512",
    "v8503rev",
    "v8511rev",
    "v8513rev"
  )
y_variables <-
  (do.call("c", lapply(seq_along(y), function(i)
    combn(y, i, FUN = list))))
y_names <-
  c(
    "Happy these days",
    "Life is meaningless*",
    "Enjoy life",
    "Future is hopeless*",
    "Good to be alive",
    "Person of worth",
    "Positive attitude towards self",
    "Do well as most others",
    "Satisfied with myself",
    "Not much to be proud of*",
    "Can't do anything right*",
    "Life not useful*"
  )

#######################################################
# Controls
#######################################################
c_variables <-
  c("v1070r",
    "v7208",
    "v7216",
    "v7217",
    "v7329",
    "v7221",
    "v7254")

c_names <-
  c(
    "race",
    "brothers and sisters",
    "mother edu",
    "mothers job",
    "enjoy school",
    "predicted grades",
    "talk with parents"
  )

#######################################################
# Subset Data
#######################################################
data$id <- (seq.int(nrow(data)))
data_short <- data[, c(
  "v7326",
  "v7325",
  "v7381",
  "v7552",
  "v7302",
  "v8502rev",
  "v8505",
  "v8509rev",
  "v8514",
  "v8504",
  "v8501",
  "v8508",
  "v8512",
  "v8503rev",
  "v8511rev",
  "v8513rev",
  "v1",
  "id",
  "v1070r",
  "v7208",
  "v7216",
  "v7217",
  "v7329",
  "v7221",
  "v7254",
  "tech"
)]


#######################################################
# Make results frame
#######################################################
results_frame <-
  resultsframe(x_var = x_variables, y_var = y_variables)

##########################################################################################
# force null onto data from each specification
# we run this before sending this script to the computer cluster and save the result
##########################################################################################
 #  y.null <- list(0)
 # 
 #  for (i in 1:nrow(results_frame)){
 # 
 #    print(i/nrow(results_frame))
 # 
 #    #################################################
 #    # Make variables
 #    #################################################
 #    
 #    data$dv <-
 #      rowMeans(subset(data, select = results_frame$y_variable[[i]]),
 #               na.rm = FALSE)
 #    data$iv <-
 #      rowMeans(subset(data, select = results_frame$x_variable[[i]]),
 #               na.rm = FALSE)
 #    
 #    #################################################
 #    # Run Correlations
 #    #################################################
 #    
 #    if (results_frame$controls[i] == "No Controls") {
 #      reg <- lm(dv ~ iv, data = data)
 #    } else if (results_frame$controls[i] == "Controls") {
 #      reg <- lm(dv ~ iv + v1070r + v7208 + v7216 + v7217 +
 #                  v7329 + v7221 + v7254,
 #        data = data
 #      )
 #    }
 # 
 #    b.i <- summary(reg)$coef[[2, 1]] %>% {ifelse(. == 0, NA, .)}
 # 
 #    y.null.i <- data$dv-(b.i*data$iv)
 #    y.null[i] <- as.data.frame(y.null.i)
 #  }
 # 
 # save(y.null, file = "4_2_y_null_mtf.RData")
 
 load("4_2_y_null_mtf.RData")
 
 ##########################################################################################
 # Run Bootstraps 
 ##########################################################################################
 
 permutation_frame <-
   data.frame(matrix(NA, nrow = bootstraps, ncol = 16))
 names(permutation_frame) <-
   c(
     "permutation_number",
     "effect.boot",
     "sign.neg.boot",
     "sign.pos.boot",
     "sign.sig.neg.boot",
     "sign.sig.pos.boot",
     "effect.boot.c",
     "sign.neg.boot.c",
     "sign.pos.boot.c",
     "sign.sig.neg.boot.c",
     "sign.sig.pos.boot.c", 
     "effect.boot.nc",
     "sign.neg.boot.nc",
     "sign.pos.boot.nc",
     "sign.sig.neg.boot.nc",
     "sign.sig.pos.boot.nc"
   )
 permutation_frame$permutation_number <- seq.int(bootstraps)
 
 #######################################################
 # Run
 #######################################################
 for (m in 1:(bootstraps)) {

   print(m)
   
   for (n in 1:nrow(results_frame)) {
     
     print(paste0(m,".",n))
     
     #################################################
     # Make variables
     #################################################
     # Select dependent variable (wellbeing)
     data$dv <- y.null[[n]]
     
     # Select independebt variable (technology)
     data$iv <- rowMeans(subset(data, select = results_frame$x_variable[[n]]), na.rm = FALSE)
     
     # Select variables
     k=sample(nrow(data),replace=T) 
     
     #################################################
     # Run Regressions
     #################################################
     if (results_frame$controls[n] == "No Controls") {
       reg <- lm(dv ~ iv, data = data[k,])
     } else if (results_frame$controls[n] == "Controls") {
       reg <- lm(dv ~ iv + v1070r + v7208 + v7216 + v7217 +
                   v7329 + v7221 + v7254,
                 data = data[k,])
     }
     
     #################################################
     # Extract Variables
     #################################################
     
     results_frame$t_value[n] <-
       summary(reg)$coef[[2, 3]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame$effect[n] <-
       summary(reg)$coef[[2, 1]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame$number[n] <- nobs(reg)
     results_frame$p_value[n] <- summary(reg)$coef[[2, 4]]
     results_frame$standard_error[n] <-
       summary(reg)$coef[[2, 2]] %>% {
         ifelse(. == 0, NA, .)
       }
   }
   
   permutation_frame[m, 2] <-
     mean(results_frame[["effect"]], na.rm = TRUE)
   permutation_frame[m, 3] <-
     table(sign(results_frame[["effect"]]))[["-1"]]
   permutation_frame[m, 4] <-
     table(sign(results_frame[["effect"]]))[["1"]]
   results_frame_sig <- results_frame %>% filter(p_value < 0.05)
   permutation_frame[m, 5] <-
     table(sign(results_frame_sig[["effect"]]))[["-1"]]
   permutation_frame[m, 6] <-
     table(sign(results_frame_sig[["effect"]]))[["1"]]
   
   results_frame_nc <-
     results_frame %>% filter(controls == "No Controls")
   permutation_frame[m, 7] <-
     mean(results_frame_nc[["effect"]], na.rm = TRUE)
   permutation_frame[m, 8] <-
     table(sign(results_frame_nc[["effect"]]))[["-1"]]
   permutation_frame[m, 9] <-
     table(sign(results_frame_nc[["effect"]]))[["1"]]
   results_frame_sig <- results_frame_nc %>% filter(p_value < 0.05)
   permutation_frame[m, 10] <-
     table(sign(results_frame_sig[["effect"]]))[["-1"]]
   permutation_frame[m, 11] <-
     table(sign(results_frame_sig[["effect"]]))[["1"]]
   
   results_frame_c <-
     results_frame %>% filter(controls == "Controls")
   permutation_frame[m, 12] <-
     mean(results_frame_c[["effect"]], na.rm = TRUE)
   permutation_frame[m, 13] <-
     table(sign(results_frame_c[["effect"]]))[["-1"]]
   permutation_frame[m, 14] <-
     table(sign(results_frame_c[["effect"]]))[["1"]]
   results_frame_sig <- results_frame_c %>% filter(p_value < 0.05)
   permutation_frame[m, 15] <-
     table(sign(results_frame_sig[["effect"]]))[["-1"]]
   permutation_frame[m, 16] <-
     table(sign(results_frame_sig[["effect"]]))[["1"]]
   
 }
 
 #######################################################
 # Print and Save Permutations
 #######################################################
 print(permutation_frame)
 write.csv(permutation_frame,file=paste0(vars["HOME"],"/mtf_permutation_frame.",vars["SLURM_ARRAY_JOB_ID"],".",vars["SLURM_ARRAY_TASK_ID"], ".csv"))