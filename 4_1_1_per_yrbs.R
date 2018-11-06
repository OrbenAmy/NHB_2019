##########################################################################################
# Specification Curve Analysis and Social Media
# R-Script 4.1: Running bootstrapped analyses for YRBS
# 
# This is not a very helpful exercise as they are all very significant, but significance
# testing is part of the SCA procedure. Furthermore this script takes a long time to run
# We therefore run it over the computer cluster using the run script also available on the 
# OSF and the command sbatch --array=1-50 4_1_per_yrbs.run

##########################################################################################
library(tidyr)
library(dplyr)

vars <- Sys.getenv(c("HOME","SLURM_ARRAY_JOB_ID","SLURM_ARRAY_TASK_ID"))
data <- read.csv(file=paste0(vars["HOME"],"/1_1_prep_yrbs_data.csv"), header=TRUE, sep = ",")

# We run 1 bootstrap with each script and then merge them together in 4_1_2_per_yrbs.R
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
  levels_cont <- c("No Controls", "Controls")
  levels_comb <- c("Mean", "1 or more")
  
  # Calculate Number of Combinations/Analyses to run
  combinations <-
    length(levels_x) * length(levels_y) * length(levels_cont) * length(levels_comb)
  
  # Setup results frame
  results_frame <-
    data.frame(matrix(NA, nrow = combinations, ncol = 8))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "combination",
      "effect",
      "number",
      "p_value",
      "standard_error"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_cont,each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_y))
  results_frame$combination <- rep(rep(rep(rep(levels_comb,each = nrow(results_frame) / (length(levels_comb) * length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_cont)))
  return(results_frame)
  return(combinations)
}

##########################################################################################
# Define measures
##########################################################################################

#######################################################
# X Variables
#######################################################
x_variables <- c("q81_n", "q82_n", "tech")
x_names <- c("TV Use", "Electronic Device Use", "tech")

#######################################################
# Y Variables
#######################################################
y <-  c("q26_n", "q27_n", "q28_n", "q29_nd", "q30_nd")
y_variables <-
  (do.call("c", lapply(seq_along(y), function(i)
    combn(y, i, FUN = list))))
y_names <-
  c("loneliness",
    "think suicide",
    "plan suicide",
    "commit suicide",
    "doctor suicide")

#######################################################
# Combination Method
#######################################################
c_variables <- c("Means", "Choose 1")
c_names <- 2 # 1 = means, 2 = choose 1

#######################################################
# Controls
#######################################################
controls <- c("race_di")
s_names <- c("dichotomous race")

#######################################################
# Subset Data
#######################################################
data_select <- data[, c(
  "q81_n",
  "q82_n",
  "q26_n",
  "q27_n",
  "q28_n",
  "q29_nd",
  "q30_nd",
  "year",
  "race_di",
  "tech"
)]
data_select$id <- seq.int(nrow(data_select))

#######################################################
# Make results frame
#######################################################
results_frame <-
  resultsframe(x_var = x_variables, y_var = y_variables)

##########################################################################################
# force null onto data for each specification
# we run this before sending this script to the computer cluster and save the result
##########################################################################################
  y.null <- list(0)
 
  for (i in 1:nrow(results_frame)){
 
    print(i)
    # Select dependent variable (wellbeing)
    if (results_frame$combination[i] == "Mean") {
      data$dv <- rowMeans(subset(data, select = results_frame$y_variable[[i]]),
                 na.rm = FALSE)
    } else {
      data$dv <-apply((subset(data, select = results_frame$y_variable[[i]])), 1, sum)
      data$dv <- ifelse(data$dv == 0, 0, ifelse(is.na(data$dv), NA, 1))
    }
    data$dv <- as.matrix(data$dv)
 
    # Select independent variable (technology)
    data$iv <- as.matrix(subset(data, select = results_frame$x_variable[[i]]))
 
    ##################################################   
    # Run Original Regressions
    #################################################
    if (results_frame$controls[i] == "No Controls") {
      reg <- lm(dv ~ iv, data = data)
    } else if (results_frame$controls[i] == "Controls") {
      reg <-
        lm(dv ~ iv + race_di,
           data = data)
    }
 
    b.i <- summary(reg)$coef[[2, 1]] %>% {ifelse(. == 0, NA, .)}
 
    y.null.i <- data$dv-(b.i*data$iv)
    y.null[i] <- as.data.frame(y.null.i)
  }
 
 save(y.null, file = "4_1_y_null_yrbs.RData")

load("4_1_y_null_yrbs.RData")

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

for (m in 1:(bootstraps)) {
#  gc()
  print(m)

  for (n in 1:nrow(results_frame)) {
    
    print(paste0(m,".",n))
    
    #################################################
    # Make variables
    #################################################
    # Select dependent variable (wellbeing)
    data$dv <- y.null[[n]]
    
    # Select independebt variable (technology)
    data$iv <- as.matrix(subset(data, select = results_frame$x_variable[[n]]))
    
    # Select variables
    k=sample(nrow(data),replace=T) 

    #################################################
    # Run Regressions
    #################################################
    if (results_frame$controls[n] == "No Controls") {
      reg <- lm(dv ~ iv, data = data[k,])
    } else if (results_frame$controls[n] == "Controls") {
      reg <-
        lm(dv ~ iv + race_di,
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
write.csv(permutation_frame,file=paste0(vars["HOME"],"/yrbs_permutation_frame.",vars["SLURM_ARRAY_JOB_ID"],".",vars["SLURM_ARRAY_TASK_ID"], ".csv"))