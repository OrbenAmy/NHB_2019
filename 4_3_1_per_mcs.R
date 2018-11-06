#!/usr/bin/env Rscript
##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 4.3.1: Making SCA MCS permutations
# This is not very helpful as they are all very significant. And it takes a very very
# long long time. We run this over the ARC computer cluster
##########################################################################################
library(tidyr)
library(dplyr)

vars <- Sys.getenv(c("HOME","SLURM_ARRAY_JOB_ID","SLURM_ARRAY_TASK_ID"))
data <- read.csv(file="1_3_prep_mcs_data.csv", header=TRUE, sep = ",")
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
x_variables <- c("fctvho00", "fccomh00", "fccmex00", "fcinth00", "fcsome00", "tech")
x_names <-
  c(
    "Weekday TV",
    "Weekday Electronic Games",
    "Own Computer",
    "Use Internet at Home",
    "Hours of Social Media Use",
    "tech"
  )

#######################################################
# Y Variables
#######################################################
load("2_3_sca_mcs_y_sample_cm.rda")
load("2_3_sca_mcs_y_sample_pr.rda")

#######################################################
# Controls
#######################################################
controls <-
  c(
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "fdacaq00",
    "fd05s00",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000"
  )

#######################################################
# Subset Data
#######################################################
data_select <-
  data[, c(
    "fctvho00",
    "fccomh00",
    "fccmex00",
    "fcinth00",
    "fcsome00",
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
    "fclife00r",
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
    "fpeer",
    "fprosoc",
    "febdtot",
    "femotion",
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "fdacaq00",
    "fd05s00",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000",
    "tech"
  )]
data_select$id <- seq.int(nrow(data_select))

##########################################################################################
# Run Permutations
##########################################################################################

#######################################################
# Make results frame
#######################################################
results_frame_cm <-
  resultsframe(x_var = x_variables, y_var = y_variables_sample_cm)
results_frame_pr <-
  resultsframe(x_var = x_variables, y_var = y_variables_sample_pr)

##########################################################################################
# force null onto data from each specification
##########################################################################################
   #  y.null.cm <- list(0)
   #  y.null.pr <- list(0)
   # 
   #  for (i in 1:nrow(results_frame_cm)){
   # 
   #    print(i/nrow(results_frame_cm))
   # 
   #    #################################################
   #    # Make variables
   #    #################################################
   #    
   #    data$dv <-
   #      rowMeans(subset(data, select = results_frame_cm$y_variable[[i]]),
   #               na.rm = FALSE)
   #    data$iv <-
   #      rowMeans(subset(data, select = results_frame_cm$x_variable[[i]]),
   #               na.rm = FALSE)
   #    #################################################
   #    # Run Correlations
   #    #################################################
   #    
   #    if (results_frame_cm$controls[i] == "No Controls") {
   #      reg <- lm(dv ~ iv, data = data)
   #    } else if (results_frame_cm$controls[i] == "Controls") {
   #      reg <- lm(dv ~ iv + edumot + fd06e00 + clpar + fcpaab00 + 
   #          fpwrdscm + fdacaq00 + fd05s00 + 
   #          fpclsi00 + fpchti00 + fdkessl + fdtots00 + foede000,
   #        data = data
   #      )
   #    }
   # 
   #    b.i <- summary(reg)$coef[[2, 1]] %>% {ifelse(. == 0, NA, .)}
   # 
   #    y.null.i <- data$dv-(b.i*data$iv)
   #    y.null.cm[i] <- as.data.frame(y.null.i)
   #  }
   # 
   # save(y.null.cm, file = "4_3_y_null_mcs_cm.RData")
   # 
   # for (i in 1:nrow(results_frame_pr)){
   # 
   #   print(i/nrow(results_frame_pr))
   # 
   #   #################################################
   #   # Make variables
   #   #################################################
   # 
   #   data$dv <-
   #     rowMeans(subset(data, select = results_frame_pr$y_variable[[i]]),
   #              na.rm = FALSE)
   #   data$iv <-
   #     rowMeans(subset(data, select = results_frame_pr$x_variable[[i]]),
   #              na.rm = FALSE)
   #   #################################################
   #   # Run Correlations
   #   #################################################
   # 
   #   if (results_frame_pr$controls[i] == "No Controls") {
   #     reg <- lm(dv ~ iv, data = data)
   #   } else if (results_frame_pr$controls[i] == "Controls") {
   #     reg <- lm(dv ~ iv + edumot + fd06e00 + clpar + fcpaab00 +
   #                 fpwrdscm + fdacaq00 + fd05s00 +
   #                 fpclsi00 + fpchti00 + fdkessl + fdtots00 + foede000,
   #               data = data
   #     )
   #   }
   # 
   #   b.i <- summary(reg)$coef[[2, 1]] %>% {ifelse(. == 0, NA, .)}
   # 
   #   y.null.i <- data$dv-(b.i*data$iv)
   #   y.null.pr[i] <- as.data.frame(y.null.i)
   # }
   # 
   # save(y.null.pr, file = "4_3_y_null_mcs_pr.RData")
 
 load("4_3_y_null_mcs_cm.RData")
 load("4_3_y_null_mcs_pr.RData")
 
 ##########################################################################################
 # Run Bootstraps (cm)
 ##########################################################################################
 
 #######################################################
 # Make Permutation Frame
 #######################################################
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
   #  gc()
   print(m)
   
   for (n in 1:nrow(results_frame_cm)) {
     
     print(paste0(m,".",n))
     
     #################################################
     # Make variables
     #################################################
     # Select dependent variable (wellbeing)
     data$dv <- y.null.cm[[n]]
     
     # Select independebt variable (technology)
     data$iv <- rowMeans(subset(data, select = results_frame_cm$x_variable[[n]]), na.rm = FALSE)
     
     # Select variables
     k=sample(nrow(data),replace=T) 
     
     #################################################
     # Run Regressions
     #################################################
     if (results_frame_cm$controls[n] == "No Controls") {
       reg <- lm(dv ~ iv, data = data[k,])
     } else if (results_frame_cm$controls[n] == "Controls") {
       reg <- lm(dv ~ iv + edumot + fd06e00 + clpar + fcpaab00 + 
                   fpwrdscm + fdacaq00 + fd05s00 + 
                   fpclsi00 + fpchti00 + fdkessl + fdtots00 + foede000,
            data = data[k,])
     }
     
     #################################################
     # Extract Variables
     #################################################
     
     results_frame_cm$t_value[n] <-
       summary(reg)$coef[[2, 3]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame_cm$effect[n] <-
       summary(reg)$coef[[2, 1]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame_cm$number[n] <- nobs(reg)
     results_frame_cm$p_value[n] <- summary(reg)$coef[[2, 4]]
     results_frame_cm$standard_error[n] <-
       summary(reg)$coef[[2, 2]] %>% {
         ifelse(. == 0, NA, .)
       }
   }
   
   for (n in 1:nrow(results_frame_pr)) {
     
     print(paste0(m,".",n))
     
     #################################################
     # Make variables
     #################################################
     # Select dependent variable (wellbeing)
     data$dv <- y.null.pr[[n]]
     
     # Select independebt variable (technology)
     data$iv <- rowMeans(subset(data, select = results_frame_pr$x_variable[[n]]), na.rm = FALSE)
     
     # Select variables
     k=sample(nrow(data),replace=T) 
     
     #################################################
     # Run Regressions
     #################################################
     if (results_frame_pr$controls[n] == "No Controls") {
       reg <- lm(dv ~ iv, data = data[k,])
     } else if (results_frame_pr$controls[n] == "Controls") {
       reg <- lm(dv ~ iv + edumot + fd06e00 + clpar + fcpaab00 + 
                   fpwrdscm + fdacaq00 + fd05s00 + 
                   fpclsi00 + fpchti00 + fdkessl + fdtots00 + foede000,
                 data = data[k,])
     }
     
     #################################################
     # Extract Variables
     #################################################
     
     results_frame_pr$t_value[n] <-
       summary(reg)$coef[[2, 3]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame_pr$effect[n] <-
       summary(reg)$coef[[2, 1]] %>% {
         ifelse(. == 0, NA, .)
       }
     results_frame_pr$number[n] <- nobs(reg)
     results_frame_pr$p_value[n] <- summary(reg)$coef[[2, 4]]
     results_frame_pr$standard_error[n] <-
       summary(reg)$coef[[2, 2]] %>% {
         ifelse(. == 0, NA, .)
       }
   }
   
   results_frame <- rbind(results_frame_cm, results_frame_pr)
   
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
write.csv(permutation_frame,file=paste0(vars["HOME"],"/mcs_permutation_frame.",vars["SLURM_ARRAY_JOB_ID"],".",vars["SLURM_ARRAY_TASK_ID"], ".csv"))