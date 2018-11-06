##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 5.3: Run MCS specification curve analysis of comparison variables
##########################################################################################
library(tidyr)
library(dplyr)
library(heplots)

vars <- Sys.getenv(c("HOME"))
data <- read.csv(file=paste0(vars["HOME"],"/1_3_prep_mcs_data.csv"), header=TRUE, sep = ",")

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
  combinations <-
    length(levels_x) * length(levels_y) * length(levels_c)
  
  # Setup results frame
  results_frame <- data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "effect",
      "t_value",
      "p_value",
      "standard_error",
      "number",
      "rsqrd"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_c, each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_c))), times = length(levels_x)), times = length(levels_y))
  return(results_frame)
}


####################################################################################
# Function "curve"
# takes results frame as an input
# returns results frame including the specification curve analysis results
####################################################################################
curve <- function(input) {
  results_frame <- input
  
  for (n in 1:nrow(results_frame)) {
    print(n/nrow(results_frame))
    #################################################
    # Make variables
    #################################################
    
    data_short$dv <-
      rowMeans(subset(data_short, select = results_frame$y_variable[[n]]),
               na.rm = FALSE)
    data_short$iv <-
      subset(data_short, select = results_frame$x_variable[[n]])
    
    #################################################
    # Run Correlations
    #################################################
    
    if (results_frame$controls[n] == "No Controls") {
      reg <- lm(scale(dv) ~ scale(iv), data = data_short)
    } else if (results_frame$controls[n] == "Controls") {
      reg <- lm(
        scale(dv) ~ scale(iv) + scale(edumot) +
          scale(fd06e00) + scale(clpar) + scale(fcpaab00) + 
          scale(fpwrdscm) + scale(fdacaq00) + scale(fd05s00) + 
          scale(fpclsi00) + scale(fpchti00) + scale(fdkessl) + scale(fdtots00) +
          scale(foede000),
        data = data_short
      )
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
    results_frame$p_value[n] <- summary(reg)$coef[[2, 4]]
    results_frame$standard_error[n] <-
      summary(reg)$coef[[2, 2]] %>% {
        ifelse(. == 0, NA, .)
      }
    results_frame$number[n] <- nobs(reg)
    results_frame$rsqrd[n] <- etasq(reg)["scale(iv)", "Partial eta^2"]
  }
  return(results_frame)
}

####################################################################################
# Comparison Specifications for Cohort Members
####################################################################################

#######################################################
# X Variables 
#######################################################
x_variables <- c(
  "fchtcm00",
  "fccycf00r",
  "fcbrkn00",
  "fcfrut00",
  "fcvegi00",
  "sleeptime",
  "fcglas00r",
  "fcares00r",
  "fchurt00r",
  "fccanb00r",
  "fcalfv00r",
  "hand",
  "tech"
)
x_names <- c(
  "Height",
  "Bicycle Use",
  "Breakfast",
  "Fruit",
  "Vegetabes",
  "Sleep",
  "Glasses",
  "Been Arrested",
  "Bullying",
  "Smoked Weed",
  "Bingedrinking",
  "Handedness",
  "Technology Use"
)


#######################################################
# Y Variables
# There are too many y_variable combinations 
# we therefore aim to run about 100,000 tests and
# therefore want to choose about 800 y_variable
# specifications
#######################################################
load("2_3_sca_mcs_y_sample_cm.rda")

#######################################################
# Control Variables
#######################################################
controls <-
  c(
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "sldif",
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
data_short <-
  data[, c(
    "fchtcm00",
    "fccycf00r",
    "fcbrkn00",
    "fcfrut00",
    "fcvegi00",
    "sleeptime",
    "fcglas00r",
    "fcares00r",
    "fchurt00r",
    "fccanb00r",
    "fcalfv00r",
    "hand",
    "tech",
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
    "fccanb00r",
    "fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r",
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "hand",
    "sleeptime",
    "fdacaq00",
    "fd05s00",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000"
  )]

#######################################################
# Run and Save
#######################################################
results_mcs_ds_cm_comp <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_cm))
save(results_mcs_ds_cm_comp, file = "5_3_comp_mcs_results_cm.rda")

#######################################################
# X Variables: Same as previously
#######################################################

#######################################################
# Y Variables
# Similar procedure as above
# We choose less randomn combinations as there 
# are more measures and we want equal number of 
# specifications for cohort members and parents
#######################################################
load("2_3_sca_mcs_y_sample_pr.rda")

#######################################################
# Subset Data
#######################################################
data_short <-
  data[, c(
    "fchtcm00",
    "fccycf00r",
    "fcbrkn00",
    "fcfrut00",
    "fcvegi00",
    "sleeptime",
    "fcglas00r",
    "fcares00r",
    "fchurt00r",
    "fccanb00r",
    "fcalfv00r",
    "hand",
    "tech",
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
    "hand",
    "sleeptime",
    "fdacaq00",
    "fd05s00",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000"
  )]

#######################################################
# Run and Save
#######################################################
results_mcs_ds_pr_comp <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_pr))
save(results_mcs_ds_pr_comp, file = "5_3_comp_mcs_results_pr.rda")

#################################################
# Bind Both
#################################################
results_mcs_ds_cm_comp$respondent <-
  rep("Cohort Member", nrow(results_mcs_ds_cm_comp))
results_mcs_ds_pr_comp$respondent <-
  rep("Parent", nrow(results_mcs_ds_pr_comp))

results_mcs_ds_comp <-
  rbind(results_mcs_ds_cm_comp, results_mcs_ds_pr_comp)
save(results_mcs_ds_comp, file = "5_3_comp_mcs_results.rda")
