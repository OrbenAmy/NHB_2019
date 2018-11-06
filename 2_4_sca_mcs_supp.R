##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 2.4: MCS Make Specification Curves for Supplementary Materials
##########################################################################################
data <- read.csv(file="/1_3_prep_mcs_data.csv", header=TRUE, sep = ",")
set.seed(111)

#######################################################
# Load libraries
#######################################################
library(tidyr)
library(dplyr)
library("heplots")

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
# Execute Specification Curve Analyses for COHORT MEMBERS
# We include digital technology use (x_variables),
# mental well-being (y_variables) and controls
####################################################################################

#######################################################
# X Variables 
#######################################################
x_variables <-
  c("fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "tech")

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
y1 <-
  c(
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
    "fcmdsm00r"
  )
y2 <-
  c("fcsati00r",
    "fcgdql00r",
    "fcdowl00r",
    "fcvalu00r",
    "fcgdsf00r")
y3 <-
  c("fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r")

#### Bind all of these specifcations together
y_variables_sample_cm <- c(list(y1, y2, y3))

#######################################################
# Control Variables
#######################################################
controls <-
  c(
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fcwrdsc",
    "fdacaq00",
    "fd05s00",
    "fpwrdscm",
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
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
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
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fcwrdsc",
    "fdacaq00",
    "fd05s00",
    "fpwrdscm",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000",
    "tech"
  )]

#######################################################
# Run and Save
#######################################################
results_mcs_sca_cm <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_cm))
save(results_mcs_sca_cm, file = "2_4_sca_mcs_supp_results_cm.rda")

####################################################################################
# Execute Specification Curve Analyses for PARENTS
# We include digital technology use (x_variables),
# mental well-being (y_variables) and controls
####################################################################################

#######################################################
# X Variables: Same as previously
#######################################################

#######################################################
# Y Variables
#######################################################
y1 <- c("fconduct")
y2 <- c("fhyper")
y3 <- c("fpeer")
y4 <- c("fprosoc")
y5 <- c("febdtot") # total
y6 <- c("femotion")
y7 <- c("femotion", "fpeer")
y8 <- c("fconduct", "fhyper")

# bind data
y_variables_sample_pr <- c(list(y2,y3,y4,y5,y6,y7,y8))

# define data
data_short <-
  data[, c(
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
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
    "fcwrdsc",
    "fdacaq00",
    "fd05s00",
    "fpwrdscm",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000"
  )]

# run and save
results_mcs_sca_pr <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_pr))
save(results_mcs_sca_pr, file = "2_4_sca_mcs_supp_results_pr.rda")

#################################################
# Bind Both
#################################################
results_mcs_sca_cm$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm))
results_mcs_sca_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_pr))

results_mcs_sca_total <- rbind(results_mcs_sca_cm, results_mcs_sca_pr)
save(results_mcs_sca_total, file = "2_4_sca_mcs_supp_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_mcs_sca_total)

####################################################################################
# Median effects
# total, separate x variables, controls/no controls
####################################################################################
results_mcs_sca_total %>% summarise(median = median(effect, na.rm = TRUE))

results_mcs_sca_total %>% filter(respondent == "Parent") %>% summarise(median = median(effect, na.rm = TRUE))
results_mcs_sca_total %>% filter(respondent == "Cohort Member") %>% summarise(median = median(effect, na.rm = TRUE))

results_mcs_sca_total %>% filter(controls == "Controls") %>% summarise(median = median(effect, na.rm = TRUE))
results_mcs_sca_total %>% filter(controls == "No Controls") %>% summarise(median = median(effect, na.rm = TRUE))

