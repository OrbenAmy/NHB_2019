##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 2.1: YRBS Make Specification Curves
##########################################################################################

# Because we use the university computer cluster we load the relevant data
# It is not necessary to run this script over the cluster as it is relatively fast (< 5 min)
vars <- Sys.getenv(c("HOME"))
data <- read.csv(file=paste0(vars["HOME"],"/1_1_prep_yrbs_data.csv"), header=TRUE, sep = ",")

#######################################################
# Load libraries
#######################################################
library(tidyr)
library(dplyr)
library(heplots)

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
    data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "combination",
      "effect",
      "number",
      "p_value",
      "standard_error",
      "rsqrd"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_cont,each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_y))
  results_frame$combination <- rep(rep(rep(rep(levels_comb,each = nrow(results_frame) / (length(levels_comb) * length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_cont)))
  return(results_frame)
  return(combinations)
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
    
    # Select Combination of wellbeing Variables
    if (results_frame$combination[n] == "Mean") {
      data_short$dv <-
        rowMeans(subset(data_short, select = results_frame$y_variable[[n]]),
                 na.rm = FALSE)
    } else {
      d1 <-
        apply((subset(data_short, select = results_frame$y_variable[[n]])), 1, sum)
      data_short$dv <- ifelse(d1 == 0, 0, ifelse(is.na(d1), NA, 1))
    }
    
    # Select technology
    data_short$iv <- subset(data_short, select = results_frame$x_variable[[n]])
    
    #################################################
    # Run Correlations
    #################################################
    
    if (results_frame$controls[n] == "No Controls") {
      reg <- lm(scale(dv) ~ scale(iv), data = data_short)
    } else if (results_frame$controls[n] == "Controls") {
      reg <-
        lm(scale(dv) ~ scale(iv) + scale(race_di),
           data = data_short)
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
    results_frame$rsqrd[n] <- etasq(reg)["scale(iv)", "Partial eta^2"]
  }
  return(results_frame)
}

####################################################################################
# Execute Specification Curve Analyses
# We include digital technology use (x_variables),
# mental well-being (y_variables),
# controls and combination methods
####################################################################################

#######################################################
# X Variables
#######################################################
x_variables <- c("q81_n", "q82_n", "tech")
x_names <-
  c("TV Use", "Electronic Device Use", "Mean Technology Use")

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
data_short <- data[, c(
  "q81_n",
  "q82_n",
  "tech",
  "q26_n",
  "q27_n",
  "q28_n",
  "q29_nd",
  "q30_nd",
  "year",
  "race_di"
)]

#######################################################
# Run and Save
#######################################################
results_yrbs_sca <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables))

save(results_yrbs_sca, file = "2_1_sca_yrbs_results.rda")
