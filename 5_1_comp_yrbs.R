##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 5.1: Run YRBS specification curve analysis of comparison variables
##########################################################################################
library(tidyr)
library(dplyr)

vars <- Sys.getenv(c("HOME"))
data <- read.csv(file=paste0(vars["HOME"],"/1_1_prep_yrbs_data.csv"), header=TRUE, sep = ",")

### make all data numeric
data_numeric <-
  data.frame(lapply(data, function(x)
    as.numeric(as.character(x))))

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
    data.frame(matrix(NA, nrow = combinations, ncol = 10))
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
      "number",
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
comp_curve <- function(input) {
  results_frame <- input
  
  for (n in 1:nrow(results_frame)) {
    
    print(n)
    #################################################
    # Make variables
    #################################################
    
    # combination
    if (results_frame$combination[n] == "Mean") {
      data_short$dv <-
        rowMeans(subset(data_short, select = results_frame$y_variable[[n]]),
                 na.rm = FALSE)
    } else {
      d1 <-
        apply((subset(data_short, select = results_frame$y_variable[[n]])), 1, sum)
      data_short$dv <- ifelse(d1 == 0, 0, ifelse(is.na(d1), NA, 1))
    }
    
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


##########################################################################################
# Define measures
##########################################################################################

#######################################################
# X Variables
#######################################################
#recode asthma so that 1 = no asthma, 2 not sure, 3 = asthma
data$q87r <- ifelse(data$q87 == 1, 3, ifelse(data$q87 == 2, 1, 2))

x_variables <- c(
  "q72",
  "q79",
  "q76",
  "q88",
  "q74",
  "q78",
  "q69",
  "q87r",
  "q47",
  "q44",
  "q24_n",
  "q18",
  "tech"
)
x_names <-
  c(
    "Eat fruit",
    "Eat breakfast",
    "Eat vegetables",
    "hours sleep",
    "Eat potatoes",
    "Drink Milk",
    "Perceived Weight",
    "Asthma",
    "Marijuana",
    "Bingedrinking",
    "Bullied",
    "Fight",
    "technology use"
  )

#######################################################
# Y Variables
#######################################################
y <- c("q26_n", "q27_n", "q28_n", "q29_nd", "q30_nd")
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
  "q72",
  "q79",
  "q76",
  "q88",
  "q80",
  "q74",
  "q78",
  "q69",
  "q87r",
  "q47",
  "q44",
  "q24_n",
  "q18",
  "tech",
  "q26_n",
  "q27_n",
  "q28_n",
  "q29_nd",
  "q30_nd",
  "year",
  "race_di"
)]

##########################################################################################
# Run Permutations
##########################################################################################
results_yrbs_sro_mom <-
  comp_curve(resultsframe(x_var = x_variables, y_var = y_variables))
save(results_yrbs_sro_mom, file = "5_1_comp_yrbs_results.rda")