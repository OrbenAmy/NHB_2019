##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 5.2: Run MTF specification curve analysis of comparison variables
##########################################################################################
library(tidyr)
library(dplyr)
library(heplots)

vars <- Sys.getenv(c("HOME","SLURM_ARRAY_JOB_ID","SLURM_ARRAY_TASK_ID"))
data <- read.csv(file=paste0(vars["HOME"],"/1_2_prep_mtf_data.csv"), header=TRUE, sep = ",")

####################################################################################
# Function "resultsframe"
# takes character vector of outcome variables as an input
# returns the results vector of all specifications
####################################################################################

# This function sets up a frame for the results to be written in
resultsframe <- function(x_var, y_var) {
  # Setup Specification Names
  levels_x <- x_var
  levels_y <- y_var
  levels_c <- c("No Controls", "Controls")
  # Calculate Number of Combinations/Analyses to run
  combinations <- length(levels_x) * length(levels_y) * length(levels_c)
  # Setup results frame
  results_frame <- data.frame(matrix(NA, nrow=combinations, ncol=9))
  colnames(results_frame) <- c("x_variable", "y_variable", "controls", "effect", "t_value", "p_value", "standard_error", "number", "rsqrd")
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each=nrow(results_frame)/length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each=nrow(results_frame)/(length(levels_x)*length(levels_y))), times=length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_c, each=nrow(results_frame)/(length(levels_x)*length(levels_y)*length(levels_c))), times=length(levels_x)), times=length(levels_y))
  return(results_frame)
}

####################################################################################
# Function "curve"
# takes results frame as an input
# returns results frame including the specification curve analysis results
####################################################################################

comp_curve <- function(input) {
  results_frame <- input
  
  for (n in 1:nrow(results_frame)) {
    #################################################
    # Make variables
    #################################################
    
    data_short$dv <-
      rowMeans(select(data_short, select = results_frame$y_variable[[n]]),
               na.rm = FALSE)
    data_short$iv <-
      rowMeans(select(data_short, select = results_frame$x_variable[[n]]),
               na.rm = FALSE)
    
    #################################################
    # Run Correlations
    #################################################
    
    if (results_frame$controls[n] == "No Controls") {
      reg <- lm(scale(dv) ~ scale(iv), data = data_short)
    } else if (results_frame$controls[n] == "Controls") {
      reg <- lm(
        scale(dv) ~ scale(iv) + scale(v1070r) +
          scale(v7208) + scale(v7216) + scale(v7217) +
          scale(v7329) + scale(v7221) + scale(v7254),
        data = data_short
      )
    }
    
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
    print(n)
  }
  return(results_frame)
}

##########################################################################################
# Define measures
##########################################################################################

#######################################################
# X Variables
#######################################################
x_variables <- c(
  "v8526",
  "v8530",
  "v7251",
  "v8527",
  "v7588",
  "v7219",
  "v7309",
  "v7327",
  "v7109",
  "v7101",
  "v7113",
  "v8516",
  "tech"
)
x_names <-
  c("breakfast",
    "sleep",
    "eat fruit",
    "eat vegetables",
    "listen music",
    "religious service",
    "go to movies",
    "homework time",
    "# drink alc",
    "cigarettes",
    "# marijuana",
    "fight",
    "technology"
  )

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
controls <-
  c("v1070r", "v7208", "v7216", "v7217", "v7329", "v7221", "v7254")
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
data_short <- data[, c(
  "v8526",
  "v8530",
  "v7251",
  "v8527",
  "v7588",
  "v7219",
  "v7309",
  "v7327",
  "v7522",
  "v7214",
  "v7109",
  "v7113",
  "v8516",
  "tech",
  "v7101",
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
  "v7544",
  "v7551",
  "v7553",
  "v7589",
  "v7590",
  "v7562",
  "v7563",
  "v1",
  "v1070r",
  "v7208",
  "v7216",
  "v7217",
  "v7329",
  "v7221",
  "v7254"
)]

##########################################################################################
# Run Permutations
##########################################################################################
results_mtf_ds_comp <-
  comp_curve(resultsframe(x_var = x_variables, y_var = y_variables))
save(results_mtf_ds_comp, file = "5_2_comp_mtf_results.rda")
