##########################################################################################
#Specification Curve Analysis and Digital Technology Use
# R-Script 2.2: MTF Make Specification Curves
##########################################################################################

# Because we use the university computer cluster we load the relevant data
data <- read.csv(file="1_2_prep_mtf_data.csv", header=TRUE, sep = ",")

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
      reg <- tryCatch(
        lm(
          scale(dv) ~ scale(iv) +
            scale(v1070r) + 
            scale(v7208) + scale(v7216) + scale(v7217) +
            scale(v7329) + scale(v7221) + scale(v7254),
          data = data_short
        ),
        error = function(err)
          NA
      )
    }
    
    #################################################
    # Extract Variables
    #################################################
    
    if (length(reg) == 1) {
      results_frame$t_value[n] <- NA
      results_frame$effect[n] <- NA
      results_frame$p_value[n] <- NA
      results_frame$standard_error[n] <- NA
      results_frame$number[n] <- NA
      results_frame$rsqrd[n] <- NA
    } else {
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
  }
  return(results_frame)
}

####################################################################################
# Execute Specification Curve Analyses
# We include digital technology use (x_variables),
# mental well-being (y_variables) and controls
# We include two sets of x and y variables as 
# Different subsets of participants answered 
# Different Questions
####################################################################################

#######################################################
# X Variables 
#######################################################
x_variables_1 <- list("v7326", "v7325", "v7381", "v7552", "tech")
x_names_1 <-
  c("TV Weekday", "TV Weekend", "Internet for News", "Social Media Use", "tech")

x_variables_2 <-
  list("v7544", "v7551", "v7553", "v7589", "v7590", "v7562", "v7563", "tech2")
x_names_2 <- c(
  "Computer for School",
  "Internet Use at Home",
  "Playing Electronic Games",
  "Social Media Use",
  "Video Chatting",
  "Texting on Mobile",
  "Calling on Mobile",
  "tech2"
)


#######################################################
# Y Variables
#######################################################
y_1 <-
  c(
    #Taking all things together, how would you say things are these days -- would you say you're very happy, pretty happy, or not too happy these days?
    "v7302",
    #Life often seems meaningless
    "v8502rev",
    #I enjoy life as much as anyone
    "v8505",
    #The future often seems hopeless
    "v8509rev",
    #It feels good to be alive
    "v8514",
    #I feel I am a person of worth, on an equal plane with others
    "v8504",
    #I take a positive attitude toward myself
    "v8501",
    #I am able to do things as well as most other people
    "v8508",
    #On the whole, I'm satisfied with myself
    "v8512",
    #I feel I do not have much to be proud of
    "v8503rev",
    #I feel that I can't do anything right
    "v8511rev",
    #I feel that my life is not very useful
    "v8513rev"
    #we do not include: v8507rev, sometimes I am not good at all (see supplementary)
  )
y_variables_1 <-
  (do.call("c", lapply(seq_along(y_1), function(i)
    combn(y_1, i, FUN = list))))
y_names_1 <-
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

y_variables_2 <- c("v7302")
y_names_2 <- c("Happiness")

#######################################################
# Control Variables
#######################################################
controls <-
  c("v1070r",
    "v7208",
    "v7216",
    "v7217",
    "v7329",
    "v7221",
    "v7254")
c_names <-
  c(
    "race recoded",
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
  "v7254", 
  "tech",
  "tech2"
)]

#######################################################
# Run and Save
#######################################################
results_mtf_ds_1 <-
  curve(resultsframe(x_var = x_variables_1, y_var = y_variables_1))

results_mtf_ds_2 <-
  curve(resultsframe(x_var = x_variables_2, y_var = y_variables_2))
results_mtf_ds_2 <- results_mtf_ds_2 %>% filter(!is.na(effect))

results_mtf_ds_total <- rbind(results_mtf_ds_1, results_mtf_ds_2)

save(results_mtf_ds_1, file = "2_2_sca_mtf_results_subset1.rda") #We save only those participants who completed more than one well-being item seperately
save(results_mtf_ds_total, file = "2_2_sca_mtf_results.rda") #We also save the complete sca
