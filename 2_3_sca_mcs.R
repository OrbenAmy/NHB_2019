##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 2.3: MCS Make Specification Curves
##########################################################################################

# Because we use the university computer cluster we load the relevant data
vars <- Sys.getenv(c("HOME"))
data <- read.csv(file=paste0(vars["HOME"],"/1_3_prep_mcs_data.csv"), header=TRUE, sep = ",")
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
# There are too many y_variable combinations 
# we therefore aim to run about 100,000 tests and
# therefore want to choose about 800 y_variable
# specifications
#######################################################

#### choose 806 randomn combinations
y <-
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
  )
y_variables <-
  (do.call("c", lapply(seq_along(y), function(i)
    combn(y, i, FUN = list))))
y_variables_sample <-
  sample(y_variables[-(1:length(y))], 806, replace = FALSE)

#### choose all the y variables by themselves
y4 <- y_variables[1:length(y)]

#### Choose the three pre-specified variables
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
y_variables_sample_cm <- c(y_variables_sample, y4, list(y1, y2, y3))
save(y_variables_sample_cm, file = "2_3_sca_mcs_y_sample_cm.rda")
length(y_variables_sample_cm)
rm(y_variables)
rm(y1)
rm(y2)
rm(y3)
rm(y4)

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
    "fpwrdscm",
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
save(results_mcs_sca_cm, file = "2_3_sca_mcs_results_cm.rda")

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
# Similar procedure as above
# We choose less randomn combinations as there 
# are more measures and we want equal number of 
# specifications for cohort members and parents
#######################################################
y <-
  c(
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
    "fpsdte00"
  )
y_variables <- (do.call("c", lapply(seq_along(y), function(i) combn(y, i, FUN = list))))
y_variables_sample <- sample(y_variables[-(1:length(y))], 801, replace = FALSE)

# Choose variables by themselves
y1 <- y_variables[1:length(y)]

# Choose mean variables
y2 <- c("fconduct")
y3 <- c("fhyper")
y4 <- c("fpeer")
y5 <- c("fprosoc")
y6 <- c("febdtot") # total
y7 <- c("femotion")
y8 <- c("femotion", "fpeer")
y9 <- c("fconduct", "fhyper")

# bind data
y_variables_sample_pr <- c(y_variables_sample, y1,
    list(y2,y3,y4,y5,y6,y7,y8,y9))
save(y_variables_sample_pr, file = "2_3_sca_mcs_y_sample_pr.rda")
length(y_variables_sample_pr)

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
    "fpwrdscm",
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
save(results_mcs_sca_pr, file = "2_3_sca_mcs_results_pr.rda")

