##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.1: YRBS Analyse Specification Curves
##########################################################################################

setwd(".../2_sca")
library(tidyverse)

load("2_1_sca_yrbs_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_yrbs_sca)

## Amount of results and significant results with dominant sign
results_yrbs_sca_sig <- results_yrbs_sca %>% filter(p_value < 0.05)
pmax(table(sign(results_yrbs_sca$effect))[[-1]],table(sign(results_yrbs_sca$effect))[[1]])
table(sign(results_yrbs_sca_sig$effect))[[-1]]

####################################################################################
# Median effects 
# total, separate x variables, controls/no controls
####################################################################################
results_yrbs_sca %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                               median_effectsize = median(rsqrd, na.rm = TRUE), 
                               median_n = median(number, na.rm = TRUE),
                               median_se = median(standard_error, na.rm = TRUE))

results_yrbs_sca %>% filter(x_variable == "q81_n") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                 median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                 median_n = median(number, na.rm = TRUE),
                                                                 median_se = median(standard_error, na.rm = TRUE))
results_yrbs_sca %>% filter(x_variable == "q82_n") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                median_n = median(number, na.rm = TRUE),
                                                                median_se = median(standard_error, na.rm = TRUE))

results_yrbs_sca %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                  median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                  median_n = median(number, na.rm = TRUE),
                                                                  median_se = median(standard_error, na.rm = TRUE))
results_yrbs_sca %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE),
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
