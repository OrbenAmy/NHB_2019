##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.3: MCS Analyse Specification Curves
##########################################################################################
setwd(".../2_sca")
library(tidyverse)

#################################################
# Bind Both
#################################################
load("2_3_sca_mcs_results_cm.rda")
load("2_3_sca_mcs_results_pr.rda")

results_mcs_sca_cm$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm))
results_mcs_sca_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_pr))

results_mcs_sca_total <- rbind(results_mcs_sca_cm, results_mcs_sca_pr)
save(results_mcs_sca_total, file = "2_3_sca_mcs_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_mcs_sca_total)

## Amount of results and significant results with dominant sign
results_mcs_sca_total_sig <- results_mcs_sca_total %>% filter(p_value < 0.05)
pmax(table(sign(results_mcs_sca_total$effect))[[-1]],table(sign(results_mcs_sca_total$effect))[[1]])
pmax(table(sign(results_mcs_sca_total_sig$effect))[[-1]],table(sign(results_mcs_sca_total_sig$effect))[[1]])


####################################################################################
# Median effects
# total, separate x variables, controls/no controls
####################################################################################
results_mcs_sca_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                    median_effectsize = median(rsqrd, na.rm = TRUE), 
                                    median_n = median(number, na.rm = TRUE),
                                    median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(respondent == "Parent") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                       median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                       median_n = median(number, na.rm = TRUE),
                                                                       median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(respondent == "Cohort Member") %>% summarise(median_effect = median(effect, na.rm = TRUE),
                                                                              median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                              median_n = median(number, na.rm = TRUE),
                                                                              median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                       median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                       median_n = median(number, na.rm = TRUE),
                                                                       median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(x_variable == "fctvho00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fccomh00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fccmex00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fcinth00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fcsome00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))

