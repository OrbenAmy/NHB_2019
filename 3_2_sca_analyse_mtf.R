##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.2: MTF Analyse Specification Curves
##########################################################################################
setwd(".../2_sca")
library(tidyverse)
load("2_2_sca_mtf_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_mtf_ds_total)

## Amount of results and significant results with dominant sign
results_mtf_ds_total_sig <- results_mtf_ds_total %>% filter(p_value < 0.05)
pmax(table(sign(results_mtf_ds_total$effect))[[-1]],table(sign(results_mtf_ds_total$effect))[[1]])
pmax(table(sign(results_mtf_ds_total_sig$effect))[[-1]],table(sign(results_mtf_ds_total_sig$effect))[[1]])

####################################################################################
# Median effects
# total, separate x variables, controls/no controls
####################################################################################
results_mtf_ds_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                   median_effectsize = median(rsqrd, na.rm = TRUE), 
                                   median_n = median(number, na.rm = TRUE),
                                   median_se = median(standard_error, na.rm = TRUE))

results_mtf_ds_total %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                      median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                      median_n = median(number, na.rm = TRUE),
                                                                      median_se = median(standard_error, na.rm = TRUE))
results_mtf_ds_total %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                         median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                         median_n = median(number, na.rm = TRUE),
                                                                         median_se = median(standard_error, na.rm = TRUE))

results_mtf_ds_total %>% filter(x_variable == "v7325") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7552") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7381") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7326") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
