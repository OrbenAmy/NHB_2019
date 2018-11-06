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

####################################################################################
# Median effects
# total, separate x variables, controls/no controls
####################################################################################
results_mtf_ds_total %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))

results_mtf_ds_total %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
results_mtf_ds_total %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))

results_mtf_ds_total %>% filter(x_variable == "v7325") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7552") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7381") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
results_mtf_ds_total %>% filter(x_variable == "v7326") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
