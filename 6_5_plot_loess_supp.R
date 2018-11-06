##########################################################################################
# Specification Curve Analysis and Digital Technology USe
# R-Script 6.4: Plot LOESS in supplementals
##########################################################################################
setwd(".../2_sca")

#######################################################
# Load libraries
#######################################################
library("tidyverse")
library(viridisLite)
library(ggplot2)
library(gridExtra)

theme_set(theme_classic())
temp_plot <- NA

#######################################################
# Remove NAs
#######################################################
temp_data <- get(load("2_3_sca_mcs_results.rda"))

for (n in 1:nrow(temp_data)) {
  if (temp_data[n, 6] != 0 & is.na(temp_data[n, 5])) {
    temp_data[n, 5] <- 0
  } else {}
}

#####################################
# Sort by Effect sizes
#####################################
temp_data <- temp_data[order(temp_data$effect),]
temp_data$index[!is.na(temp_data$effect)] <-
  1:nrow(temp_data[!is.na(temp_data$effect),])
temp_data$sig <- "0"
temp_data$sig[temp_data$p_value < .05] <- "1"
temp_data$upper <- temp_data$effect + temp_data$standard_error
temp_data$lower <- temp_data$effect - temp_data$standard_error

#############################################
# Smoother Report
#############################################
temp_data$respondent_f <- ifelse(temp_data$respondent == "Cohort Member", 0, 1)
temp_data$effect_hat <- fitted(loess(respondent_f~index, temp_data))

plot1 <- ggplot(temp_data, aes(x = index)) +
  geom_jitter(aes(y = respondent_f), shape = ".", alpha = 0.5) +
  geom_line(aes(y = effect_hat)) +
  labs(subtitle = "Association between technology use and well-being more negative when adolescent self-report is used") +
  scale_y_continuous(name = "Adolescent Self-Report (0) or Parent Report (1)", breaks = c(0,1)) +
  scale_x_continuous(name = "Specification (sorted)") +
  theme(
    legend.position = "none"
  )
print(plot1)
ggsave(file="sfig7.jpg", plot1, width = 8, height = 4)
#############################################
# Smoother Controls
#############################################
temp_data$controls_f <- ifelse(temp_data$controls == "No Controls", 0, 1)
temp_data$effect_hat <- fitted(loess(controls_f~index, temp_data))

plot2 <- ggplot(temp_data, aes(x = index)) +
  geom_jitter(aes(y = controls_f), shape = ".", alpha = 0.5) +
  geom_line(aes(y = effect_hat)) +
  labs(subtitle = "Association between technology use and well-being less negative when controls included") +
  scale_y_continuous(name = "No Controls (0) or Controls (1)", breaks = c(0,1)) +
  scale_x_continuous(name = "Specification (sorted)") +
  theme(
    legend.position = "none"
  )
print(plot2)
ggsave(file="sfig8.jpg", plot2, width = 8, height = 4)
