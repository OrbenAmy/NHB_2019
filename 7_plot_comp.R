##########################################################################################
# Specification Curve Analysis and Social Media 
# R-Script 7: Plotting specification curves 
##########################################################################################
setwd(".../5_comp")

library(tidyverse)
theme_set(theme_classic())
library(viridisLite)
library(gridExtra)
library(grid)

##########################################################################################
# MCS
##########################################################################################

#####################################
# X variables and X names
#####################################
x_variables <- c("fchtcm00", "fccycf00r", "fcbrkn00", "fcfrut00", 
                 "fcvegi00", "sleeptime", "fcglas00r", 
                 "fcares00r", "fchurt00r", "fccanb00r", "fcalfv00r", 
                 "hand", "tech")
x_names <- c("Height", "Bicycle Use", "Breakfast", "Fruit", 
             "Vegetabes", "Sleep", "Glasses", 
             "Been Arrested", "Bullying", "Smoked Weed", "Bingedrinking", 
             "Handedness", "Technology Use")
vars <- as.data.frame(cbind(x_variables, x_names))
names(vars) <- c("Group.1", "Names")

#####################################
# Examine medians and % change
#####################################
load("5_3_comp_mcs_results.rda")
medians <- aggregate(results_mcs_ds_comp[, 4], list(results_mcs_ds_comp$x_variable), median)
medians <- left_join(medians, vars, by = "Group.1" )
names(medians) <- c("Var_name", "x", "Group.1")
medians$percentage <- ((medians$x-medians[13,2])/abs(medians[13,2])*100)
medians$times <- medians$x/medians[13,2]
medians

number <- aggregate(results_mcs_ds_comp[, 7:9], list(results_mcs_ds_comp$x_variable), median)
number$number <- round(number$number, 0)
number$rsqrd <- round(number$rsqrd, 3)
number$standard_error <- round(number$standard_error, 3)
number <- left_join(number, vars, by = "Group.1" )
number
#####################################
# Plot medians
#####################################
median_plot <- medians %>% 
  filter(Group.1 %in% c("Bicycle Use", "Height", "Handedness", "Glasses", "Technology Use")) %>%
  arrange(x)
median_plot$group_factor <- factor(median_plot$Group.1, ordered = TRUE)
median_plot$group_factor <- factor(median_plot$group_factor, levels(median_plot$group_factor)[c(2,5,3,4,1)])
bar1 <- ggplot(data = median_plot, aes(x = group_factor, y = x, fill = group_factor)) +
  scale_fill_manual(values = magma(5, alpha = 0.4, begin = 0, end = 0.8, direction = 1), guide = FALSE) +
  coord_flip() +
  geom_bar(stat="identity") + 
  scale_x_discrete(position = "top") +
  theme(text = element_text(size=7)) +
  labs(x = "Adolescent Variable", y = "Median Standardised Regression Coefficient")
bar1

#####################################
# Edit MCS data
#####################################
for (i in 1:length(x_variables)){
  temp_data <- results_mcs_ds_comp
  temp_data <- temp_data %>% filter(x_variable == x_variables[i]) ##filter data for each comparison variable
  
  for (n in 1:nrow(temp_data)){ ##remove NAs
    if (temp_data[n,5] != 0 & is.na(temp_data[n,4])){
      temp_data[n,4] <- 0
    } else {}
  }
  
  temp_data <- temp_data[order(temp_data$effect),] ##sort by effect sizes
  temp_data$index[!is.na(temp_data$effect)] <- 1:nrow(temp_data[!is.na(temp_data$effect),]) ##create index variable
  
  temp_data$sig <- "0" ##add signifiance
  temp_data$sig[temp_data$p_value < .05] <- "1"
  
  temp_data$upper <- temp_data$effect+temp_data$standard_error ##add standard errors
  temp_data$lower <- temp_data$effect-temp_data$standard_error
  
  if (temp_data[1,1] != "tech"){
    if (temp_data[1,1] %in% x_variables[c(3,4,5,6)]){
      temp_data$level <- "Panel A: Positive"
    } else if (temp_data[1,1] %in% x_variables[c(1,2,7,12)]){
      temp_data$level <- "Panel B: No Effect"
    } else if (temp_data[1,1] %in% x_variables[c(8,9,10,11)]){
      temp_data$level <- "Panel C: Negative"
    } else {
      temp_data$level <- "0"
    }
    assign(paste("temp_data_", i, sep = ""), temp_data) ##assign name
    
  } else {
    for (j in 1:3){
      names <- c("Panel A: Positive", "Panel B: No Effect", "Panel C: Negative")
      temp_data$level <- names[j]
      assign(paste("temp_data_tech_", j, sep = ""), temp_data)
    }
  }
}


### merge data
temp_data <- do.call("rbind", list(temp_data_1, temp_data_2, temp_data_3, temp_data_4,
                                   temp_data_5, temp_data_6, temp_data_7, temp_data_8,
                                   temp_data_9, temp_data_10, temp_data_11, temp_data_12,
                                   temp_data_tech_1, temp_data_tech_2,
                                   temp_data_tech_3))
temp_data$x_variable <- dplyr::recode(temp_data$x_variable, 
                                      'fchtcm00'='Height',
                                      'fccycf00r'='Bicycle',
                                      'fcbrkn00'='Breakfast',
                                      'fcfrut00'='Fruit',
                                      'fcvegi00'='Vegetables',
                                      'sleeptime'='Sleep',
                                      'fcglas00r'='Glasses',
                                      'fcares00r'='Arrested',
                                      'fchurt00r'='Bullied',
                                      'fccanb00r'='Marijuana',
                                      'fcalfv00r'='Binge Drinking',
                                      'hand'='Handedness',
                                      'tech'='Technology')

#order needed cyberbully, marijuana, bingedrinking, tech, 
#arrested, glasses, handedness, height, bicycle
#vegetables, fruit, sleep, breakfast
temp_data$x_variable_f <- factor(temp_data$x_variable, ordered = TRUE)
temp_data$x_variable_f <- factor(temp_data$x_variable_f, levels(temp_data$x_variable_f)[c(5,10,3,1, 
                                                                                          7,12,8,9,2,
                                                                                          13,6,11,4)])
temp_data$x_variable_f <- factor(temp_data$x_variable_f, levels=rev(levels(temp_data$x_variable_f)))

#############################################
# Specification indicators: Aggregate Curve
#############################################
temp_plot <- NA
setwd(".../7_plot_comp")

scaleFUN <- function(x) sprintf("%.2f", x)

xs <- split(temp_data, f = temp_data$level)
p1_mcs <- ggplot(data = xs$`Panel C: Negative`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.2),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification (Ranked)", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p2_mcs <- ggplot(data = xs$`Panel B: No Effect`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.2),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p3_mcs <- ggplot(data = xs$`Panel A: Positive`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.2),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 
grid.arrange(p1_mcs,p2_mcs, p3_mcs, nrow = 3)
g1 <- arrangeGrob(p3_mcs,p2_mcs,p1_mcs, nrow = 3)

setwd(".../7_plot_comp")
ggsave(filename = paste("sfig11.png"),
       width = 6, height = 12, g1)

#############################################
# merge bar and stuff
#############################################
p2 <- ggplot(data = xs$`No Effect`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=7), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(3, "mm"),
        legend.position = c(0.8, 0.2),
        text = element_text(size=7),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_blank()) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

bar1_wp <- arrangeGrob(bar1, top = textGrob("B", x = unit(0.2, "npc")
                                          , y   = unit(0.9, "npc"), just=c("centre","top"),
                                          gp=gpar(col="black", fontsize=7)))
p2_wp <- arrangeGrob(p2, top = textGrob("A", x = unit(0.2, "npc")
                                               , y   = unit(0.9, "npc"), just=c("centre","top"),
                                               gp=gpar(col="black", fontsize=7)))

fig5 <- grid.arrange(p2_wp, bar1_wp, ncol = 2)
ggsave(filename = paste("fig5.pdf"),
       width = 180, height = 90, units = "mm", fig5)

##########################################################################################
# YRBS
##########################################################################################
setwd(".../5_comp")

#####################################
# X variables and X names
#####################################
x_variables <- c("q72", "q79", "q76", "q88",
                 "q74", "q78", "q69", "q87r",
                 "q47", "q44", "q24_n", "q18", "tech")

x_names <- c("Eat fruit", "Eat breakfast", "Eat vegetables", "hours sleep", 
             "Eat potatoes", "Drink Milk", "Perceived Weight", "Asthma",
             "Marijuana", "Bingedrinking", "Bullied", "Fight",
             "technology use")
vars <- as.data.frame(cbind(x_variables, x_names))
names(vars) <- c("Group.1", "Names")

#####################################
# Medians
#####################################
load("5_1_comp_yrbs_results.rda")
results_yrbs_sro_mom <- results_yrbs_sro_mom[,-9]
medians <- aggregate(results_yrbs_sro_mom[,5], list(results_yrbs_sro_mom$x_variable), median)
medians <- left_join(medians, vars, by = "Group.1" )
names(medians) <- c("Var_name", "x", "Group.1")
medians$percentage <- ((medians$x-medians[13,2])/abs(medians[13,2])*100)
medians$times <- medians$x/medians[13,2]
medians

number <- aggregate(results_yrbs_sro_mom[, c(6,8,9)], list(results_yrbs_sro_mom$x_variable), median)
number$number <- round(number$number, 0)
number$rsqrd <- round(number$rsqrd, 3)
number$standard_error <- round(number$standard_error, 3)
number <- left_join(number, vars, by = "Group.1" )
number

#####################################
# Load specification data 
# Remove NAs 
#####################################
### separate data
for (i in 1:length(x_variables)){
  temp_data <- results_yrbs_sro_mom  ##load data
  temp_data <- temp_data %>% dplyr::filter(x_variable == x_variables[i]) ##filter data for each comparison variable
  
  for (n in 1:nrow(temp_data)){ ##remove NAs
    if (temp_data[n,5] != 0 & is.na(temp_data[n,8])){
      temp_data[n,5] <- 0
    } else {}
  }
  
  temp_data <- temp_data[order(temp_data$effect),] ##sort by effect sizes
  temp_data$index[!is.na(temp_data$effect)] <- 1:nrow(temp_data[!is.na(temp_data$effect),]) ##create index variable
  
  temp_data$sig <- "0" ##add signifiance
  temp_data$sig[temp_data$p_value < .05] <- "1"
  
  temp_data$upper <- temp_data$effect+temp_data$standard_error ##add standard errors
  temp_data$lower <- temp_data$effect-temp_data$standard_error
  
  if (temp_data[1,1] != "tech"){
    if (temp_data[1,1] %in% x_variables[c(1,2,3,4)]){
      temp_data$level <- "Panel A: Positive"
    } else if (temp_data[1,1] %in% x_variables[c(5,6,7,8)]){
      temp_data$level <- "Panel B: No Effect"
    } else if (temp_data[1,1] %in% x_variables[c(9,10,11,12)]){
      temp_data$level <- "Panel C: Negative"
    } else {
      temp_data$level <- "0"
    }
    assign(paste("temp_data_", i, sep = ""), temp_data) ##assign name
    
  } else {
    for (j in 1:3){
      names <- c("Panel A: Positive", "Panel B: No Effect", "Panel C: Negative")
      temp_data$level <- names[j]
      assign(paste("temp_data_tech_", j, sep = ""), temp_data)
    }
  }
  
}

### merge data
temp_data <- do.call("rbind", list(temp_data_1, temp_data_2, temp_data_3, temp_data_4,
                                   temp_data_5, temp_data_6, temp_data_7, temp_data_8,
                                   temp_data_9, temp_data_10, temp_data_11, temp_data_12,
                                   temp_data_tech_1, temp_data_tech_2,
                                   temp_data_tech_3))
temp_data$x_variable <- dplyr::recode(temp_data$x_variable, 'q72'='Fruit',
                                      'q79'='Breakfast',
                                      'q76'='Vegetables',
                                      'q88'='Sleep',
                                      'q74'='Potatoes',
                                      'q78'='Milk',
                                      'q69'='Perceived Weight',
                                      'q87r'='Asthma',
                                      'q47'='Marijuana',
                                      'q44'='Binge Drinking',
                                      'q24_n'='Bullied',
                                      'q18'='Fight',
                                      'tech'='Technology')

#order: sleep, breakfast, fruit, vegetables, 
#asthma, milk, potatoes, perceived weight, tech,
#binge drinking, marijuana, fight, bully
temp_data$x_variable_f <- factor(temp_data$x_variable, ordered = TRUE)
temp_data$x_variable_f <- factor(temp_data$x_variable_f, levels(temp_data$x_variable_f)[c(11, 3, 6, 12,
                                                                                          1, 8, 10, 9,
                                                                                          13, 2, 7, 5, 4)])

#############################################
# Specification indicators: Curve
#############################################
temp_plot <- NA

scaleFUN <- function(x) sprintf("%.2f", x)

xs <- split(temp_data, f = temp_data$level)
p1_yrbs <- ggplot(data = xs$`Panel C: Negative`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.2),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification (Ranked)", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p2_yrbs <- ggplot(data = xs$`Panel B: No Effect`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p3_yrbs <- ggplot(data = xs$`Panel A: Positive`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.5),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free")
grid.arrange(p3_yrbs,p2_yrbs,p1_yrbs, nrow = 3)
g2 <- arrangeGrob(p3_yrbs,p2_yrbs,p1_yrbs, nrow = 3)

setwd(".../7_plot_comp")
ggsave(filename = paste("sfig9.png"),
       width = 6, height = 12, g2)

##########################################################################################
# MTF
##########################################################################################

#####################################
# X variables and X names
#####################################
x_variables <- c("v8526", "v8530", "v7251", "v8527",
                 "v7588", "v7219", "v7309", "v7327", 
                 "v7109", "v7101", "v7113", "v8516", "tech") 
x_names <- c("breakfast", "sleep", "eat fruit", "eat vegetables",
             "listen music", "religious service",  "go to movies", "homework time",
             "drink alc", "Cigarettes", "marijuana", "fight", "technology")
vars <- as.data.frame(cbind(x_variables, x_names))
names(vars) <- c("Group.1", "Names")
#####################################
# Medians
#####################################
load("5_2_comp_mtf_results.rda")
medians <- aggregate(results_mtf_ds_comp[, 4], list(results_mtf_ds_comp$x_variable), median)
medians <- left_join(medians, vars, by = "Group.1" )
medians$percentage <- ((medians$x-medians[1,2])/abs(medians[1,2])*100)
medians$times <- medians$x/medians[1,2]
medians

number <- aggregate(results_mtf_ds_comp[, c(7:9)], list(results_mtf_ds_comp$x_variable), median)
number$number <- round(number$number, 0)
number$rsqrd <- round(number$rsqrd, 3)
number$standard_error <- round(number$standard_error, 3)
number <- left_join(number, vars, by = "Group.1" )
number

#####################################
# Load specification data 
# Remove NAs 
#####################################
### separate data
for (i in 1:length(x_variables)){
  load("5_2_comp_mtf_results.rda")
  temp_data <- results_mtf_ds_comp  ##load data
  temp_data <- temp_data %>% filter(x_variable == x_variables[i]) ##filter data for each comparison variable
  
  for (n in 1:nrow(temp_data)){ ##remove NAs
    if (temp_data[n,4] != 0 & is.na(temp_data[n,6])){
      temp_data[n,4] <- 0
    } else {}
  }
  
  temp_data <- temp_data[order(temp_data$effect),] ##sort by effect sizes
  temp_data$index[!is.na(temp_data$effect)] <- 1:nrow(temp_data[!is.na(temp_data$effect),]) ##create index variable
  
  temp_data$sig <- "0" ##add signifiance
  temp_data$sig[temp_data$p_value < .05] <- "1"
  
  temp_data$upper <- temp_data$effect+temp_data$standard_error ##add standard errors
  temp_data$lower <- temp_data$effect-temp_data$standard_error
  
  if (temp_data[1,1] != "tech"){
    if (temp_data[1,1] %in% x_variables[c(1,2,3,4)]){
      temp_data$level <- "Panel A: Positive"
    } else if (temp_data[1,1] %in% x_variables[c(5,6,7,8)]){
      temp_data$level <- "Panel B: No Effect"
    } else if (temp_data[1,1] %in% x_variables[c(9,10,11,12)]){
      temp_data$level <- "Panel C: Negative"
    } else {
      temp_data$level <- "0"
    }
    assign(paste("temp_data_", i, sep = ""), temp_data) ##assign name
    
  } else {
    for (j in 1:3){
      names <- c("Panel A: Positive", "Panel B: No Effect", "Panel C: Negative")
      temp_data$level <- names[j]
      assign(paste("temp_data_tech_", j, sep = ""), temp_data)
    }
  }
}

### merge data
temp_data <- do.call("rbind", list(temp_data_1, temp_data_2, temp_data_3, temp_data_4,
                                   temp_data_5, temp_data_6, temp_data_7, temp_data_8,
                                   temp_data_9, temp_data_10, temp_data_11, temp_data_12, 
                                   temp_data_tech_1, temp_data_tech_2,
                                   temp_data_tech_3))
temp_data$x_variable <- dplyr::recode(temp_data$x_variable, 
                                      'v8526'='Breakfast',
                                      'v8530'='Sleep',
                                      'v7251'='Fruit',
                                      'v7101'='Cigarettes',
                                      'v8527'='Vegetables',
                                      'v7588'='Music',
                                      'v7219'='Religion',
                                      'v7309'='Go to Movies',
                                      'v7327'='Homework Time',
                                      'v7109'='Binge Drinking',
                                      'v7113'='Marijuana',
                                      'v8516'='Fight',
                                      'tech'='Technology')

#ordered: music, cigarettes, fight, weed, alcohol
# tech, homework, fruit, movies
# religion, veggie, breakfast, sleep
temp_data$x_variable_f <- factor(temp_data$x_variable, ordered = TRUE)
temp_data$x_variable_f <- factor(temp_data$x_variable_f, levels(temp_data$x_variable_f)[c(11,2,13,10,
                                                                                          6,5,7,12,
                                                                                          1,8,4,3,9)])
#############################################
# Specification indicators: Curve
#############################################
temp_plot <- NA

scaleFUN <- function(x) sprintf("%.2f", x)

xs <- split(temp_data, f = temp_data$level)
p1_mtf <- ggplot(data = xs$`Panel C: Negative`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.2),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification (Ranked)", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p2_mtf <- ggplot(data = xs$`Panel B: No Effect`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.8, 0.1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

p3_mtf <- ggplot(data = xs$`Panel A: Positive`, aes(x = index, colour = x_variable_f, fill = x_variable_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4,  colour=NA) +
  geom_line(aes(y=effect), size = 0.2) +
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0, end = 0.8, direction = -1), guide = FALSE) +
  scale_fill_manual(values = magma(5, alpha = 0.0000001, begin = 0, end = 0.8, direction = -1)) +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.margin = margin(0,0), 
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.3, 0.85),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  labs(fill="", x = "Specification", y = "Standardised Regression Coefficient") +
  scale_y_continuous(labels=scaleFUN) +
  facet_grid(. ~level, scales="free") 

grid.arrange(p3_mtf,p2_mtf,p1_mtf, nrow = 3)
g3 <- arrangeGrob(p3_mtf,p2_mtf,p1_mtf, nrow = 3)

setwd(".../7_plot_comp")
ggsave(filename = paste("sfig10.png"),
       width = 6, height = 12, g3)

##########################################################################################
# Merge and make general graph 
##########################################################################################
setwd(".../7_plot_comp")
t1 <- grid.arrange(p3_yrbs,p2_yrbs,p1_yrbs, ncol=1)
t2 <- grid.arrange(p3_mtf,p2_mtf,p1_mtf, ncol=1)
t3 <- grid.arrange(p3_mcs,p2_mcs,p1_mcs, ncol=1)
gc <- arrangeGrob(t1,t2,t3,ncol=3)
ggsave(filename = paste("plot_comp.png", sep = ""),
       width = 12, height = 12, gc)

