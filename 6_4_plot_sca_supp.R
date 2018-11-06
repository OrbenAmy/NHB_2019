##########################################################################################
# Specification Curve Analysis and Digital Technology USe
# R-Script 6.4: Plot the supplementary material plots 
##########################################################################################
setwd(".../2_sca")

#######################################################
# Load libraries
#######################################################
library("tidyverse")
library(ggplot2)
library(gridExtra)
theme_set(theme_classic())
temp_plot <- NA

#######################################################
# Remove NAs
#######################################################
temp_data <- get(load("2_2_sca_mtf_results.rda"))

for (n in 1:nrow(temp_data)) {
  if (temp_data[n, 7] != 0 & is.na(temp_data[n, 6])) {
    temp_data[n, 6] <- 0
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
# Specification Curve
#############################################
h <- round(median(temp_data$effect), digits = 3)
plot1 <- ggplot(temp_data, aes(x = 1:nrow(temp_data))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") +
  geom_point(aes(y = effect, color = sig), size = 0.1) +
  geom_hline(yintercept = h, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  ggtitle("Adolescent Well-Being") +
  scale_y_continuous(name = "Regression Coefficient") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )

print(plot1)
setwd(".../6_plot_sca")

#############################################
# Specification Scatterplot
#############################################

####################
# Define Variables
####################
x_var <-
  c(
    "v7326",
    "v7325",
    "v7381",
    "v7552",
    "v7544",
    "v7551",
    "v7553",
    "v7589",
    "v7590",
    "v7562",
    "v7563",
    "tech",
    "tech2"
  )
x_names <-
  c(
    "TV Weekday",
    "TV Weekend",
    "Internet for News",
    "Social Media Use",
    "Computer for School",
    "Internet Use at Home",
    "Playing Electronic Games",
    "Social Media Use (7 item)",
    "Video Chatting",
    "Texting on Mobile",
    "Calling on Mobile",
    "Technology Mean 1",
    "Technology Mean 2"
  )

y_var <-
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
y_names <-
  c(
    "Unhappiness",
    "Life is meaningless",
    "Enjoy life -",
    "Future is hopeless",
    "Good to be alive -",
    "Person of worth -",
    "Positive attitude towards self -",
    "Do well as most others -",
    "Satisfied with myself -",
    "Not much to be proud of",
    "Can't do anything right",
    "Life not useful"
  )

d_var <- c("Controls", "No Controls")
d_names <- c("Controls", "No Controls")

axis_names <-
  c("Technology Use", "Depressive Symptoms", "Controls")

####################
# Make Scatterplot
####################
results_frame <- temp_data

variables <- c(x_var, y_var, d_var)
dot_data <-
  array(0, dim = c(nrow(results_frame), (length(variables))))

for (i in 1:nrow(results_frame)) {
  factors_x <- results_frame[[i, 1]]
  factors_y <- results_frame[[i, 2]]
  factors_d <- as.character(results_frame[[i, 3]])
  for (l in 1:(length(variables))) {
    if ((identical(factors_x, variables[l]) == TRUE) |
        (any(factors_y == variables[l]) == TRUE) |
        (identical(factors_d, variables[l]) == TRUE) == TRUE)  {
      dot_data[i, l] <- results_frame[[i, 10]]
    } else {
      dot_data[i, l] <- NA
    }
  }
}

dot_data <- as.data.frame(dot_data)
names(dot_data) <- c(x_names, y_names, d_names)

dot_data_long <-
  gather(dot_data, vars, vars_score, 1:ncol(dot_data))
dot_data_long <- dot_data_long[complete.cases(dot_data_long),]
dot_data_long$grouping <- 1

for (i in 1:nrow(dot_data_long)) {
  dot_data_long[i, 3] <-
    ifelse((any(dot_data_long[i, 1] == x_names) == TRUE),
           axis_names[1],
           ifelse((any(
             dot_data_long[i, 1] == y_names
           ) == TRUE), axis_names[2], axis_names[3]))
}

dd <- as.data.frame(colMeans(dot_data, na.rm = TRUE))
dd <- rownames_to_column(dd, var = "rowname")
names(dd) <- c("rowname", "mean")
dd$grouping <- 1
for (i in 1:nrow(dd)) {
  dd[i, 3] <-
    ifelse((any(dd[i, 1] == x_names) == TRUE),
           axis_names[1],
           ifelse((any(dd[i, 1] == y_names) == TRUE), axis_names[2], axis_names[3]))
}
dd <- dd %>% dplyr::arrange(mean) %>% dplyr::arrange(grouping)

dot_data_long$grouping_or <-
  factor(dot_data_long$grouping,
         levels = axis_names,
         ordered = TRUE)
dot_data_long$vars_or <-
  factor(dot_data_long$vars,
         ordered = TRUE,
         levels = dd$rowname)

index_data <- results_frame[, c("index", "sig")]
colnames(index_data)[1] <- "vars_score"
dot_data_long <-
  dplyr::left_join(dot_data_long, index_data, by = "vars_score")

plot.multiverse.vars <-
  ggplot(data = dot_data_long, aes(x = vars_or, y = vars_score, color = sig)) +
  geom_point(shape = 15,
             size = 2,
             alpha = 0.05) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  coord_flip() +
  facet_grid(grouping_or ~ ., scales = "free", space = "free") +
  labs(y = "Specification Number", x = "Variables") +
  theme(
    legend.position = "none",
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_blank()
  )
print(plot.multiverse.vars)

### Put curve and scatterplot together
plots <- list(plot1, plot.multiverse.vars)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}
g <- do.call("grid.arrange", c(grobs, ncol = 1))

### Save
ggsave(file="sfig4.jpg", g, width = 10, height = 8)

##########################################################################################
# Figure 2: separate
##########################################################################################
setwd("/2_sca")

#######################################################
# Remove NAs
#######################################################
temp_data <- get(load("2_2_sca_mtf_results_subset1.rda"))

for (n in 1:nrow(temp_data)) {
  if (temp_data[n, 7] != 0 & is.na(temp_data[n, 6])) {
    temp_data[n, 6] <- 0
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
# Variables
#############################################
x_variables <- list("v7326", "v7325", "v7381", "v7552")
x_names <-
  c(
    "TV Viewing on Weekday",
    "TV Viewing on Weekend",
    "Using Internet to get News",
    "Social Media Use"
  )

for (m in 1:4) {
  x_var <- x_variables[m]
  temp_data_2 <- temp_data %>% filter(grepl(x_var, x_variable))
  temp_data_2$index2[!is.na(temp_data_2$effect)] <-
    1:nrow(temp_data_2[!is.na(temp_data_2$effect), ])
  
  temp_data_1 <-
    dplyr::filter(temp_data_2, grepl('v8502rev', y_variable))
  temp_data_1 <-
    dplyr::filter(temp_data_1, grepl('v8509rev', y_variable))
  temp_data_1 <-
    dplyr::filter(temp_data_1, grepl('v8505', y_variable))
  temp_data_1 <-
    dplyr::filter(temp_data_1, grepl('v8514', y_variable))
  temp_data_1 <-
    dplyr::filter(temp_data_1, grepl('v8513rev', y_variable))
  temp_data_1 <-
    dplyr::filter(temp_data_1, grepl('v8511rev', y_variable))
  
  for (n in 1:nrow(temp_data_1)) {
    if (length(temp_data_1$y_variable[[n]]) == 6 &
        temp_data_1[n, 3] == "No Controls") {
      row <- n
    } else {
    }
  }
  
  print(temp_data_1[row, 14] / nrow(temp_data_2))
  
  temp_data_2$test <- rep("0", nrow(temp_data_2))
  temp_data_2[(match(temp_data_1[row, 14], temp_data_2$index2)), 'test'] <-
    "1"
  
  h <- round(median(temp_data_2$effect), digits = 3)
  plot1 <- ggplot(temp_data_2, aes(x = 1:nrow(temp_data_2))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") +
    geom_point(aes(y = effect, color = test, size = test)) +
    geom_hline(yintercept = h, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c("#000000", "#FF0000")) +
    scale_size_manual(values = c(0.1, 2)) +
    ggtitle(x_names[m]) +
    scale_y_continuous(name = "Regression Coefficient") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    )
  
  print(plot1)
  assign(paste("plot_", m, sep = ""), plot1)
  
}

setwd("6_plot_sca")

grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2)
gc <- arrangeGrob(plot_1, plot_2, plot_3, plot_4, ncol = 2)
ggsave(
  filename = paste("sfig5.jpg", sep = ""),
  width = 9,
  height = 4,
  gc
)

##########################################################################################
# Figure 3: MCS Defensible Specifications Curve
##########################################################################################
setwd("/2_sca")

#######################################################
# Load libraries
#######################################################
library("tidyverse")
library(viridisLite)
library(ggplot2)
library(grid)

theme_set(theme_classic())
temp_plot <- NA

#######################################################
# Remove NAs
#######################################################
temp_data <- get(load("2_4_sca_mcs_supp_results.rda"))

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
# Specification Curve
#############################################
h <- round(median(temp_data$effect), digits = 3)
plot1 <- ggplot(temp_data, aes(x = 1:nrow(temp_data))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") +
  geom_point(aes(y = effect, color = sig), size = 0.1) +
  geom_hline(yintercept = h, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  ggtitle("Adolescent Well-Being") +
  scale_y_continuous(name = "Regression Coefficient") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )
print(plot1)

#############################################
# Ptint numbers
#############################################
print(h)
round(median(temp_data[temp_data$respondent == "Cohort Member",]$effect), digits = 3)
round(median(temp_data[temp_data$respondent != "Cohort Member",]$effect), digits = 3)

#############################################
# Specification indicators: Scatterplot
#############################################
x_var <-
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
    "Mean Technology"
  )

mf <-
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
se <-
  c("fcsati00r",
    "fcgdql00r",
    "fcdowl00r",
    "fcvalu00r",
    "fcgdsf00r")
wb <-
  c("fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r")
sdq_cp <-
  c("fconduct",
    "fpsdtt00",
    "fpsdor00",
    "fpsdfb00",
    "fpsdcs00",
    "fpsdoa00")
sdq_hy <-
  c("fhyper",
    "fpsdro00",
    "fpsdfs00",
    "fpsddc00",
    "fpsdte00",
    "fpsdst00")
sdq_pp <-
  c("fpeer",
    "fpsdsp00",
    "fpsdgf00",
    "fpsdlc00",
    "fpsdgb00",
    "fpsdpb00")
sdq_pr <-
  c("fprosoc",
    "fpsdpf00",
    "fpsdsr00",
    "fpsdhu00",
    "fpsdky00",
    "fpsdvh00")
sdq_em <-
  c("femotion",
    "fpsdhs00",
    "fpsdmw00",
    "fpsdud00",
    "fpsdnc00",
    "fpsdfe00")

y_names <-
  c(
    "Well-Being",
    "Rosenberg Self-Esteem",
    "Moods and Feelings Scale",
    "SDQ Conduct Problems",
    "SDQ Hyperactivity",
    "SDQ Peer Problems",
    "SDQ Prosocial",
    "SDQ Emotional Symptoms"
  )

c_var <- c("Parent", "Cohort Member")

d_var <- c("Controls", "No Controls")

axis_names <-
  c("technology use", "mental health", "respondent", "controls")

y_var <- y_names
c_names <- c_var
d_names <- d_var
results_frame <- temp_data

variables <- c(x_var, y_var, c_var, d_var)
dot_data <-
  array(0, dim = c(nrow(results_frame), (length(variables))))

for (i in 1:nrow(results_frame)) {
  factors_x <- results_frame[[i, 1]]
  factors_y <- results_frame[[i, 2]]
  factors_c <- results_frame[[i, 10]]
  factors_d <- results_frame[[i, 3]]
  for (l in 1:(length(variables))) {
    if ((any(factors_x == variables[l]) == TRUE) |
        (any(factors_d == variables[l]) == TRUE) |
        (any(factors_c == variables[l]) == TRUE) == TRUE) {
      dot_data[i, l] <- results_frame[[i, 11]]
    } else {
      dot_data[i, l] <- NA
    }
  }
  if (any(factors_y %in% wb) == TRUE) {
    dot_data[i, 7] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% se) == TRUE) {
    dot_data[i, 8] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% mf) == TRUE) {
    dot_data[i, 9] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% sdq_cp) == TRUE) {
    dot_data[i, 10] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% sdq_hy) == TRUE) {
    dot_data[i, 11] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% sdq_pp) == TRUE) {
    dot_data[i, 12] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% sdq_pr) == TRUE) {
    dot_data[i, 13] <- results_frame[[i, 11]]
  }
  if (any(factors_y %in% sdq_em) == TRUE) {
    dot_data[i, 14] <- results_frame[[i, 11]]
  }
}

dot_data <- as.data.frame(dot_data)
names(dot_data) <- c(x_names, y_names, c_names, d_names)

dot_data_long <-
  gather(dot_data, vars, vars_score, 1:ncol(dot_data))
dot_data_long <- dot_data_long[complete.cases(dot_data_long), ]
dot_data_long$grouping <- 1

for (i in 1:nrow(dot_data_long)) {
  dot_data_long[i, 3] <-
    ifelse((any(dot_data_long[i, 1] == x_names) == TRUE), axis_names[1],
           ifelse((any(dot_data_long[i, 1] == y_names) == TRUE),
                  axis_names[2],
                  ifelse((any(
                    dot_data_long[i, 1] == c_names
                  ) == TRUE), axis_names[3], axis_names[4])
           ))
}


dd <- as.data.frame(colMeans(dot_data, na.rm = TRUE))
dd <- rownames_to_column(dd, var = "rowname")
names(dd) <- c("rowname", "mean")
dd$grouping <- 1
for (i in 1:nrow(dd)) {
  dd[i, 3] <-
    ifelse((any(dd[i, 1] == x_names) == TRUE), axis_names[1], ifelse((any(dd[i, 1] == y_names) == TRUE),
                                                                     axis_names[2],
                                                                     ifelse((any(dd[i, 1] == c_names) == TRUE), axis_names[3], axis_names[4])
    ))
}

dd <- dd %>% dplyr::arrange(mean) %>% dplyr::arrange(grouping)

dot_data_long$grouping_or <-
  factor(dot_data_long$grouping,
         levels = axis_names,
         ordered = TRUE)
dot_data_long$vars_or <-
  factor(dot_data_long$vars,
         ordered = TRUE,
         levels = dd$rowname)

index_data <- results_frame[, c("index", "sig")]
colnames(index_data)[1] <- "vars_score"
dot_data_long <-
  dplyr::left_join(dot_data_long, index_data, by = "vars_score")

plot.multiverse.vars <-
  ggplot(data = dot_data_long, aes(x = vars_or, y = vars_score, color = sig)) +
  geom_point(shape = 15,
             size = 2,
             alpha = 1) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  coord_flip() +
  facet_grid(grouping_or ~ ., scales = "free", space = "free") +
  labs(y = "Specification Number", x = "Variables") +
  theme(
    legend.position = "none",
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_blank()
  )
print(plot.multiverse.vars)


### Put curve and scatterplot together
plots <- list(plot1, plot.multiverse.vars)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}
g <- do.call("grid.arrange", c(grobs, ncol = 1))

### Save
ggsave(file="sfig6.jpg", g, width = 10, height = 8)
