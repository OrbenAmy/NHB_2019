##########################################################################################
# Specification Curve Analysis and Digital Technology USe
# R-Script 6.1: Plot the specification curves for MTF
##########################################################################################
setwd(".../2_sca")

#######################################################
# Load libraries
#######################################################
library("tidyverse")
library(ggplot2)
theme_set(theme_classic())
temp_plot <- NA

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
# Specification Curve
#############################################
h <- round(median(temp_data$effect), digits = 3)
plot1 <- ggplot(temp_data, aes(x = 1:nrow(temp_data))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") +
  geom_point(aes(y = effect, color = sig), size = 0.01) +
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
    axis.line.x = element_blank(),
    text = element_text(size=7)
  )

print(plot1)
setwd(".../6_plot_sca")

#############################################
# Specification Curve: number
#############################################
plot_number <- ggplot(temp_data, aes(x = 1:nrow(temp_data))) +
  geom_point(aes(y = number, color = sig), size = 0.1) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  ggtitle("SCA: MTF") +
  labs(x = "Specification (Ranked)", y = "Number of Observations") +
  theme(
    legend.position = "none"
  )
setwd(".../6_plot_sca")
ggsave(file="sfig2.jpg", plot_number, width = 6, height = 2)

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
    "tech"
  )
x_names <-
  c(
    "TV Weekday",
    "TV Weekend",
    "Internet for News",
    "Social Media Use",
    "Technology Mean"
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
  geom_jitter(shape = ".", alpha = 0.5, size = 0.001) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  coord_flip() +
  facet_grid(grouping_or ~ ., scales = "free", space = "free") +
  labs(y = "Specification Number", x = "Variables") +
  theme(
    legend.position = "none",
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    text = element_text(size=7)
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
ggsave(file="fig2.pdf", g,
       width = 180, height = 144, units = "mm")