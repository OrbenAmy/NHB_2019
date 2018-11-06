##########################################################################################
# Specification Curve Analysis and Digital Technology USe
# R-Script 6.1: Plot the specification curves for MTF
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
  as.numeric(1:nrow(temp_data[!is.na(temp_data$effect),]))
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
# Specification Curve: number
#############################################
plot_number <- ggplot(temp_data, aes(x = 1:nrow(temp_data))) +
  geom_point(aes(y = number, color = sig), size = 0.1) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  ggtitle("SCA: MCS") +
  scale_y_continuous(name = "Number of Observations") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )
setwd(".../6_plot_sca")
ggsave(file="sfig3.jpg", plot_number, width = 6, height = 2)

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
  geom_jitter(shape = ".", alpha = 0.5) +
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
ggsave(file="fig3.jpg", g, width = 10, height = 8)

#####################################
# Plot control/no control separately
#####################################

median_effect_all <- median(temp_data$effect, na.rm = TRUE)
median_p_all <- median(temp_data$p_value, na.rm = TRUE)
colours <-
  viridis(
    7,
    alpha = 1,
    begin = 0,
    end = 0.9,
    direction = -1
  )
colours_lite <-
  viridis(
    7,
    alpha = 0.008,
    begin = 0,
    end = 0.9,
    direction = -1
  )

temp_data_c <- temp_data %>% filter(controls == "Controls")
temp_data_c$index2[!is.na(temp_data_c$effect)] <-
  1:nrow(temp_data_c[!is.na(temp_data_c$effect), ])
temp_data_c$col <-
  ifelse(temp_data_c$sig == 0, colours[4], colours_lite[4])
median_effect_c <- median(temp_data_c$effect, na.rm = TRUE)
median_p_c <- median(temp_data_c$p_value, na.rm = TRUE)

temp_data_nc <- temp_data %>% filter(controls == "No Controls")
temp_data_nc$index2[!is.na(temp_data_nc$effect)] <-
  1:nrow(temp_data_nc[!is.na(temp_data_nc$effect), ])
temp_data_nc$col <-
  ifelse(temp_data_nc$sig == 0, colours[7], colours_lite[7])
median_effect_nc <- median(temp_data_nc$effect, na.rm = TRUE)
median_p_nc <- median(temp_data_nc$p_value, na.rm = TRUE)

median_effect_nc
median_effect_c
h1 <- median_effect_c
h2 <- median_effect_nc

#############################################
# Plot
#############################################
plot1 <- ggplot() +
  geom_hline(yintercept = h2,
             linetype = "dashed",
             colour = colours[7]) +
  geom_hline(yintercept = h1,
             linetype = "dashed",
             colour = colours[4]) +
  geom_ribbon(
    data = temp_data_c,
    aes(ymin = lower, ymax = upper, x = index2),
    colour = NA,
    fill = colours[4],
    alpha = 0.3
  ) +
  geom_point(data = temp_data_c,
             aes(y = effect, x = index2, colour = col),
             size = 0.1) +
  geom_ribbon(
    data = temp_data_nc,
    aes(ymin = lower, ymax = upper, x = index2),
    colour = NA,
    fill = colours[7],
    alpha = 0.5
  ) +
  geom_point(data = temp_data_nc,
             aes(y = effect, x = index2, colour = col),
             size = 0.1) +
  scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000),
                     limits = c(0, 11000)) +
  ggtitle("Adolescent Well-Being") +
  labs(x = "Specification Number", y = "Regression Coefficient") +
  annotate(
    "text",
    x = 10500,
    y = c(-0.078,-0.014),
    label = c("No Controls = -0.068", "Controls = -0.005"),
    size = 3,
    colour = c(colours[7], colours[4]),
    hjust = 1
  ) +
  scale_color_identity()
print(plot1)

ggsave(
  filename = "fig4.png",
  width = 5,
  height = 5
)
