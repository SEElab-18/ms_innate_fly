# Clear workspace
rm(list = ls())

# Load Libraries
library(tidyverse)
library(performance)
library(pavo)
library(glmmTMB)
library(DHARMa)

# Load data
data_colour_preference_stats <- read.csv("../02_data/colour_preference_rawdata.csv")
specs <- getspec("../02_data/spectra", ext = 'jaz')

# Count non-choices per species
  data_colour_preference_stats |> 
  filter(is.na(first_visit)) |> 
  group_by(fly_species) |> 
  summarise(non_choices = n())

# Filter non-choices
data_filtered <- 
  data_colour_preference_stats %>%
  filter(!is.na(first_visit), !is.na(fly_species))

#### VISUAL MODELLING ####
# Process spectra and calculate visual model
specs <- procspec(specs, opt = 'smooth', fixneg = 'zero')
vis.specs <- vismodel(specs, visual = "musca", achromatic = "md.r1")
cat.specs <- colspace(vis.specs, space = "categorical")

# Plots
plot(specs, col = c('pink', 'goldenrod', 'grey', 'blue', 'forestgreen'), lwd = 2)
plot(cat.specs, col = c('pink', 'goldenrod', 'grey', 'blue', 'forestgreen'))

# Create mapping for colour labels in cat.specs
cat.specs$first_visit <- c('pink', 'yellow', 'white', 'blue', 'green')

# Define colour options
colour_options <- c('pink', 'yellow', 'white', 'blue', 'green')

# Expand data so each fly has one row per colour option, then join correct HSL values
data_long <- data_filtered %>%
  select(fly_number, fly_species, fly_sex, first_visit) %>%  # do not include HSL columns here
  distinct() %>%
  crossing(colour_option = colour_options) %>%
  mutate(
    chosen = ifelse(first_visit == colour_option, 1, 0)
  ) %>%
  left_join(
    cat.specs %>% 
      mutate(colour_option = first_visit) %>% 
      select(colour_option, r.vec, h.theta, lum),
    by = "colour_option"
  )

# GLMMs for each species
# E. tenax
model_e_tenax <- glmmTMB(
  chosen ~ h.theta + r.vec + lum + (1 | fly_number),
  data = data_long %>% filter(fly_species == "E. tenax"),
  family = binomial
)

# M. domestica
model_m_domestica <- glmmTMB(
  chosen ~ h.theta + r.vec + lum + (1 | fly_number),
  data = data_long %>% filter(fly_species == "M. domestica"),
  family = binomial
)

# C. stygia
model_c_stygia <- glmmTMB(
  chosen ~ h.theta + r.vec + lum + (1 | fly_number),
  data = data_long %>% filter(fly_species == "C. stygia"),
  family = binomial
)

# LRTs via drop1() for each species model
lr_e_tenax     <- drop1(model_e_tenax,     test = "Chisq")
lr_m_domestica <- drop1(model_m_domestica, test = "Chisq")
lr_c_stygia    <- drop1(model_c_stygia,    test = "Chisq")

# Summarise models
summary(model_e_tenax)
r2(model_e_tenax)
summary(model_m_domestica)
r2(model_m_domestica)
summary(model_c_stygia)
r2(model_c_stygia)

# Assumptions check for each model
simulateResiduals(model_e_tenax, plot = TRUE)
simulateResiduals(model_m_domestica, plot = TRUE)
simulateResiduals(model_c_stygia, plot = TRUE)

#### CHI-SQUARE TESTS ####

# Function to perform Chi-squared Test
do_chisq_test <- function(data, species, split_by_sex = FALSE) {
  data_species <- data %>% filter(fly_species == species)
  
  if (split_by_sex) {
    data_species <- data_species %>% filter(!is.na(fly_sex))
    contingency_table <- table(data_species$first_visit, data_species$fly_sex)
  } else {
    contingency_table <- table(data_species$first_visit)
  }
  
  chisq.test(contingency_table)
}

# E. tenax
chisq_e_tenax <- do_chisq_test(data_filtered, "E. tenax")
chisq_e_tenax_sex <- do_chisq_test(data_filtered, "E. tenax", TRUE)

# M. domestica
chisq_m_domestica <- do_chisq_test(data_filtered, "M. domestica")
chisq_m_domestica_sex <- do_chisq_test(data_filtered, "M. domestica", TRUE)

# C. stygia
chisq_c_stygia <- do_chisq_test(data_filtered, "C. stygia")
chisq_c_stygia_sex <- do_chisq_test(data_filtered, "C. stygia", TRUE)

#### LATENCY AND DURATION ####

# Function to Convert Time Format to Minutes
convert_to_minutes <- function(time_col) {
  as.numeric(sub("\\..*", "", time_col)) + (as.numeric(sub(".*\\.", "", time_col)) / 60)
}

# Landing Time
model_latency <- glmmTMB(
  landing_time_1_min ~ fly_species * first_visit + (1 | fly_number),
  data = data_landing_time,
  family = Gamma(link = "log")
)
summary(model_latency)
car::Anova(model_latency, type = 2)  # Wald χ² tests
simulateResiduals(model_latency, plot = TRUE)

# Summary Statistics for Landing Time
summary_landing_time <- data_landing_time %>%
  group_by(fly_species) %>%
  summarise(
    Mean_Landing_Time = mean(landing_time_1_min, na.rm = TRUE),
    Median_Landing_Time = median(landing_time_1_min, na.rm = TRUE),
    SD_Landing_Time = sd(landing_time_1_min, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_landing_time)

# Duration of Visit
data_landing_duration <- data_filtered %>%
  mutate(landing_duration_1_min = convert_to_minutes(landing_duration_1))

model_duration <- glmmTMB(
  landing_duration_1_min ~ fly_species * first_visit + (1 | fly_number),
  data = data_landing_duration,
  family = Gamma(link = "log")
)
summary(model_duration)
car::Anova(model_duration, type = 2)  # Wald χ² tests
simulateResiduals(model_duration, plot = TRUE)

# Summary Statistics for Duration
summary_landing_duration <- 
  data_landing_duration %>%
  group_by(fly_species) %>%
  summarise(
    Mean_Duration_Time = mean(landing_duration_1_min, na.rm = TRUE),
    Median_Duration_Time = median(landing_duration_1_min, na.rm = TRUE),
    SD_Duration_Time = sd(landing_duration_1_min, na.rm = TRUE),
    .groups = 'drop'
  )