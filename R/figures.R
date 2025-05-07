# Clear workspace
rm(list=ls())


# Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

# Data

data_colour_preference <- read.csv("../data/colour_preference_rawdata.csv")

#### FIRST VISITS ONLY

### Proportion of flies visiting flowers of each colour.
data_first_visit <- data_colour_preference %>%
  mutate(fly_species = case_when(
    fly_species == "C. stygia" ~ "Calliphora stygia",
    fly_species == "E. tenax" ~ "Eristalis tenax",
    fly_species == "M. domestica" ~ "Musca domestica",
    TRUE ~ fly_species
  )) %>%
  mutate(fly_species = factor(fly_species, levels = c("Eristalis tenax", "Calliphora stygia", "Musca domestica"))) %>%
  filter(!is.na(first_visit) & !is.na(fly_species) & fly_species != "Unknown") %>%
  count(fly_species, first_visit) %>%
  group_by(fly_species) %>%
  mutate(proportion = n / sum(n))

colour_map <- c(
  "yellow" = "#feed00",  
  "blue" = "#33b3ff",
  "pink" = "#fd95d6",
  "white" = "white"
)

## Bargraph - proportion of flies visiting each colour
p1 <- 
ggplot(data_first_visit, aes(x = first_visit, y = proportion, fill = first_visit)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) + 
  geom_text(aes(label = n),           
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  size = 4) +           
  labs(
    x = "Flower colour first visited",
    y = "Proportion of flies visiting"
  ) +
  #facet_wrap(~ fly_species, scales = "free_y") +  
  facet_grid(. ~ fly_species, scales = "fixed") +  # Arrange facets in columns
  scale_fill_manual(values = colour_map) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-0.02, 0.74)) + 
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = 1.8),   
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.position = "none",                 # Hide legend
    strip.text = element_text(face = "italic"), 
    #panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black box around the plot
    panel.spacing = unit(1.5, "lines"))  # Increase spacing between panels

### Proportions of flies visiting flowers of each colour by sex 

# Filter data and count occurrences, then calculate proportions
data_sex_first_visit <- data_colour_preference %>%
  mutate(fly_species = case_when(
    fly_species == "C. stygia" ~ "Calliphora stygia",
    fly_species == "E. tenax" ~ "Eristalis tenax",
    fly_species == "M. domestica" ~ "Musca domestica",
    TRUE ~ fly_species
  )) %>%
  mutate(fly_species = factor(fly_species, levels = c("Eristalis tenax", "Calliphora stygia", "Musca domestica"))) %>%
  filter(!is.na(first_visit) & !is.na(fly_sex) & !is.na(fly_species) & fly_species != "Unknown") %>%
  count(fly_species, fly_sex, first_visit) %>%
  group_by(fly_species, fly_sex) %>%  
  mutate(proportion = n / sum(n))     

# Create the bar graph with proportions
ggplot(data_sex_first_visit, aes(x = fly_sex, y = proportion, fill = first_visit)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) + 
  geom_text(aes(label = n),         
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  size = 10) +          
  scale_fill_manual(values = colour_map) +  
  labs(
    x = "Flower first visited separated by sex",
    y = "Proportion of flies visiting",
    fill = "First Visit"
  ) +
  facet_wrap(~ fly_species, scales = "free_y") +  
  scale_y_continuous(expand = c(0, 0), limits = c(-0.03, 0.75)) +  
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 29),  
    axis.text.y = element_text(size = 32),  
    axis.title.x = element_text(size = 34, vjust = -0.5),  
    axis.title.y = element_text(size = 34, vjust = 1.8),  
    plot.title = element_text(size = 34),    
    strip.text = element_text(size = 32, face = "italic"),  
    legend.text = element_text(size = 32),   
    legend.title = element_text(size = 32),   
    legend.position = "none",              
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.spacing = unit(1.5, "lines"))  

### TIME TAKEN TO LAND ON EACH COLOUR

# Filtering data for time taken to land first visit
data_time_taken_to_land <- data_colour_preference %>%
  mutate(fly_species = case_when(
    fly_species == "C. stygia" ~ "Calliphora stygia",
    fly_species == "E. tenax" ~ "Eristalis tenax",
    fly_species == "M. domestica" ~ "Musca domestica",
    TRUE ~ fly_species
  )) %>%
  mutate(fly_species = factor(fly_species, levels = c("Eristalis tenax", "Calliphora stygia", "Musca domestica"))) %>%
  filter(!is.na(landing_time_1) & !is.na(first_visit) & !is.na(fly_species) & fly_species != "Unknown") %>%
  filter(first_visit %in% names(colour_map)) %>%  # Keep only the colors specified in color_map
  mutate(
    # Convert landing_time_1 from character (minutes and seconds) to numeric (total minutes)
    landing_time_1_min = as.numeric(sub("\\..*", "", landing_time_1)) +  # Extract whole minutes
      (as.numeric(sub(".*\\.", "", landing_time_1)) / 60)  # Convert seconds to minutes
  )

# Create the boxplot
p2 <- 
ggplot(data_time_taken_to_land, aes(x = first_visit, y = landing_time_1_min, fill = first_visit)) +
  geom_boxplot(colour = "black") +  
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.8) +  
  scale_fill_manual(values = colour_map) +  
  labs(
    x = "Flower first visited",
    y = "Time taken to land (min)",
    fill = "Colour"
  ) +
  #facet_wrap(~ fly_species, scales = "free_y") +  
  facet_grid(. ~ fly_species, scales = "fixed") +  # Arrange facets in columns
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 20.5)) + 
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = 1.8),  
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_blank(), 
    legend.position = "none",                 
    #panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.spacing = unit(1.5, "lines")) 


### LANDING DURATION ON EACH COLOUR

# Filtering data to include duration for first visit

data_duration <- data_colour_preference %>%
  mutate(fly_species = case_when(
    fly_species == "C. stygia" ~ "Calliphora stygia",
    fly_species == "E. tenax" ~ "Eristalis tenax",
    fly_species == "M. domestica" ~ "Musca domestica",
    TRUE ~ fly_species
  )) %>%
  mutate(fly_species = factor(fly_species, levels = c("Eristalis tenax", "Calliphora stygia", "Musca domestica"))) %>%
  filter(!is.na(landing_duration_1) & !is.na(first_visit) & !is.na(fly_species) & fly_species != "Unknown") %>%
  filter(first_visit %in% names(colour_map))  # Keep only the colors specified in color_map


# Create the boxplot
p3 <-
ggplot(data_duration, aes(x = first_visit, y = landing_duration_1, fill = first_visit)) + 
  geom_boxplot(colour = "black") +  # Create the boxplot with black borders
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.8) +  # Add jittered points on top
  scale_fill_manual(values = colour_map) +  # Apply the color map
  labs(
    x = "Flower first visited",
    y = "Duration of visit (min)",
    fill = "Colour"
  ) +
  #facet_wrap(~ fly_species, scales = "free_y") +  # Facet by fly species
  facet_grid(. ~ fly_species, scales = "fixed") +  # Arrange facets in columns
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 20.5)) +  # Ensure y-axis starts at 0 and no extra space at the bottom
  theme_minimal() + 
  theme(
    axis.title.x = element_text(vjust = -0.5),  # Increase x-axis title size
    axis.title.y = element_text(vjust = 1.8),  # Increase y-axis title size
    legend.position = "none",                 # Hide legend
    strip.text = element_blank(),
    #panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black box around the plot
    panel.spacing = unit(1.5, "lines"))  # Increase spacing between panels

# Arrance plots
  p1 / p2 / p3
  ggsave('../figs/choices.png', height = 10)


