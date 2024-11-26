# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(grid)
library(gridExtra)

# Load data
df <- read.csv("Public.csv")

# Hypothesis 1: High-profile airlines (Delta, United, American, Southwest) have to pay
# non-repair-related costs more often than other airlines and pay them in larger sums


# -----------------------------------------------------------------------------
# Hypothesis 2: Repair costs are highest when planes are struck by large birds during takeoff and landing
# Filter data for multiple species
large_bird_data <- df %>%
  filter(SPECIES %in% c("Unknown bird - large", "Unknown bird - medium", "Unknown bird - small"))

# Calculate mean repair costs by phase of flight and species
mean_values <- large_bird_data %>%
  group_by(PHASE_OF_FLIGHT, SPECIES) %>%
  summarise(mean_cost = mean(COST_REPAIRS, na.rm = TRUE), .groups = "drop")

# Create bar plot with mean repair costs and custom colors for each species
ggplot(data = large_bird_data, aes(x = PHASE_OF_FLIGHT, y = COST_REPAIRS, fill = SPECIES)) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(data = mean_values, aes(x = PHASE_OF_FLIGHT, y = mean_cost, 
                                    label = scales::label_dollar()(mean_cost)), vjust = -0.5) +
  labs(title = "Repair Costs for Large Birds Struck During Takeoff and Landing", 
       x = "Phase of Flight", 
       y = "Repair Costs (USD)") +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_fill_manual(values = c("Unknown bird - large" = "blue", 
                               "Unknown bird - medium" = "green", 
                               "Unknown bird - small" = "red"))  # Custom colors for each species


# -----------------------------------------------------------------------------
# Hypothesis 3:


# -----------------------------------------------------------------------------
# Hypothesis 4:


# -----------------------------------------------------------------------------
# Hypothesis 5: More strikes occur when cloud coverage and precipitation levels are higher

# Step 1: Filter relevant data and clean unexpected categories
df_weather <- df %>%
  filter(!is.na(SKY), !is.na(PRECIPITATION)) %>% # Remove rows with missing SKY or PRECIPITATION
  mutate(
    PRECIPITATION = case_when(
      PRECIPITATION == "" ~ "None",               # Treat empty strings as "None"
      PRECIPITATION == "Fog" ~ "Fog",             # Keep "Fog" as is
      PRECIPITATION %in% c("Fog, Rain", "Rain, Snow") ~ "Rain", # Combine complex values into "Rain"
      TRUE ~ PRECIPITATION                        # Keep all other values as-is
    )
  ) %>%
  group_by(SKY, PRECIPITATION) %>%
  summarize(Num_Strikes = n(), .groups = 'drop') # Count number of strikes for each combination

# Step 2: Plot bird strikes by cloud coverage without legend
plot_cloud_coverage <- ggplot(df_weather, aes(x = reorder(SKY, -Num_Strikes), y = Num_Strikes, fill = SKY)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Bird Strikes by Cloud Coverage",
    x = "Cloud Coverage",
    y = "Number of Strikes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Step 3: Plot bird strikes by precipitation without legend
plot_precipitation <- ggplot(df_weather, aes(x = reorder(PRECIPITATION, -Num_Strikes), y = Num_Strikes, fill = PRECIPITATION)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Bird Strikes by Precipitation Levels",
    x = "Precipitation",
    y = "Number of Strikes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Step 4: Combined analysis (cloud coverage and precipitation) with legend
plot_combined <- ggplot(df_weather, aes(x = SKY, y = Num_Strikes, fill = PRECIPITATION)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "None" = "#F8766D",
    "Rain" = "#00BFC4",
    "Snow" = "#C77CFF",
    "Fog" = "#999999"
  )) +
  labs(
    title = "Bird Strikes by Cloud Coverage and Precipitation",
    x = "Cloud Coverage",
    y = "Number of Strikes",
    fill = "Precipitation"
  )

    # Step 5: Display plots
    plot_combined
    grid.arrange(plot_cloud_coverage, plot_precipitation, plot_combined, ncol = 1)
