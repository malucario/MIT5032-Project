# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)

# Load data
#setwd("") - uncomment and fill with your directory path to load file. Or add file to your project folder in R and run the line below.
df <- read.csv("Public.csv")
head(df)

# -----------------------------------------------------------------------------
# Hypothesis 1: More bird strikes will occur during migration months (i.e., April/October) during the daytime when there is no precipitation.
# Keep only the desired columns
birdstrike_filtered <- df %>%
  select(COST_REPAIRS, PRECIPITATION, INCIDENT_DATE, INCIDENT_MONTH, 
         INCIDENT_YEAR, TIME_OF_DAY, SIZE, HEIGHT, DISTANCE, 
         PHASE_OF_FLIGHT, STATE, DAMAGE_LEVEL)

# Create a column indicating if it's a migration month
birdstrike_filtered$migration_month <- birdstrike_filtered$INCIDENT_MONTH %in% c(4, 10)

# Create a new column for Day/Night classification
birdstrike_filtered$day_or_night <- ifelse(birdstrike_filtered$TIME_OF_DAY %in% c("Day"), "Day", "Night")

# Reclassify the PRECIPITATION column into a more structured factor variable
birdstrike_filtered$weather_condition <- factor(birdstrike_filtered$PRECIPITATION, 
                                                levels = c("None", "Rain", "Fog", "Snow", "Fog, Rain", "Rain, Snow"))

# Check the new levels
levels(birdstrike_filtered$weather_condition)

# Create summary data for the 3 factors: migration month, day or night, and weather condition
summary_data_3d <- birdstrike_filtered %>%
  group_by(migration_month, day_or_night, weather_condition) %>%
  summarise(bird_strike_count = n(), .groups = "drop")

# View the updated summary
print(n = 23, summary_data_3d)

#create the chart
ggplot(summary_data_3d, aes(x = weather_condition, y = bird_strike_count, fill = day_or_night)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ migration_month) +
  geom_text(aes(label = bird_strike_count), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  labs(x = "Weather Condition", y = "Bird Strike Count", fill = "Time of Day") +
  ggtitle("Bird Strikes: A Three-Dimensional Analysis", 
          subtitle = "Weather, Time of Day, and Migration Month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14),   # Main title size
    plot.subtitle = element_text(size = 10), # Subtitle size
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10) # Adjust top margin
  )

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
# Hypothesis 3: High-profile airlines (Delta, United, American, Southwest) have to pay more non-repair-related costs than other airlines at busier airports
# Mutate database
df_costs <- df %>% 
  mutate(
    AIRLINE_CLASS = ifelse(OPERATOR %in% c("AMERICAN AIRLINES", "SOUTHWEST AIRLINES", "DELTA AIR LINES", "UNITED AIRLINES"),
                     'MAJOR', "OTHER"), # categorize airlines into "major" or "other"
    BUSY_AIRPORT = case_when(
      AIRPORT %in% c('HARTSFIELD - JACKSON ATLANTA INTL ARPT', 'LOS ANGELES INTL','DALLAS/FORT WORTH INTL ARPT',
                     'DENVER INTL AIRPORT',"CHICAGO O'HARE INTL ARPT",'JOHN F KENNEDY INTL','ORLANDO INTL',
                     'HARRY REID INTERNATIONAL AIRPORT','CHARLOTTE/DOUGLAS INTL ARPT','MIAMI INTL','SEATTLE-TACOMA INTL',
                     'NEWARK LIBERTY INTL ARPT','SAN FRANCISCO INTL ARPT','PHOENIX SKY HARBOR INTL ARPT',
                     'GEORGE BUSH INTERCONTINENTAL/ HOUSTON ARPT','GENERAL EDWARD LAWRENCE LOGAN INTL ARPT',
                     'FORT LAUDERDALE/HOLLYWOOD INTL','MINNEAPOLIS-ST PAUL INTL/WOLD-CHAMBERLAIN ARPT','LA GUARDIA ARPT',
                     'DETROIT METRO WAYNE COUNTY ARPT') ~ 'BUSY',
      T ~ 'NOT BUSY' # categorise airports as "busy" or "not busy"
    )
  )

# Filter out private, business, and government flights (no customer feedback)
exclude_operators <- c('PRIVATELY OWNED', 'GOVERNMENT', 
                       'US CUSTOMS AND BORDER PROTECTION', 
                       'US COAST GUARD', 'BUSINESS')
df_costs <- df_costs %>%
  filter(!OPERATOR %in% exclude_operators)


# Calculate average costs for airport and airline groups
avg_cost_data <- df_costs %>%
  group_by(BUSY_AIRPORT, AIRLINE_CLASS) %>%
  summarise(Average_COST_OTHER = mean(COST_OTHER, na.rm = TRUE), .groups = "drop")


# Show average cost for major vs non-major airline at busy vs not busy airports
ggplot(avg_cost_data, aes(fill=AIRLINE_CLASS, y=Average_COST_OTHER, x=BUSY_AIRPORT)) +
  geom_bar(position='stack',stat='identity') + 
  labs(title="Non-Repair Costs Per Airline Group at Different Airport Classes", x="Airport Classification",y="Average Non-Repair Cost")

# -----------------------------------------------------------------------------
# Hypothesis 4: Damage levels and repair costs for larger aircraft have increased over the past five years.

#Past 5 years and large aircraft (Mass ≥ 3)
recent_data <- df[df$INCIDENT_YEAR >= (max(df$INCIDENT_YEAR) - 5), ]
recent_large_aircraft_data <- recent_data[recent_data$AC_MASS >= 3, ]

#Average repair costs by year and aircraft mass
avg_costs_mass_year <- aggregate(COST_REPAIRS ~ INCIDENT_YEAR + AC_MASS, data = recent_large_aircraft_data, FUN = mean, na.rm = TRUE)

#Repair Costs by Aircraft Mass
ggplot(avg_costs_mass_year, aes(x = factor(INCIDENT_YEAR), y = COST_REPAIRS, fill = factor(AC_MASS))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Repair Costs by Aircraft Mass (≥ 3) Over the Past Five Years",
    x = "Year",
    y = "Average Repair Costs (USD)",
    fill = "Aircraft Mass"
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "$", accuracy = 1)) + # Format y-axis as currency
  theme_minimal()

# DAMAGE_LEVEL data by excluding "M?"
recent_large_aircraft_data <- recent_large_aircraft_data[recent_large_aircraft_data$DAMAGE_LEVEL != "M?", ]

# DAMAGE_LEVEL by year
damage_level_by_year <- as.data.frame(table(recent_large_aircraft_data$INCIDENT_YEAR, recent_large_aircraft_data$DAMAGE_LEVEL))
colnames(damage_level_by_year) <- c("Year", "Damage_Level", "Count")

#Damage Levels by Year
ggplot(damage_level_by_year, aes(x = Year, y = Count, color = Damage_Level, group = Damage_Level)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Damage Levels Over the Past 5 Years for Large Aircrafts",
    x = "Year",
    y = "Count of Damaged Aircrafts",
    color = "Damage Level"
  ) +
  scale_color_manual(
    values = c("N" = "blue", "M" = "red", "S" = "green"),
    labels = c("N" = "None", "M" = "Minor", "S" = "Substantial")
  ) +
  theme_minimal()

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
