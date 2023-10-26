# Install and load necessary libraries
install.packages(c("tidyverse", "ggthemes"))
library(tidyverse)
library(ggthemes)

# Load the data from the provided CSV files
countries_df <- read.csv('C:\\Users\\Rick-Royal\\Downloads\\Project-1\\Project-1\\countries.csv')
matdeaths_df <- read.csv('C:\\Users\\Rick-Royal\\Downloads\\Project-1\\Project-1\\matdeaths.csv')
mmr_df <- read.csv('C:\\Users\\Rick-Royal\\Downloads\\Project-1\\Project-1\\mmr.csv')
countries_df <- rename(countries_df, iso = `ï..iso`)

head(countries_df)
head(mmr_df)
head(matdeaths_df)

# Merge the datasets to include WHO regional groupings
mmr_with_region <- merge(mmr_df, select(countries_df, iso, region), by="iso", all.x=TRUE)
matdeaths_with_region <- merge(matdeaths_df, select(countries_df, iso, region), by="iso", all.x=TRUE)

# Filter data to focus on the lower bound (for clarity)
mmr_with_region_lower <- filter(mmr_with_region, bound == "lower")
matdeaths_with_region_lower <- filter(matdeaths_with_region, bound == "lower")

# Aggregate data by year and region
mmr_grouped <- mmr_with_region_lower %>%
  group_by(year, region) %>%
  summarise(mmr = mean(mmr, na.rm=TRUE))

matdeaths_grouped <- matdeaths_with_region_lower %>%
  group_by(year, region) %>%
  summarise(value = sum(value, na.rm=TRUE))

# Visualization for Maternal Mortality Ratio (MMR) by WHO Region
ggplot(mmr_grouped, aes(x=year, y=mmr, color=region)) +
  geom_line(size=1) +
  labs(title="Trends in Maternal Mortality Ratio (MMR) by WHO Region",
       x="Year",
       y="Maternal Mortality Ratio (per 100,000 live births)",
       color="WHO Region") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_tableau()

# Visualization for Maternal Deaths by WHO Region
ggplot(matdeaths_grouped, aes(x=year, y=value, color=region)) +
  geom_line(size=1) +
  labs(title="Trends in Maternal Deaths by WHO Region",
       x="Year",
       y="Total Maternal Deaths",
       color="WHO Region") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_tableau()
