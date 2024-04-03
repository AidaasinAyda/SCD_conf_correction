# Convert "Geographical_region" to character if it's a factor
#Plot the frequencies of ERC adjustment per geographical region on a world map

library(sf)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)

# ERC_matching$MatchERC <- trimws(ERC_matching$MatchERC, which = "right")

region_percentage <- ERC_matching %>%
  select(Country1, Geographical_region, MatchERC) %>%
  # group_by the column you're interested in making groups from (e.g. turn countries into regions (continents etc))
  group_by(Geographical_region) %>%
  # count the yes and no for each region (that you created in the previous step)
  count(MatchERC) %>%
  # split the column that contains no/yes into 2 columns, assign these a value from the n (created by the count)
  pivot_wider(names_from = MatchERC, values_from = n) %>%
  # calculate the percentage
  mutate(perc_yes = yes * 100 / (no + yes))

# grab the lat and long data for the plots
world_map <- map_data("world")
# remove Antarctica
world_map <- subset(world_map, region != "Antarctica")

# join the regions with the countries
country_data <- ERC_matching %>%
  select(Country1, Geographical_region) %>%
  left_join(region_percentage)

# add the percentage data per country as well as per region
plotting_data <- world_map %>%
  full_join(country_data, by = c("region" = "Country1"))


# Do the plotting
percentage_map <- ggplot(plotting_data) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f") +
  geom_map(map = world_map, aes(map_id = region, fill = perc_yes)) +
  scale_fill_gradient(
    low = "white", high = "red", name = "ERC-adjustment per geographical region (%)"
  ) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_fixed() +
  theme(axis.title = element_blank(),  # Remove axis labels
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        legend.position = "top",       # Position legend at the top
        legend.direction = "horizontal")  # Horizontal legend

percentage_map
