library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(mapproj)
library(sf)
library(tidyr)
library(rgeos)
library(rworldmap) #heeft de getmap function
#Read in an sav file
ERC_matching <- haven::read_sav("data/Dataset ERC Matching without inter-country studies dd 17-03-2023.sav")

#zo was het voorheen. Nu het geupdate bestand in de nieuwe dataframe, inclusief mixed individuals "yes"
ERC_matching <- read.spss("C:\\Users\\asofk\\OneDrive\\Документы\\ERC_matching\\data\\ERC_matching_domestic_studies_dd_05-05-2023.sav",to.data.frame=TRUE)

# "no" in MatchERC bevat een spatie aan de rechterkant, trimmen
ERC_matching$MatchERC <- trimws(ERC_matching$MatchERC, which = "right")

#####uitstapje centroids berekenen
# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
centroids_df <- as.data.frame(centroids)

#heeft geen eerste columnnaam. rechtzetten
colnames(centroids_df)

# Add additional columns to centroids_df based on "x" and "y" columns
centroids_df$region <- row.names(centroids_df)
centroids_df$long <- centroids_df$x
centroids_df$lat <- centroids_df$y

# Remove original "x" and "y" columns
centroids_df$x <- NULL
centroids_df$y <- NULL

# Update column names
colnames(centroids_df) <- c("region", "long", "lat")

#nu wil ik de article count eigenlijk hierbij voegen
merged_data2 = merge(article_count, centroids_df, by.x="Country", by.y = "region")

#Nu Palesstine, Kurdistan en Guadeloupe toevoegen aan merged_data2.Coorinaten van Siirt, Salfit en Point-a-pitre toevoegen als observaties aan centroids_df. Let op, Kurdistan bestaat niet in world. dus geen group!
# Create a new dataframe with the new observations
new_data <- data.frame(
  Country = c("Kurdistan", "Palestine", "Guadeloupe"),
  Article_Count = c(1, 2, 14),
  long = c(41.941978, 35.178822, -61.533590),
  lat = c(37.927402, 32.083012, 16.238090)
)


#Create a data frame which contains the amount of unique articles published per country
# Create a new data frame with two columns: "Country" and "Article_Count"
article_count <- ERC_matching %>%
  group_by(Country1) %>%   # Group by "Country1" column
  summarise(Article_Count = n())   # Count the number of articles per country

# Rename the "Country1" column to "Country"
article_count <- rename(article_count, Country = Country1)

#Controle: check whether there are any countries that do not match between article_count and centroids_df
non_matching_obs <- anti_join(article_count, centroids_df, by = c("Country" = "region"))

# View non-matching observations
print(non_matching_obs)

#nu veranderen van de namen die niet matchen
# Define the mapping of old observation names to new observation names
observation_mapping <- c("DRC" = "Democratic Republic of the Congo",
                         "Republic_of_the_Congo" = "Republic of the Congo",
                         "Ivory_Coast" = "Ivory Coast",
                         "Tanzania" = "United Republic of Tanzania",
                         "UK" = "United Kingdom",
                         "USA" = "United States of America")

# Update observation names in article_count dataframe
article_count <- article_count %>% 
  mutate(Country = recode(Country, !!!observation_mapping, .default = Country))


# Use rbind() to append the new data to merged_data2
merged_data2 <- rbind(merged_data2, new_data)


# Load the RDS file into a new variable
#van geextraheerde tabel HbS prevalence een RDS file gemaakt
#tot nu toe niet succesvol in het veranderen van een string naar numeric
Prevalence_data_AF2 <- readRDS("C:\\Users\\asofk\\OneDrive\\Документы\\ERC_matching\\Prevalence_data_AF.rds")



#eerst de string splitsen op spacebar
# Split the HbS_AF column by space and keep only the first part
# Split the "HbS AF (IQR5)" column by space and keep only the first part
Prevalence_data_AF2$`HbS AF (IQR5)` <- as.numeric(sapply(strsplit(as.character(Prevalence_data_AF2$`HbS AF (IQR5)`), " "), `[`, 1)) #succesvol in prevalentiecijfer HbS allelfrequentie

is.numeric(Prevalence_data_AF2$`HbS AF (IQR5)`)#is nu een nummer

#opslaan als RDS
saveRDS(Prevalence_data_AF2, file= "Prevalence_data_HbSAF.rds")

head(Prevalence_data_AF2)


# Merge merged_data2 and Prevalence_data_AF2 data frames by "Country" column
merged_data3 <- inner_join(merged_data2, Prevalence_data_AF2, by = "Country")

#plak de allele frequency vast aan de world map
merged_data4 <- world_map %>%
  full_join(merged_data3, by = join_by(region == Country), multiple = "all")



merged_data4 <- merged_data4 %>%
  select(-c(Population, CBR, `WHO/HbS region`, Surveys, `AS neonates/year`, `SS neonates/year`, "M&D"))


# Load the map data
world_map <- map_data("world")

#remove antarctica
world_map <- subset(world_map, region != "Antarctica")

#UK en USA respectievelijk veranderen naar United States of America en United Kingdom
# Change "UK" to "United Kingdom" and "USA" to "United States" Nog niet gecheckt voor alle landen
world_map$region <- ifelse(world_map$region == "UK", "United Kingdom", world_map$region)
world_map$region <- ifelse(world_map$region == "USA", "United States of America", world_map$region)
world_map$region <- ifelse(world_map$region == "Tanzania", "United Republic of Tanzania", world_map$region)


#check of alle landen uit merged_data4 overeenkomen met world_map
# Get the unique values of the "Country" column in merged_data4
merged_data4_countries <- unique(merged_data4$region)

# Check if all the unique countries in merged_data4 appear in world_map
all(merged_data4_countries %in% world_map$region)

setdiff(unique(merged_data4$region), unique(world_map$region))
#returns: UK, Tanzania en USA. Gefixt inmiddels

#Hier verder gaan naar de wereldkaart

# Merge data frames based on a common column, e.g., "Country" or "name"
#merged_data <- merge(world_map, article_count, by.x = "region", by.y = "Country")

#Create the map
map_plot <- ggplot() +
   geom_map(data= merged_data4, map=world_map, aes(map_id=region, fill = `HbS AF (IQR5)`)) +
  coord_map(xlim=c(-180,180), ylim=c(-60,80))+ 
  theme_minimal()+
  scale_fill_gradient(name = "HbS AF (IQR5)", low = "#F0F0F0", high = "blue", na.value = "gray80", guide = "colorbar")+
  expand_limits(x = merged_data4$long.y, y = merged_data4$lat.y)
  

# Add circles on top of the map with size based on "Article Count" data
map_plot <- map_plot +
  geom_point(data = merged_data3, aes(x = long, y = lat, size = Article_Count),
             color = "black", fill='transparent', shape = 21) +
  scale_size_continuous(name = "Article Count in 11 years", range = c(2, 10),
                        breaks = seq(100, max(merged_data3$Article_Count), by = 50)) +  # Use "Article Count" as the size of the circles
  guides(size = guide_legend())

# Customize the plot
map_plot <- map_plot +
  labs(title = "Geographical Map with Circles", x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"), 
       
)
        

# Display the plot
map_plot
