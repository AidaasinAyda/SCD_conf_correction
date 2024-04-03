#Research subjects identifying with multiple races or ethnicities mentioned in the free text. Look at frequencies and where the study population was sourced from in these studies
library(dplyr)

ERC_matching$MixedindividualsconsideredasanERC <- trimws(ERC_matching$MixedindividualsconsideredasanERC)
# Create a frequency table
frequency_table <- table(ERC_matching$MixedindividualsconsideredasanERC)

frequency_table
mixed_selection <- ERC_matching %>%
  filter(MixedindividualsconsideredasanERC == "yes") %>% 
  group_by(Country1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

mixed_selection

#In studies with a SSA population, which ethno racial labels were used?

#Cleaning step: remove whitespace
ERC_matching$UseofBlackafrican <- trimws(ERC_matching$UseofBlackafrican)

#Cleaning step: Record number 3958 should be a "yes" instead of "N/A"

outlier_NA_afr <- ERC_matching %>% 
  filter(MatchERC == "yes" & trimws(UseofBlackafrican) == "N/A") %>% 
  select(Recordnumber)
ERC_matching$UseofBlackafrican <- trimws(ERC_matching$UseofBlackafrican)
ERC_matching <- ERC_matching %>% 
  mutate(UseofBlackafrican = ifelse(Recordnumber == 3958, "yes", UseofBlackafrican))

#Inspect which labels which are a derivative of African/Black were used in SSA
Table_labelsused <- ERC_matching %>%
  mutate(MatchERC = trimws(MatchERC),
         Geographical_region = trimws(Geographical_region)) %>%
  filter(MatchERC == "yes" & Geographical_region == "SSA") %>%
  select(Whatword) %>%
  filter(Whatword != "N/A") %>%  # Filter out "N/A" values
  unique()

print(Table_labelsused)



#Inspect cross-national comparative studies dataframe, studies that did adjust for ERCs
library(haven)
library(dplyr)

# Read the SPSS file using haven
ERC_matching_inter <- read_sav("Dataset_ERC_Matching_inter-country_studies_dd_17-03-2023.sav")

# Filter the dataframe
ndf <- ERC_matching_inter %>% 
  filter(MatchERC == "yes")

# Print the filtered dataframe
print(ndf)



