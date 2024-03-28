#Frequency of which method was used in ascertaining ERCs (y/n)
# Which ERC labels were used?
library(dplyr)
library(forcats)
library(foreign)

#If there is ERC-adjustment in a paper, then the methodology determining ERC mentioned should be a yes or no. Not, N/A
NA_method <-ERC_matching %>% filter(MatchERC=="yes")%>% filter(MethodologydeterminingERCmentioned == "N/A")%>%
  data.frame()

#Cleaning step: We discover here that row 3661 en 4191 should be no, correct this
ERC_matching <- ERC_matching %>%
  mutate(MethodologydeterminingERCmentioned = ifelse(Recordnumber %in% c(3661, 4191), "no", MethodologydeterminingERCmentioned))
#Trim whitespace
ERC_matching$MethodologydeterminingERCmentioned <- trimws(ERC_matching$MethodologydeterminingERCmentioned)

#Create the table "Methodology determining ERC mentioned?" This table only contains "yes" or "no"
Tabel_method_yn <-ERC_matching %>% filter(MatchERC== "yes") %>% 
  select(MethodologydeterminingERCmentioned) %>% 
  table()

#What methodology is used?. 
#Cleaning step: Remove the option "sibling/relative recruitment" since this method can be used to correct for more factors
method_df <- data.frame(
  ERC_matching %>% 
    filter(MethodologydeterminingERCmentioned == "yes") %>%
    select(What_method) %>%
    filter(What_method != "sibling/relative recruitment") %>%
    table()
)
print(method_df)

#Select the cases in which no specification was provided on the method of ERC confounder adjustment.

#Cleaning step: record number 432. Change "no specification" into a "yes"
ERC_matching <- ERC_matching %>%
  mutate(NospecificationofERC = ifelse(Recordnumber %in% c(432), "yes", NospecificationofERC))

#Inspect table with articles in which no methodology was specified
Tabel_specification <-ERC_matching %>% filter(MatchERC== "yes") %>% 
  select("NospecificationofERC") %>% 
  table()

Tabel_specification
#Cleaning step: Record number 3958 should be a "yes" instead of "N/A"
outlier_NA_afr <- ERC_matching %>% 
  filter(MatchERC == "yes" & trimws(UseofBlackafrican) == "N/A") %>% 
  select(Recordnumber)
ERC_matching$UseofBlackafrican <- trimws(ERC_matching$UseofBlackafrican)
ERC_matching <- ERC_matching %>% 
  mutate(UseofBlackafrican = ifelse(Recordnumber == 3958, "yes", UseofBlackafrican))

#Inspect which labels which are a derivative of African/Black were used
Tabel_lablesused <-ERC_matching %>% filter(MatchERC== "yes") %>% 
  select("UseofBlackafrican") %>% 
  table()
Tabel_lablesused

### Inspect whether a rationale was provided as to why to adjust for ERCs
Exp_given <- ERC_matching %>% filter(MatchERC=="yes")%>%
  select(ExplanationaroundERC) %>%
  table()

Exp_given 

Exp_given_matchyes <- ERC_matching %>% filter(MatchERC=="yes")%>%
  select(ExplanationaroundERC) %>%
  table()

Exp_given_matchyes

#Inspect the frequency of how often an rationale was provided to not adjust for ERCs
Exp_given_matchno <- ERC_matching %>% filter(MatchERC=="no ")%>%
  select(ExplanationaroundERC) %>%
  table()

Exp_given_matchno
#kijken naar alle demografische matchingsmethoden
Conf_adj <- ERC_matching %>% 
  select(MatchERC, MATCHSES, MatchAGE, MatchGENDERSEX)%>%
  data.frame()

Conf_adj

#Examine specific reasons for adjusting for ERCs 
library(dplyr)
library(writexl)

# Cleaning step: Trim whitespace from the "ExplanationaroundERC" column
ERC_matching <- ERC_matching %>%
  mutate(ExplanationaroundERC = trimws(ExplanationaroundERC))

Reasons_for_matching <- ERC_matching %>%
  filter(MatchERC == "yes", ExplanationaroundERC == "yes") %>%
  select(Recordnumber, Whatexplanation)

# Cleaning step
#Record number 207, 536, 3304 en 4628 did not really provide a rationale. Remove.
record_numbers_to_remove <- c(207, 536, 3304, 4628)
Reasons_for_matching <- Reasons_for_matching %>%
  filter(!Recordnumber %in% record_numbers_to_remove)

#Cleaning step: Record number 975, 1461 en 2368 DID provide a rationale
record_numbers_to_add <- c(975, 1461, 2368)
additional_records <- ERC_matching %>%
  filter(Recordnumber %in% record_numbers_to_add) %>%
  select(Recordnumber, Whatexplanation)
Reasons_for_matching <- bind_rows(Reasons_for_matching, additional_records)

#Improved rationale table
print(Reasons_for_matching)

#Examine reasons for NOT adjusting for ERCs
Reasons_for_not_matching <- ERC_matching %>% 
  filter(MatchERC == "no", ExplanationaroundERC == "yes") %>% 
  select(Recordnumber, Whatexplanation)

print(Reasons_for_not_matching)
