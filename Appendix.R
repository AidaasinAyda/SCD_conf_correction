# Create a table with counts for each unique variable in the specified columns
country_counts <- table(ERC_matching$Country1)
age_counts <- table(ERC_matching$MatchAGE)
gendersex_counts <- table(ERC_matching$MatchGENDERSEX)
ses_counts <- table(ERC_matching$MATCHSES)
other_counts <- table(ERC_matching$MatchOther)
erc_counts <- table(ERC_matching$MatchERC)
explanation_erc_counts <- table(ERC_matching$ExplanationaroundERC)
use_black_african_counts <- table(ERC_matching$UseofBlackafrican)
methodology_erc_counts <- table(ERC_matching$MethodologydeterminingERCmentioned)
mixed_individuals_counts <- table(ERC_matching$MixedindividualsconsideredasanERC)
w_nw_counts <- table(ERC_matching$W_NW)
citescore_counts <- table(ERC_matching$Quartiles_citescore)

# View the counts for each column
print(country_counts)
print(age_counts)
print(gendersex_counts)
print(ses_counts)
print(other_counts)
print(erc_counts)
print(explanation_erc_counts)
print(use_black_african_counts)
print(methodology_erc_counts)
print(mixed_individuals_counts)
print(w_nw_counts)
print(citescore_counts)



#Export to Excel
library(writexl)

# Convert the tables to data frames
country_counts_df <- as.data.frame(country_counts)
age_counts_df <- as.data.frame(age_counts)
gendersex_counts_df <- as.data.frame(gendersex_counts)
ses_counts_df <- as.data.frame(ses_counts)
other_counts_df <- as.data.frame(other_counts)
erc_counts_df <- as.data.frame(erc_counts)
explanation_erc_counts_df <- as.data.frame(explanation_erc_counts)
use_black_african_counts_df <- as.data.frame(use_black_african_counts)
methodology_erc_counts_df <- as.data.frame(methodology_erc_counts)
mixed_individuals_counts_df <- as.data.frame(mixed_individuals_counts)
w_nw_counts_df <- as.data.frame(w_nw_counts)
citescore_counts_df <- as.data.frame(citescore_counts)

# Create a list of data frames
tables_list <- list(
  country_counts_df,
  age_counts_df,
  gendersex_counts_df,
  ses_counts_df,
  other_counts_df,
  erc_counts_df,
  explanation_erc_counts_df,
  use_black_african_counts_df,
  methodology_erc_counts_df,
  mixed_individuals_counts_df,
  w_nw_counts_df,
  citescore_counts_df
)

# Create a list of sheet names
sheet_names <- c(
  "Country Counts",
  "Age Counts",
  "Gender/Sex Counts",
  "SES Counts",
  "Other Counts",
  "ERC Counts",
  "Explanation around ERC Counts",
  "Use of Black African Counts",
  "Methodology for determining ERC mentioned Counts",
  "Mixed Individuals Considered as an ERC Counts",
  "W_NW Counts",
  "CiteScore Counts"
)

# Create an Excel workbook and add each data frame to a separate sheet
write_xlsx(tables_list, path = "Table_for_appendix_ERC_matchingdd16-10-2023.xlsx")

