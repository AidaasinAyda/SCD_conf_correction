library(dplyr)
library(tidyr)
library(epiR)

# Trim whitespace in 'MatchERC' column
ERC_matching$MatchERC <- trimws(ERC_matching$MatchERC)

# Create a data frame for matching per geographical region
correction_df <- ERC_matching %>%
  group_by(Geographical_region, MatchERC) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = MatchERC, values_from = count, values_fill = 0) %>%
  ungroup()

# Reorder columns
correction_df <- correction_df[, c('Geographical_region', 'yes', 'no')]

# Print the resulting data frame
print(correction_df)

#Chi-square test ERC confounder adjustment per geographical region
geograph_ERCmatch_tbl <- xtabs (~ Geographical_region + MatchERC,data=ERC_matching)
print(geograph_ERCmatch_tbl)
chisq.test(geograph_ERCmatch_tbl)

#Odds ratio ERC adjustment Western vs. Non-Western regions
OR_Western_ERCmatch <- epi.2by2(Western_ERCmatch_tbl, method="case.control", conf.level=0.95)
print(OR_Western_ERCmatch)

#Odds ratio Q1-published journals vs. non-Q1 published journals for ERC confounder adjustment
# Trim white spaces in the 'Q1_yesno' column
ERC_matching$Q1_yesno <- trimws(ERC_matching$Q1_yesno)
# Replace "N/A" with "no" in the 'Q1_yesno' column. We assume that the not-listed journal quartiles in the datafram cannot be Q1
ERC_matching$Q1_yesno[ERC_matching$Q1_yesno == "N/A"] <- "no"
# Create contingency table with modified data
Q1_yesno_MatchERC <- xtabs(~Q1_yesno + MatchERC, data=ERC_matching)
#analyse Odds ratio
result <- epi.2by2(Q1_yesno_MatchERC)
print(result)
