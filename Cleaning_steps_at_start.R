# Read in an sav file (replace "path_to_your_file.sav" with the actual file path)
library(haven)
ERC_matching <- read_sav("ERC_matching_domestic_studies_dd_05-05-2023.sav")

# Remove leading and trailing whitespace from specific columns
ERC_matching$MatchERC <- trimws(ERC_matching$MatchERC)
ERC_matching$MATCHSES <- trimws(ERC_matching$MATCHSES)
ERC_matching$MatchOther <- trimws(ERC_matching$MatchOther)
ERC_matching$MatchAGE <- trimws(ERC_matching$MatchAGE)
ERC_matching$MatchGENDERSEX <- trimws(ERC_matching$MatchGENDERSEX)
ERC_matching$Geographical_region <- trimws(ERC_matching$Geographical_region)
