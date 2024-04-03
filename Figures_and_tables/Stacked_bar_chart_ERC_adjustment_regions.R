#create a bar chart for matching on age gender and sex in proportions per geographical region
library(dplyr)
library(ggplot2)
library(tidyr)
library(foreign)
library(forcats)
library(ghibli)

# Calculate counts for each category, stratified by geographical region
count_table <- ERC_matching %>%
  group_by(Geographical_region) %>%
  summarize(
    Age_Yes = sum(MatchAGE == "yes"),
    Age_No = sum(MatchAGE == "no"),
    ERC_Yes = sum(MatchERC == "yes"),
    ERC_No = sum(MatchERC == "no"),
    GenderSex_Yes = sum(MatchGENDERSEX == "yes"),
    GenderSex_No = sum(MatchGENDERSEX == "no"),
    MatchOther_Yes = sum(MatchOther == "yes"),
    MatchOther_No = sum(MatchOther == "no"),
    SES_Yes = sum(MATCHSES == "yes"),
    SES_No = sum(MATCHSES == "no")
  ) %>%
  ungroup()

count_table <- count_table %>%
  mutate(Geographical_region = recode(Geographical_region, "E" = "EUR", "CA" = "CAR"))

count_table$Geographical_region <- factor(count_table$Geographical_region)

count_table

# Filter only the "yes" counts
yes_counts <- count_table %>%
  select(Geographical_region, Age_Yes, ERC_Yes, GenderSex_Yes, MatchOther_Yes, SES_Yes)

# Reshape the data for plotting
count_table_long <- yes_counts %>%
  pivot_longer(
    cols = -Geographical_region,
    names_to = "Category",
    values_to = "Count"
  ) %>%
  mutate(
    Category = factor(Category, levels = c("Age_Yes", "ERC_Yes", "GenderSex_Yes", "MatchOther_Yes", "SES_Yes"))
  )


# Reorder the levels of the "Category" factor variable to place "ERC_Yes" at the bottom
count_table_long$Category <- factor(count_table_long$Category, levels = c("Age_Yes", "GenderSex_Yes", "MatchOther_Yes", "SES_Yes", "ERC_Yes"))

count_table_long$Category <- recode(count_table_long$Category, 
                                    "Age_Yes" = "Age",
                                    "GenderSex_Yes" = "Gender/sex",
                                    "ERC_Yes" = "ERC",
                                    "MatchOther_Yes" = "Other covariates",
                                    "SES_Yes" = "SES"
                                    
)

# Calculate the total count of "ERC_Yes" for each geographical region
erc_region_totals <- count_table %>%
  select(Geographical_region, ERC_Yes) %>%
  arrange(desc(ERC_Yes))

# Reorder the levels of the "Geographical_region" factor variable based on the total "ERC_Yes" count
count_table_long$Geographical_region <- factor(count_table_long$Geographical_region, levels = erc_region_totals$Geographical_region)

# Define the "MononokeMedium" palette with direction -1 to get the lightest colors first
mononokemedium_palette <- ghibli_palette("MononokeMedium", n = 7, direction = -1, type = "discrete")

# Define a custom palette starting with the lightest colors
custom_palette <- mononokemedium_palette

# Create the stacked bar chart with the custom color palette
bar_chart <- ggplot(count_table_long, aes(x = Geographical_region, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Geographical region", y = "No. of operationalized confounders per geographical region", fill = "Category adjusted for") +
  scale_fill_manual(values = custom_palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar_chart






