library(dplyr)
library(ggplot2)
# Calculate the percentage of "yes" (matched ERC) out of the total count per year
erc_counts_per_year <- ERC_matching %>%
  group_by(Year) %>%
  summarize(total_count = n(), 
            yes_count = sum(MatchERC == "yes")) %>%
  ungroup() %>%
  mutate(percentage = (yes_count / total_count) * 100)

# Create the bar plot

ggplot(erc_counts_per_year, aes(x = Year, y = percentage, fill = "Matched ERC")) +
  geom_bar(stat = "identity", fill = "#4169E1") +  # Change bar color to blue
  labs(
    x = "Year",
    y = "Percentage of ERC-adjusted articles (%)"
  ) +
  scale_fill_manual(values = c("Matched ERC" = "#4169E1")) +  # Adjust fill color
  theme_minimal() +
  theme(
    plot.title = element_blank(),  # Remove the title
    panel.border = element_blank()  # Remove the box outline
  )
