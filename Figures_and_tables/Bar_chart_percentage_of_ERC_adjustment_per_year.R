# Calculate the percentage of "yes" (matched ERC) out of the total count per year
erc_counts_per_year <- ERC_matching %>%
  group_by(Year) %>%
  summarize(total_count = n(), 
            yes_count = sum(MatchERC == "yes")) %>%
  ungroup() %>%
  mutate(percentage = (yes_count / total_count) * 100)

# Create the bar plot
ggplot(erc_counts_per_year, aes(x = Year, y = percentage, fill = "Matched ERC")) +
  geom_bar(stat = "identity") +
  labs(
    title = "Percentage of ERC-Adjusted Articles per Year",
    x = "Year",
    y = "Percentage of ERC-adjusted articles(%)"
  ) +
  scale_fill_manual(values = c("ERC-adjusted articles" = "#2E8B57")) +
  theme_minimal()
