#Create tables for adjustment on age, sex/gender, ERC, SES and other covariates in SCD research
columns_to_analyze <- c("MatchAGE", "MatchGENDERSEX", "MATCHSES", "MatchERC", "MatchOther")

tables_by_column <- lapply(columns_to_analyze, function(column) {
  ERC_matching %>%
    group_by(Geographical_region) %>%
    summarise(
      total_count = n(),
      yes_count = sum(.data[[column]] == "yes"),
      no_count = sum(.data[[column]] == "no ")
    ) %>%
    mutate(
      yes_percentage = round(yes_count / total_count * 100),
      no_percentage = round(no_count / total_count * 100)
    ) %>% 
    arrange(desc(yes_percentage)) %>%  # Arrange by descending yes_percentage
    as.data.frame()
   })

# Print the tables for each column
for (i in seq_along(columns_to_analyze)) {
  cat("Table for", columns_to_analyze[i], ":\n")
  print(tables_by_column[[i]])
  cat("\n")
}

#chisquare MatchSES compare to MatchERC for Sub-Saharan Africa
# Subset the dataframe
subset_data_SSA <- ERC_matching[ERC_matching$Geographical_region == "SSA", ]
# Create a contingency table with counts of "yes" in MATCHSES and MatchERC
contingency_table <- table(subset_data_SSA$MATCHSES, subset_data_SSA$MatchERC)
# Label rows and columns
rownames(contingency_table) <- c("MATCHSES_No", "MATCHSES_Yes")
colnames(contingency_table) <- c("MatchERC_No", "MatchERC_Yes")
# Perform chi-square test
result <- chisq.test(contingency_table)
# Print the result
print(result)

#Create mulilinear model with SES adjustment, adjustment for other confounders and asses it's influence on ERC-adjustment
library("lmerTest")
library("MuMIn")

# Remove leading and trailing whitespace from ERC_matching$MatchERC en MATCHSES en MatchOther
ERC_matching$MatchERC <- trimws(ERC_matching$MatchERC)
ERC_matching$MATCHSES <- trimws(ERC_matching$MATCHSES)
ERC_matching$MatchOther <- trimws(ERC_matching$MatchOther)
ERC_matching$Discipline1 <- trimws(ERC_matching$Discipline1)


#Changes yes to 1 and no to 0 for use in the model
ERC_matching$ERCMatch2[ERC_matching$MatchERC == "yes"] = 1
ERC_matching$ERCMatch2[ERC_matching$MatchERC == "no"] = 0

ERC_matching$MATCHSES2[ERC_matching$MATCHSES == "yes"] = 1
ERC_matching$MATCHSES2[ERC_matching$MATCHSES == "no"] = 0

ERC_matching$MatchOther2[ERC_matching$MatchOther == "yes"] = 1
ERC_matching$MatchOther2[ERC_matching$MatchOther == "no"] = 0

ERC_matching$ERCMatch2 <- as.factor(ERC_matching$ERCMatch2)
ERC_matching$MATCHSES2 <- as.factor(ERC_matching$MATCHSES2)
ERC_matching$MatchOther2 <-as.factor(ERC_matching$MatchOther2)
ERC_matching$Email <- as.factor(ERC_matching$Email)
## model wat invloed heeft op matching van ethnicity and race proberen
## met corresponding author, nu met een andere package. Journal neem
## ik niet mee als random intercept. discipline wordt al meegenomen

control <- glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
fit <- glmer(ERCMatch2 ~ MATCHSES2 + MatchOther + Geographical_region + (1|Discipline1),
                       data=ERC_matching, family=binomial, control=control)
fit.null <- glmer(ERCMatch2 ~ 1 + (1|Discipline1),
                       data=ERC_matching, family=binomial, control=control)
summary(fit)  ## deze
exp(coeffs(fit))
exp(confint.merMod(fit, method="Wald"))

r.squaredGLMM(object=fit, null=fit.null)

print(packageVersion('lme4'))


