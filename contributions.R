# Assuming 'yield' is a data frame with columns for yields of different countries
# and 'GF' is the global factor PCA result



# Create a data frame for factor contributions
contributions <- data.frame(
  country = c("DE", "FR", "ES", "IT", "GB"),  # Adjust country codes accordingly
  variable = rep(c("GF", "EU"), times = 5),
  contribution = NA
)

# Loop through each country and perform regression
for (i in 1:5) {
  country_code <- contributions$country[i]
  
  # Create a data frame for regression
  regression_data <- data.frame(
    GF = GF$score[, 1],  # Assuming 'GF' is the result of global factor PCA
    EU = GF$score[, 2],  # Assuming 'GF' is the result of global factor PCA
    yield = yield[, i + 1]  # Assuming the first column is country names, adjust accordingly
  )
  
  # Fit the regression model
  model <- lm(yield ~ GF + EU, data = regression_data)
  
  # Extract contributions
  contributions[contributions$country == country_code, "contribution"] <- residuals(model)
}

# Create a long-format data frame
contributions_long <- melt(contributions, id.vars = c("country", "variable"), value.name = "contribution")

# Plot the contributions
ggplot(contributions_long, aes(x = variable, y = contribution, fill = variable)) +
  geom_col() +
  facet_wrap(~country, scales = "free") +
  labs(x = "Factor", y = "Contribution to Yield", fill = "Factor")
