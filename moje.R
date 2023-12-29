# Install and load necessary packages (if not already installed)
# install.packages("tidyquant")
# install.packages("ggplot2")

library(tidyquant)
library(ggplot2)
library(readxl)

# Load your daily bond yield data (replace 'your_dataset.csv' with your actual dataset)
# The dataset should have columns for Date, Germany, Italy, Spain, and Britain bond yields
GT2year <- read_excel("C:/Users/karzup7386/Desktop/mesec 2/PCA/datadaily.xlsx", sheet = 2)

bond_yield_data = na.omit(GT2year)
bond_yield_subset = as.matrix(bond_yield_data[,2:6]/100)

# Assuming your dataset looks like this:
# Date       Germany Italy  Spain   Britain
# 2023-01-01  0.02    0.03   0.04    0.01
# ...

# Select the relevant columns for analysis
#bond_yield_subset <- bond_yield_data[, c("Germany", "Italy", "Spain", "Britain")]

# Perform PCA
pca_result <- prcomp(bond_yield_subset, center = TRUE, scale = TRUE)

# Explore the results
summary(pca_result)

# Plot the cumulative proportion of variance explained
cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(cumulative_variance, type = "b", xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained")

# Choose the number of principal components to retain based on the plot

# Visualize individual contributions
contributions <- pca_result$rotation
rownames(contributions) <- colnames(bond_yield_subset)

# Access the loadings (correlations between original variables and principal components) ISTUU
loadings <- pca_result$rotation

# Convert the matrix to a data frame
contributions_df <- as.data.frame(contributions)

# Plot individual contributions
ggplot(data = contributions_df, aes(x = rownames(contributions_df), y = PC1)) +
  geom_bar(stat = "identity") +
  labs(title = "Individual Contributions to PC1")


