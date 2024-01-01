install.packages("xts")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("princomp")
install.packages("m.loadings")
install.packages("eigen")
install.packages("quantmod")
install.packages("read.xl")
install.packages("yhat")
install.packages ("dplyr")
require(dplyr)
require(read.xl)
require(xts)
require(jsonlite)
require(ggplot2)
require(reshape2)
require(princomp)
require(m.loadings)
require(quamtmod)
require(xlsx)
require(yhat)
library(quantmod)
library(zoo)
library(readxl)
if (!require("Rblpapi", quietly = TRUE)) install.packages("Rblpapi")
library(Rblpapi, quietly = TRUE)
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
library(broom)


#=========================================================#
#Load Data
#=========================================================#

GT2year <- read_excel("/Users/Karmen/Documents/magistrska/10year.xlsx", sheet = 4).  #2year

yld   = na.omit(GT2year)
yield = as.matrix(yld[,2:10])
dates = as.matrix(yld[,1])
#dates    = yld[,1] # date


#------------------------------------
#Regress PCA components and data: https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
#-----------------------------------

pca_GL= prcomp(yield, center=TRUE, scale =TRUE , rank = 1)
summary(pca_GL)

#graf z cumulative variance
bond_yield_data = na.omit(GT2year)
bond_yield_subset = as.matrix(bond_yield_data[,2:10]/100)
pca_result <- prcomp(bond_yield_subset, center = TRUE, scale = TRUE)
summary(pca_result)

# Plot the cumulative proportion of variance explained
cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(cumulative_variance, type = "b", xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained")

#correlacija z pc1/globalnim faktorjem
summary(pca_GL$x) # x= the value of each sample in terms of the pca
print(pca_GL$rotation) #rotation is the relationship (correlation/anticorr)between initial variables x and PCA components

rotation_df <- data.frame(Variable = rownames(pca_GL$rotation), PC1 = pca_GL$rotation[, 1])

# Plotting rotation
ggplot(rotation_df, aes(x = Variable, y = PC1, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Original Variable", y = "Correlation with the global factor", fill = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Scale the data before performing PCA
# scaled_yield <- scale(yield)

# Perform PCA using princomp -ta ocitno za vse componente
pcagl <- princomp(scaled_yield, cor = TRUE)
#summary(pcagl)
#summary(pcagl$scores) # x= the value of each sample in terms of the pca
#print(pcagl$loadings) #rotation/loadings is the relationship (correlation/anticorr)between initial variables x and PCA components



# Create a biplot: visualize the relationship between samples and variables in the reduced-dimensional space. 
#The biplot allows you to see how each variable contributes to the principal components and how samples are distributed in this space.
pcaglfig <- biplot(pcagl, cex = 0.5)
#pca_GLfig = biplot(pca_GL, cex = 0.5) (ne dela kr ma sam PC1, mi rabmo vec)


#———————————————————
#Regress data with two PCA components
#———————————————————

#Če me zanima kaj premika 2y DE, moram njo regresirat z 1 PCA (global F)in eurozone factor (EMU obveznice)

#FR2y <- yield[,3]
#components = as.data.frame(cbind(FR2y, pca_GL$x[,1]))

GF = as.data.frame(pca_GL$x[,1])
dates <- as.matrix(yld[,1])
DE2y <- yield[,1]
fit1 <- lm(DE2y~ ., data = GF)
summary(fit1)$adj.r.squared

IT2y <- yield[,2]
fit4 <- lm(IT2y ~., data= GF)
summary(fit4)$adj.r.squared

GB2y <- yield[,3]
fit5 <- lm(GB2y ~., data= GF)
summary(fit5)$adj.r.squared

US2y <- yield[,4]
fit6 <- lm(US2y ~., data= GF)
summary(fit6)$adj.r.squared

JP2y <- yield[,5]
fit7 <- lm(JP2y ~., data= GF)
summary(fit7)$adj.r.squared

CA2y <- yield[,6]
fit8 <- lm(CA2y ~., data= GF)
summary(fit8)$adj.r.squared

AU2y <- yield[,7]
fit9 <- lm(AU2y ~., data= GF)
summary(fit9)$adj.r.squared

CH2y <- yield[,8]
fit10 <- lm(CH2y ~., data= GF)
summary(fit10)$adj.r.squared

SE2y <- yield[,9]
fit11 <- lm(SE2y ~., data= GF)
summary(fit11)$adj.r.squared


#Construct series that cant be explained by the PCA1 (global Factor)
resDE = fit1$residuals
resIT = fit4$residuals
resGB = fit5$residuals
resUS = fit6$residuals
resJP = fit7$residuals
resCA = fit8$residuals
resAU = fit9$residuals
resCH = fit10$residuals
resSE = fit11$residuals

# Convert dates to Date format
dates <- as.Date(dates, format = "%d.%m.%Y")

# Create a data frame for residuals
residuals_data <- data.frame(
  date = as.Date(dates),
  DE = resDE,
  IT = resIT,
  GB = resGB,
  US = resUS,
  JP = resJP,
  CA = resCA,
  AU = resAU,
  CH = resCH,
  SE = resSE
  
)

# Line chart of residuals for each country
ggplot(residuals_data, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = CH, color = "CH")) +
  geom_line(aes(y = SE, color = "SE")) +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = GB, color = "GB")) +
  geom_line(aes(y = US, color = "US")) +
  geom_line(aes(y = JP, color = "JP")) +
  geom_line(aes(y = CA, color = "CA")) +
  geom_line(aes(y = AU, color = "AU")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "CH" = "green", "SE" = "red", "IT" = "purple", "GB" = "orange", "US" = "pink", "JP" = "lightblue", "CA" = "darkgreen","AU" = "brown"))

#Eurofaktor

Eurofactor = data.frame(resSE, resDE)
as.matrix(Eurofactor)

#Euro factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_EU = prcomp(Eurofactor, center=TRUE, scale.=T, rank. = 1)
EU     = as.data.frame(PCA_EU$x[,1])
summary(PCA_EU)
components = as.data.frame(cbind(GF, EU))

fit12 <- lm(DE2y~.,data= components)
summary(fit12)$adj.r.squared

fit42 <- lm(IT2y~.,data= components)
summary(fit42)$adj.r.squared

fit52 <- lm(GB2y~.,data= components)
summary(fit52)$adj.r.squared

fit62 <- lm(US2y~.,data= components)
summary(fit62)$adj.r.squared

fit72 <- lm(JP2y~.,data= components)
summary(fit72)$adj.r.squared

fit82 <- lm(CA2y~.,data= components)
summary(fit82)$adj.r.squared

fit92 <- lm(AU2y~.,data= components)
summary(fit92)$adj.r.squared

fit102 <- lm(CH2y ~., data= components)
summary(fit102)$adj.r.squared

fit112 <- lm(SE2y ~., data= components)
summary(fit112)$adj.r.squared


#Construct series that cant be explained by the PCA1 (global Factor)
resDE2 = fit12$residuals
resIT2 = fit42$residuals
resGB2 = fit52$residuals
resUS2 = fit62$residuals
resJP2 = fit72$residuals
resCA2 = fit82$residuals
resAU2 = fit92$residuals
resCH2 = fit102$residuals
resSE2 = fit112$residuals

# Create a data frame for residuals
residuals_data2 <- data.frame(
  date = as.Date(dates),
  DE = resDE2,
  SE = resFR2,
  US = resUS2
)

# Line chart of residuals for each country
ggplot(residuals_data2, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = SE, color = "SE")) +
  geom_line(aes(y = US, color = "US")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "SE" = "green", "US" = "red"))



#Anglo-Saxon faktor

USfactor = data.frame(resUS, resCA)
as.matrix(USfactor)

#US factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_US = prcomp(USfactor, center=TRUE, scale.=T, rank. = 1)
US     = as.data.frame(PCA_US$x[,1])
summary(PCA_US)
components = as.data.frame(cbind(GF, EU, US, resJP, resCH))

fit123 <- lm(DE2y~.,data= components)
summary(fit123)$adj.r.squared

fit423 <- lm(IT2y~.,data= components)
summary(fit423)$adj.r.squared

fit523 <- lm(GB2y~.,data= components)
summary(fit523)$adj.r.squared

fit623 <- lm(US2y~.,data= components)
summary(fit623)$adj.r.squared

fit723 <- lm(JP2y~.,data= components)
summary(fit723)$adj.r.squared

fit823 <- lm(CA2y~.,data= components)
summary(fit823)$adj.r.squared

fit923 <- lm(AU2y~.,data= components)
summary(fit923)$adj.r.squared

fit1023 <- lm(CH2y ~., data= components)
summary(fit1023)$adj.r.squared

fit1123 <- lm(SE2y ~., data= components)
summary(fit1123)$adj.r.squared



#Construct series that cant be explained by the PCA1 (global Factor)
resDE3 = fit123$residuals
resIT3 = fit423$residuals
resGB3 = fit523$residuals
resUS3 = fit623$residuals
resJP3 = fit723$residuals
resCA3 = fit823$residuals
resAU3 = fit923$residuals
resCH3 = fit1023$residuals
resSE3 = fit1123$residuals


# Create a data frame for residuals
residuals_data <- data.frame(
  date = as.Date(dates),
  DE = resDE3,
  IT = resIT3,
  GB = resGB3,
  US = resUS3,
  JP = resJP3,
  CA = resCA3,
  AU = resAU3,
  CH = resCH3,
  SE = resSE3
  
)

# Line chart of residuals for each country
ggplot(residuals_data, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = CH, color = "CH")) +
  geom_line(aes(y = SE, color = "SE")) +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = GB, color = "GB")) +
  geom_line(aes(y = US, color = "US")) +
  geom_line(aes(y = JP, color = "JP")) +
  geom_line(aes(y = CA, color = "CA")) +
  geom_line(aes(y = AU, color = "AU")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "CH" = "green", "SE" = "red", "IT" = "purple", "GB" = "orange", "US" = "pink", "JP" = "lightblue", "CA" = "darkgreen","AU" = "brown"))




#DE dekompozicija 1
coefficients <- coef(summary(fit123))
average_yield <- mean(DE2y)

contributions <- data.frame(
  predictor = rownames(coefficients),
  Factor_contribution = coefficients[, 1]
)

In_coef <- contributions[contributions$predictor == "(Intercept)", "Factor_contribution"]
GF_coef <- contributions[contributions$predictor == "`pca_GL$x[, 1]`", "Factor_contribution"]
US_coef <- contributions[contributions$predictor == "`PCA_US$x[, 1]`", "Factor_contribution"]
EU_coef <- contributions[contributions$predictor == "`PCA_EU$x[, 1]`", "Factor_contribution"]
JP_coef <- contributions[contributions$predictor == "resJP", "Factor_contribution"]
CH_coef <- contributions[contributions$predictor == "resCH", "Factor_contribution"]

GF_comp <- components[1]
US_comp <- components[3]
EU_comp <- components[2]
JP_comp <- components[4]
CH_comp <- components[5]

contribution_GF <- GF_coef * GF_comp
contribution_EU <- EU_coef * EU_comp
contribution_US <- US_coef * US_comp
contribution_JP <- JP_coef * JP_comp
contribution_CH <- CH_coef * CH_comp

augmented_data3 <- augment(fit123)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = DE2y,
                               GF = In_coef + contribution_GF[, 1],
                               EU = contribution_EU[, 1],
                               US = contribution_US[, 1],
                               JP = contribution_JP[, 1],
                               CH = contribution_CH[, 1],
                               idiosyncratic = augmented_data3$.resid)


#graf z ribbonom namest area

ggplot(contributions_df, aes(x = date, y = yield)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = GF, fill = "GF"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = EU, fill = "EU"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = US, fill = "US"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = JP, fill = "JP"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = CH, fill = "CH"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = idiosyncratic, fill = "Idio"), alpha = 0.5) +
  # Add more geom_ribbon layers for additional factors
  labs(title = "Global-European Factor Decomposition of DE 10Y Sovereign Nominal Yields",
       x = "date",
       y = "yield") +
  scale_fill_manual(values = c("GF" = "blue", "EU" = "green", "US" = "pink", "JP" = "lightblue", "CH" = "darkgreen", "Idio" = "orange")) +
  theme_minimal()



#dekompozicija z area in yieldom in vrstnim redom

# Reshape the data frame to long format
contributions_long <- tidyr::gather(contributions_df, key = "variable", value = "contribution", GF, EU, CH, US, JP, idiosyncratic)

# Add the Date information
contributions_long$Date <- dates

# Define the desired order of predictors
desired_order <- c("idiosyncratic", "JP", "US", "CH", "EU", "GF")

# Convert the "variable" to a factor with the desired order
contributions_long$variable <- factor(contributions_long$variable, levels = desired_order)


ggplot(contributions_long, aes(x = Date, y = contribution, fill = variable)) +
  geom_area() +
  geom_line(aes(x = Date, y = yield, color = "Yield"), size = 0.1, linetype = "solid") +  # Add line for yield
  labs(title = "Contributions to Prediction",
       x = "Date",
       y = "Contribution to Prediction",
       fill = "Predictor Variable") +
  scale_fill_manual(values = c("US" = "pink", "GF" = "blue", "EU" = "green", "JP" = "yellow", "CH" = "darkgreen", "idiosyncratic" = "orange")) +
  scale_color_manual(values = c("Yield" = "black")) +  # Adjust line color
  theme_minimal()




#US dekompozicija 1
coefficients <- coef(summary(fit623))

contributions <- data.frame(
  predictor = rownames(coefficients),
  Factor_contribution = coefficients[, 1]
)

In_coef <- contributions[contributions$predictor == "(Intercept)", "Factor_contribution"]
GF_coef <- contributions[contributions$predictor == "`pca_GL$x[, 1]`", "Factor_contribution"]
US_coef <- contributions[contributions$predictor == "`PCA_US$x[, 1]`", "Factor_contribution"]
EU_coef <- contributions[contributions$predictor == "`PCA_EU$x[, 1]`", "Factor_contribution"]
JP_coef <- contributions[contributions$predictor == "resJP", "Factor_contribution"]
CH_coef <- contributions[contributions$predictor == "resCH", "Factor_contribution"]

GF_comp <- components[1]
US_comp <- components[3]
EU_comp <- components[2]
JP_comp <- components[4]
CH_comp <- components[5]

contribution_GF <- GF_coef * GF_comp
contribution_EU <- EU_coef * EU_comp
contribution_US <- US_coef * US_comp
contribution_JP <- JP_coef * JP_comp
contribution_CH <- CH_coef * CH_comp

augmented_data3 <- augment(fit623)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = US2y,
                               GF = In_coef + contribution_GF[, 1],
                               EU = contribution_EU[, 1],
                               US = contribution_US[, 1],
                               JP = contribution_JP[, 1],
                               CH = contribution_CH[, 1],
                               idiosyncratic = augmented_data3$.resid)


#dekompozicija z area in yieldom in vrstnim redom

# Reshape the data frame to long format
contributions_long <- tidyr::gather(contributions_df, key = "variable", value = "contribution", GF, EU, CH, US, JP, idiosyncratic)

# Add the Date information
contributions_long$Date <- dates

# Define the desired order of predictors
desired_order <- c("idiosyncratic", "JP", "US", "CH", "EU", "GF")

# Convert the "variable" to a factor with the desired order
contributions_long$variable <- factor(contributions_long$variable, levels = desired_order)


ggplot(contributions_long, aes(x = Date, y = contribution, fill = variable)) +
  geom_area() +
  geom_line(aes(x = Date, y = yield, color = "Yield"), size = 0.5, linetype = "solid") +  # Add line for yield
  labs(title = "Contributions to Prediction",
       x = "Date",
       y = "Contribution to Prediction",
       fill = "Predictor Variable") +
  scale_fill_manual(values = c("US" = "pink", "GF" = "blue", "EU" = "green", "JP" = "yellow", "CH" = "darkgreen", "idiosyncratic" = "orange")) +
  scale_color_manual(values = c("Yield" = "black")) +  # Adjust line color
  theme_minimal()


