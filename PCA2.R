#——————————————————–#
# PCA three factors of bond yields
#========================================================#
#setwd("//optnt/optshr/6-tedensko porocilo/Odbor za denpol/PCA")
#=========================================================#
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
install.packages("dplyr")
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

#file_path <- "C:/Users/karzup7386/Desktop/mesec 2/PCA/data1.xlsx"
#data3 <- read_excel(file=file_path, sheetIndex = 1, header= T, startRow = 6 )

GTDEM <- read_excel("C:/Users/karzup7386/Desktop/mesec 2/PCA/data1.xlsx", sheet = 2)

# Connect to Bloomberg
 con <- blpConnect()

opt= c(predicitySelection="DAILY")
DE3M=bdh("GTDEM3M Govt","YLD_YTM_MID","01.01.2000","24.01.2023", options=opt)

podatki_DE3M <- bdh("GTDEM3M Govt",
     c("YLD_YTM_MID"),
     start.date = as.Date("2000-01-01"),
     end.date = as.Date("2023-01-24"))

#graphics.off(); rm(list = ls())

# graph function

#=========================================================#
#Load Data
#=========================================================#

GT2year <- read_excel("C:/Users/karzup7386/Desktop/mesec 2/PCA/datadaily2.xlsx", sheet = 2)

yld   = na.omit(GT2year)
yield = as.matrix(yld[,2:6])
dates = as.matrix(yld[,1])
#dates    = yld[,1] # date
mat   = c(3,12,60,120,180,240,360) #Months

# f_draw_factor_loading = function(m.loadings, var, title){
#   x11(width=16/3,height=5);
#   matplot(mat,m.loadings,type="l",xaxt="n",lwd=5,lty=1, 
#           main=title,col=rainbow(3),ylim=c(-0.6, 0.6),
#           xlab= "Maturity",ylab = "Factor loadings")
#   legend("bottomright",max(m.loadings),col=rainbow(3),lty=1,lwd=3,
#          legend=paste0(c("1st PC-","2nd PC-","3rd PC-"),var))
#   axis(1,mat,mat)
#}


#------------------------------------
#Regress PCA components and data: https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
#-----------------------------------

pca_GL= prcomp(yield, center=TRUE, scale =TRUE , rank = 1)
summary(pca_GL)

#graf z cumulative variance
bond_yield_data = na.omit(GT2year)
bond_yield_subset = as.matrix(bond_yield_data[,2:6]/100)
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

#Èe me zanima kaj premika 2y DE, moram njo regresirat z 1 PCA (global F)in eurozone factor (EMU obveznice)

#FR2y <- yield[,3]
#components = as.data.frame(cbind(FR2y, pca_GL$x[,1]))

GF = as.data.frame(pca_GL$x[,1])
dates <- as.matrix(yld[,1])
DE2y <- yield[,1]
fit1 <- lm(DE2y~ ., data = GF)
summary(fit1)$adj.r.squared

FR2y <- yield[,2]
fit2 <- lm(FR2y ~., data= GF)
summary(fit2)$adj.r.squared

ES2y <- yield[,3]
fit3 <- lm(ES2y ~., data= GF)
summary(fit3)$adj.r.squared

IT2y <- yield[,4]
fit4 <- lm(IT2y ~., data= GF)
summary(fit4)$adj.r.squared

GB2y <- yield[,5]
fit5 <- lm(GB2y ~., data= GF)
summary(fit5)$adj.r.squared



#Construct series that cant be explained by the PCA1 (global Factor)
resDE = fit1$residuals
resFR = fit2$residuals
resES = fit3$residuals
resIT = fit4$residuals
resGB = fit5$residuals

# Create a data frame for residuals
residuals_data <- data.frame(
  date = as.Date(dates),
  DE = resDE,
  FR = resFR,
  ES = resES,
  IT = resIT,
  GB = resGB
)

# Line chart of residuals for each country
ggplot(residuals_data, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = FR, color = "FR")) +
  geom_line(aes(y = ES, color = "ES")) +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = GB, color = "GB")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "FR" = "green", "ES" = "red", "IT" = "purple", "GB" = "orange"))

#Eurofaktor

Eurofactor = data.frame(resFR, resES, resDE)
as.matrix(Eurofactor)

#Euro factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_EU = prcomp(Eurofactor, center=TRUE, scale.=T, rank. = 1)
EU     = as.data.frame(PCA_EU$x[,1])
summary(PCA_EU)
components = as.data.frame(cbind(GF, EU))

fit12 <- lm(DE2y~.,data= components)
summary(fit12)$adj.r.squared

fit22 <- lm(FR2y~.,data= components)
summary(fit22)$adj.r.squared

fit32 <- lm(ES2y~.,data= components)
summary(fit32)$adj.r.squared

fit42 <- lm(IT2y~.,data= components)
summary(fit42)$adj.r.squared

fit52 <- lm(GB2y~.,data= components)
summary(fit52)$adj.r.squared

#Construct series that cant be explained by the PCA1 (global Factor)
resDE2 = fit12$residuals
resFR2 = fit22$residuals
resES2 = fit32$residuals
resIT2 = fit42$residuals
resGB2 = fit52$residuals


# Create a data frame for residuals
residuals_data2 <- data.frame(
  date = as.Date(dates),
  DE = resDE2,
  FR = resFR2,
  ES = resES2
)

# Line chart of residuals for each country
ggplot(residuals_data2, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = FR, color = "FR")) +
  geom_line(aes(y = ES, color = "ES")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "FR" = "green", "ES" = "red"))

# Create a data frame for residuals
residuals_data3 <- data.frame(
  date = as.Date(dates),
  DE = resDE2,
  FR = resFR2,
  ES = resES2,
  IT = resIT2,
  GB = resGB2
)

# Line chart of residuals for each country
ggplot(residuals_data3, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = FR, color = "FR")) +
  geom_line(aes(y = ES, color = "ES")) +
  geom_line(aes(y = GB, color = "GB")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "FR" = "green", "ES" = "red", "GB" = "orange"))


#Calculate the factor contribution for each predictor
# Extract coefficients and create a data frame

coefficients_table <- coef(summary(fit12))
contributions <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)

print(contributions)

# Sort the factor contributions in descending order
factor_contributions = contributions[order(-contributions$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to DE2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "navyblue", high = "steelblue")

coefficients_table <- coef(summary(fit12))
contributions <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)


coefficients_table <- coef(summary(fit22))
contributions2 <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)
print(contributions2)

# Sort the factor contributions in descending order
factor_contributions = contributions2[order(-contributions2$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions2, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to FR2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "darkgreen", high = "green") 


coefficients_table <- coef(summary(fit32))
contributions3 <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)
print(contributions3)

# Sort the factor contributions in descending order
factor_contributions = contributions3[order(-contributions3$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions3, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to ES2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "darkred", high = "red") 


coefficients_table <- coef(summary(fit42))
contributions4 <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)
print(contributions4)

# Sort the factor contributions in descending order
factor_contributions = contributions4[order(-contributions4$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions4, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to IT2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "purple", high = "pink") 


coefficients_table <- coef(summary(fit52))
contributions5 <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)
print(contributions5)

# Sort the factor contributions in descending order
factor_contributions = contributions5[order(-contributions5$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions5, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to GB2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "brown", high = "orange") 



#IT faktor

ITfactor = data.frame(resIT)
ITfactor = as.matrix(ITfactor)

#Euro factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_IT = prcomp(ITfactor, center=TRUE, scale.=T, rank. = 1)
IT     = as.data.frame(PCA_IT$x[,1])

GBfactor = data.frame(resGB)
GBfactor = as.matrix(GBfactor)

PCA_GB = prcomp(GBfactor, center=TRUE, scale.=T, rank. = 1)
GB     = as.data.frame(PCA_GB$x[,1])

summary(PCA_GB)
components = as.data.frame(cbind(GF, EU, IT, GB))

fit12 <- lm(DE2y~.,data= components)
summary(fit12)$adj.r.squared

fit22 <- lm(FR2y~.,data= components)
summary(fit22)$adj.r.squared

fit32 <- lm(ES2y~.,data= components)
summary(fit32)$adj.r.squared

fit42 <- lm(IT2y~.,data= components)
summary(fit42)$adj.r.squared

fit52 <- lm(GB2y~.,data= components)
summary(fit52)$adj.r.squared

#Construct series that cant be explained by the PCA1 (global Factor)
resDE2 = fit12$residuals
resFR2 = fit22$residuals
resES2 = fit32$residuals
resIT2 = fit42$residuals
resGB2 = fit52$residuals


# Create a data frame for residuals
residuals_data3 <- data.frame(
  date = as.Date(dates),
  DE = resDE2,
  FR = resFR2,
  ES = resES2,
  IT = resIT2,
  GB = resGB2
)

# Line chart of residuals for each country
ggplot(residuals_data3, aes(x = date)) +
  geom_line(aes(y = DE, color = "DE")) +
  geom_line(aes(y = FR, color = "FR")) +
  geom_line(aes(y = ES, color = "ES")) +
  geom_line(aes(y = GB, color = "GB")) +
  labs(title = "Residuals for Each Country",
       x = "Date",
       y = "Residuals") +
  scale_color_manual(values = c("DE" = "blue", "FR" = "green", "ES" = "red", "GB" = "orange"))


#Calculate the factor contribution for each predictor
# Extract coefficients and create a data frame

coefficients_table <- coef(summary(fit12))
contributions <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)

print(contributions)

# Sort the factor contributions in descending order
factor_contributions = contributions[order(-contributions$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to DE2y",
       x = "Predictor",
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "navyblue", high = "steelblue")

coefficients_table <- coef(summary(fit12))
contributions <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)



# Assuming you have a data frame named 'data' with columns: Date, Yield, Factor1, Factor2, ...

X1 = components$`pca_GL$x[, 1]`
X2 = components$`PCA_EU$x[, 1]`
X3 = components$`PCA_IT$x[, 1]`
X4 = components$`PCA_GB$x[, 1]`

# Calculate average yield
average_yield <- mean(DE2y)

# Create a data frame for the plot
plot_data <- data.frame(
  Date = as.Date(dates),
  Yield = DE2y,
  X1 = X1,
  X2 = X2,
  X3 = X3,
  X4 = X4,
  Factor1_contribution = X1 - average_yield,
  Factor2_contribution = X2 - average_yield,
  Factor3_contribution = X3 - average_yield,
  Factor4_contribution = X4 - average_yield
  # Add more factors if needed
)

# Plotting
ggplot(plot_data, aes(x = Date, y = Yield)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = Factor1_contribution, fill = "Factor1"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = Factor2_contribution, fill = "Factor2"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = Factor3_contribution, fill = "Factor3"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = Factor4_contribution, fill = "Factor4"), alpha = 0.5) +
  # Add more geom_ribbon layers for additional factors
  labs(title = "Global-European Factor Decomposition of German 2Y Sovereign Nominal Yields",
       x = "Date",
       y = "Yield") +
  scale_fill_manual(values = c("Factor1" = "blue", "Factor2" = "green", "Factor3" = "darkgreen","Factor4" = "red")) +
  theme_minimal()


#average_DE2y = yld$GTDEM2Y.Govt
Avg_DE2y = mean(DE2y)

#Calculate the factor contribution for each predictor
# Extract coefficients and create a data frame

coefficients_table <- coef(summary(fit12))
contributions <- data.frame(
  predictor = rownames(coefficients_table),
  Factor_contribution = coefficients_table[, 1]
)

print(contributions)

# Sort the factor contributions in descending order
factor_contributions = contributions[order(-contributions$Factor_contribution),]

# Create a data frame with the relative factor contributions
Relative_Factor_Contribution = coefficients_table[, 1] / Avg_DE2y


# Create a data frame to store the results
factor_contributions_relative <- data.frame(Predictor = rownames(coefficients_table),
                                            Relative_Factor_Contribution = Relative_Factor_Contribution)

# Sort the factor contributions in descending order
factor_contributions_relative <- factor_contributions_relative[order(-factor_contributions_relative$Relative_Factor_Contribution),]


# Convert Predictor to factor
factor_contributions_relative$Predictor <- factor(factor_contributions_relative$Predictor)

# Create a bar plot of the relative factor contributions
ggplot(factor_contributions_relative, aes(x = Predictor, y = Relative_Factor_Contribution, fill = Predictor)) +
  geom_col(color = "black") +
  labs(title = "Relative Factor Contributions to DE2y",
       x = "Predictor",
       y = "Relative Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Predictor") +
  guides(fill = guide_legend(title = "Predictor"))


# Calculate the predicted values for each observation using yhat function
yhat_all = predict.lm(fit12)

# Extract the factor contributions from the model
contributions <- coef(fit12) * apply(yield) 

print(contributions)

##########################################################

#install.packages("reshape2")
require(ggplot2)
require(reshape2)
require(yhat)


# Calculate the predicted values without the "GF" predictor
DE2y_EU = update(fit12, . ~ . -GF)
EU = predict(DE2y_EU)

#contribution of EU factor
contribution_EU = yhat_all - DE2y_EU$model

# Calculate the predicted values without the "GF" predictor
DE2y_GF = update(fit12, . ~ . -EU)
GF = predict(DE2y_GF)

contribution_GF = yhat_all - DE2y_GF$model

# View the contributions for each predictor variable
a = data.frame(GF = contribution_GF, EU = contribution_EU)


# Calculate the predicted values without the "GF" predictor
DE2y_EU <- update(fit12, . ~ . - GF)
EU <- predict(DE2y_EU)

# Contribution of EU factor
contribution_EU <- fit12$model$GF - EU

# Calculate the predicted values without the "EU" predictor
DE2y_GF <- update(fit12, . ~ . - EU)
GF <- predict(DE2y_GF)

# Contribution of GF factor
contribution_GF <- fit12$model$EU - GF

# View the contributions for each predictor variable
contributions_df <- data.frame(GF = contribution_GF, EU = contribution_EU)


#PRVA
# Use augment to calculate contributions
augmented_data <- augment(fit12)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = DE2y,
                               GF = augmented_data$.fitted - augmented_data$.resid,
                               EU = augmented_data$.resid,
                               idiosyncratic = resDE2)

print(contributions_df)

#DRUGA
#DE
# Use augment to calculate contributions
augmented_data1 <- augment(fit1)
augmented_data2 <- augment(fit12)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = DE2y,
                               GF = augmented_data1$.fitted - augmented_data1$.resid,
                               EU = augmented_data2$.fitted - augmented_data2$.resid -augmented_data1$.fitted + augmented_data1$.resid,
                               idiosyncratic = augmented_data2$.resid)

ggplot(contributions_df, aes(x = date, y = yield)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = GF, fill = "GF"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = EU, fill = "EU"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = idiosyncratic, fill = "Idio"), alpha = 0.5) +
  # Add more geom_ribbon layers for additional factors
  labs(title = "Global-European Factor Decomposition of DE 10Y Sovereign Nominal Yields",
       x = "date",
       y = "yield") +
  scale_fill_manual(values = c("GF" = "blue", "EU" = "green", "Idio" = "orange")) +
  theme_minimal()

#fr
# Use augment to calculate contributions
augmented_data1 <- augment(fit5)
augmented_data2 <- augment(fit52)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = GB2y,
                               GF = augmented_data2$.fitted - augmented_data2$.resid,
                               EU = augmented_data2$.fitted -augmented_data1$.fitted,
                               idiosyncratic = augmented_data2$.resid)

ggplot(contributions_df, aes(x = date, y = yield)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = GF, fill = "GF"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = EU, fill = "EU"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = idiosyncratic, fill = "Idio"), alpha = 0.5) +
  # Add more geom_ribbon layers for additional factors
  labs(title = "Global-European Factor Decomposition of GB 10Y Sovereign Nominal Yields",
       x = "date",
       y = "yield") +
  scale_fill_manual(values = c("GF" = "blue", "EU" = "green", "Idio" = "orange")) +
  theme_minimal()

#TRETA
#IT faktor

ITfactor = data.frame(resIT)
ITfactor = as.matrix(ITfactor)

#Euro factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_IT = prcomp(ITfactor, center=TRUE, scale.=T, rank. = 1)
IT     = as.data.frame(PCA_IT$x[,1])

GBfactor = data.frame(resGB)
GBfactor = as.matrix(GBfactor)

PCA_GB = prcomp(GBfactor, center=TRUE, scale.=T, rank. = 1)
GB     = as.data.frame(PCA_GB$x[,1])

summary(PCA_GB)
components = as.data.frame(EU)

fit12 <- lm(resDE~.,data= components)
summary(fit12)$adj.r.squared

fit22 <- lm(FR2y~.,data= components)
summary(fit22)$adj.r.squared

fit32 <- lm(ES2y~.,data= components)
summary(fit32)$adj.r.squared

fit42 <- lm(IT2y~.,data= components)
summary(fit42)$adj.r.squared

fit52 <- lm(GB2y~.,data= components)
summary(fit52)$adj.r.squared
# Use augment to calculate contributions
augmented_data1 <- augment(fit1)
augmented_data2 <- augment(fit12)

# View the contributions for each predictor variable
contributions_df <- data.frame(date = as.Date(dates),
                               yield = DE2y,
                               GF = augmented_data1$.fitted - augmented_data1$.resid,
                               EU = augmented_data2$.fitted - augmented_data2$.resid,
                               idiosyncratic = augmented_data2$.resid)

ggplot(contributions_df, aes(x = date, y = yield)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = GF, fill = "GF"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = EU, fill = "EU"), alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = idiosyncratic, fill = "Idio"), alpha = 0.5) +
  # Add more geom_ribbon layers for additional factors
  labs(title = "Global-European Factor Decomposition of German 2Y Sovereign Nominal Yields",
       x = "date",
       y = "yield") +
  scale_fill_manual(values = c("GF" = "blue", "EU" = "green", "Idio" = "orange")) +
  theme_minimal()



# Create a line chart
ggplot(contributions_df, aes(x = date, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Contributions, Yield, and Idiosyncratic for DE2y",
       x = "Date",
       y = "Value",
       color = "Variable") +
  theme_minimal()

# Assuming contributions_df is already available and has a "Date" column

# Load the ggplot2 package if not already loaded
library(ggplot2)

# Reshape the data frame to long format
contributions_long <- tidyr::gather(contributions_df, key = "variable", value = "contribution", GF, EU)

# Add the Date information
contributions_long$Date <- dates

# Create an area chart
ggplot(contributions_long, aes(x = Date, y = contribution, fill = variable)) +
  geom_area() +
  labs(title = "Contributions to Prediction",
       x = "Date",
       y = "Contribution to Prediction",
       fill = "Predictor Variable") +
  scale_fill_manual(values = c("GF" = "blue", "EU" = "red")) +
  theme_minimal()






# Combine the contributions into a data frame
contributions = data.frame(
  variable = c("GF", "EU"),
  contribution = c(contribution_GF, contribution_EU))

contributions_long = melt(contributions, varnames = c("index", "variable"), value.name = "contribution")

# Rename the columns
colnames(contributions_long)[2] <- "index"

# Convert the "index" column to a factor
contributions_long$index <- factor(contributions_long$index)

# Create a bar plot of the contributions
ggplot(contributions_long, aes(x = variable, y = contribution)) +
  geom_col(fill = "blue") +
  labs(x = "Predictor variable", y = "Contribution to prediction")



# Create a data frame of the factor contributions
contributions = data.frame(
  GF = predict(update(fit12, . ~ . -EU)) - yhat_all,
  EU = predict(update(fit12, . ~ . -GF)) - yhat_all)

# Reshape the data frame to long format
contributions_long <- melt(contributions, varnames = c("index", "variable"), value.name = "contribution")
# Rename the columns
colnames(contributions_long)[2] <- "index"

# Convert the "index" column to a factor
#contributions_long$index <- factor(contributions_long$index)

# Create an area plot of the factor contributions
ggplot(contributions_long, aes(x = index, y = contributions, fill = variable)) +
  geom_area() +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"), 
                    labels = c("wt", "hp", "disp")) +
  labs(x = "Observation index", y = "Contribution to prediction",
       fill = "Predictor variable")

X1 = components$`pca_GL$x[, 1]`
X2 = components$`PCA_EU$x[, 1]`
mod <- lm(DE2y~ X1 + X2)
tmp <- data.frame(t(coef(mod) * t(cbind(1, X1, X2))))
names(tmp) <- c("Intercept", "X1", "X2")
ggplot(x=as.factor(1:6960), fill=variable, weight=value, geom="bar", data=melt(tmp)) +
  geom_point(aes(x=1:6960, y=predict(mod)))

#plot(fit2)


yield_data <- data.frame(
  Date = as.Date(dates),
  Global = GF,
  Euro_Area = EU,
  Idiosyncratic = resDE2
)

# Create a new data frame for plotting
plot_data <- yield_data %>%
  select(Date, Global, Euro_Area, Idiosyncratic) %>%
  pivot_longer(-Date, names_to = "Factor", values_to = "Yield")

# Plot the chart
ggplot(plot_data, aes(x = Date, y = Yield, fill = Factor)) +
  geom_area() +
  scale_fill_manual(values = c("darkblue", "red", "green")) +
  theme_minimal() +
  labs(x = "Date", y = "Yield", fill = "Factor") +
  ggtitle("Global-local decomposition of French 2-year and 10-year sovereign yields")



# Izraèun vsote prispevkov
Total_Contribution <- GF + EU + resDE2

# Primerjava s skupno vrednostjo donosnosti
summary_stats <- summary(plot_data$Yield - plot_data$Total_Contribution)

# Izhod rezultatov
print(summary_stats)







