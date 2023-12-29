#——————————————————–#
# PCA three factors of bond yields
#========================================================#
setwd("//optnt/optshr/6-tedensko porocilo/Odbor za denpol/PCA")
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
install.packages("rblpapi")
require(rblpapi)

install.packages("RBloomberg")
library(RBloomberg)

if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}

library(broom)


# Connect to Bloomberg
con <- blpConnect()

opt= c(predicitySelection="DAILY")
DE3M=bdh("GTDEM3M Govt","YLD_YTM_MID","01.01.2000","24.01.2023", options=opt)

graphics.off(); rm(list = ls())

# graph function
#———————————————————–

f_draw_factor_loading = function(m.loadings, var, title){
  x11(width=16/3,height=5);
  matplot(mat,m.loadings,type="l",xaxt="n",lwd=5,lty=1, 
          main=title,col=rainbow(3),ylim=c(-0.6, 0.6),
          xlab= "Maturity",ylab = "Factor loadings")
  legend("bottomright",max(m.loadings),col=rainbow(3),lty=1,lwd=3,
         legend=paste0(c("1st PC-","2nd PC-","3rd PC-"),var))
  axis(1,mat,mat)
}

#=========================================================#
#Load Data
#=========================================================#

df  = read.xlsx(file="datadaily.xlsx", sheetIndex = 1, header= T, startRow = 6 )
dfUS=read.table(file ="Sample.txt",header=T)

yld   = na.omit(df)
yield = as.matrix(yld[,2:6]/100)
ym    = df[,1] # date
mat   = c(3,12,60,120,180,240,360) #Months


#------------------------------------
#Regress PCA components and data: https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
#-----------------------------------

pca_GL= prcomp(yield, center=TRUE, scale.=T, rank. = 1)
pcagl = princomp(yield) #loadings = eigenvectors, https://stats.stackexchange.com/questions/87037/which-variables-explain-which-pca-components-and-vice-versa
summary(pca_GL)
summary(pca_GL$x) # x= te value of each sample in terms of the pca
print(pcaGL$rotation) #rotation is the relationship (correlation/anticorr)between initial x and PCA
pca_GLfig= biplot(cex = .5)

#———————————————————
#Regress data with two PCA components
#———————————————————
#Tukaj regresiram 2 PCA s spremenljivko 3MDE.
#Èe me zanima kaj premika 10y DE, moram njo regresirat z PCA 1 (global Factor)in eurozone factor (EMU obveznice)

components = as.data.frame(cbind(FR2y= yield[,"GTFRF2Y.Govt"], pca_GL$x[,1]))
GF         = as.data.frame(pca_GL$x[,1])


fit1       = lm(FR2y~ ., data = GF)
fit2       = lm(yld$GTFRF2Y.Govt ~., data= GF)
summary(fit2)$adj.r.squared
fit3       = lm(yld$GTESP2Y.Govt ~., data= GF)
summary(fit3)$adj.r.squared
fit4       = lm(yld$GTDEM2Y.Govt ~., data= GF)
summary(fit4)$adj.r.squared
fit5       = lm(yld$GTGBP2Y.Govt ~., data= GF)
summary(fit5)$adj.r.squared

#Construct series that cant be explained by the PCA1 (global Factor)
resFR = fit2$residuals
resES = fit3$residuals
resDE = fit4$residuals
resUK = fit5$residuals

Eurofactor = data.frame(resFR, resES, resDE)
as.matrix(Eurofactor)

#Euro factor, by taking PCA1 of DE,ES, FR residuals after accounting for global factor
PCA_EU = prcomp(Eurofactor, center=TRUE, scale.=T, rank. = 1)
EU     = as.data.frame(PCA_EU$x[,1])
summary(PCA_EU)
components = as.data.frame(cbind(GF, EU))

DE2y= lm(yld$GTDEM2Y.Govt~.,data= components)
summary = summary(DE2y)

#Calculate the factor contribution for each predictor

contributions = data.frame (predictor = names(coefficients(DE2y)),
                            Factor_contribution = coef(summary)[,1])

print(contributions)

# Sort the factor contributions in descending order
factor_contributions = contributions[order(-contributions$Factor_contribution),]

# Create a bar chart of the factor contributions
ggplot(contributions, aes(x = predictor, y = Factor_contribution, fill = Factor_contribution)) +
  geom_bar(stat = "identity") +
  labs(title = "Factor Contributions to DE2y",
       x = "Predictor",x
       y = "Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "navyblue", high = "steelblue")

# Calculate the average DE2y value since 2015
average_DE2y = yld$GTDEM2Y.Govt
Avg_DE2y = mean(average_DE2y)

# Create a data frame with the relative factor contributions
factor_contributions_relative = data.frame(Predictor = names(coefficients(DE2y)),
                                            Relative_Factor_Contribution = coef(summary)[,1] / yld$GTDEM2Y.Govt)

# Sort the factor contributions in descending order
factor_contributions_relative <- factor_contributions_relative[order(-factor_contributions_relative$Relative_Factor_Contribution),]


# Create an area chart of the relative factor contributions
ggplot(factor_contributions_relative, aes(x = Predictor, y = Relative_Factor_Contribution, fill = Predictor)) +
  geom_area() +
  labs(title = "Relative Factor Contributions to DE2y",
       x = "Predictor",
       y = "Relative Factor Contribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Predictor")



# Calculate the predicted values for each observation using yhat function
yhat_all = predict.lm(DE2y)

# Extract the factor contributions from the model
contributions <- coef(DE2y) * apply(yield) 

print(contributions)

##########################################################

#install.packages("reshape2")
require(ggplot2)
require(reshape2)
require(yhat)


# Calculate the predicted values without the "GF" predictor
DE2y_EU = update(DE2y, . ~ . -GF)
EU = predict(DE2y_EU)

#contribution of EU factor
contribution_EU = yhat_all - DE2y_EU$model

# Calculate the predicted values without the "GF" predictor
DE2y_GF = update(DE2y, . ~ . -EU)
GF = predict(DE2y_GF)

contribution_GF = yhat_all - DE2y_GF$model

# View the contributions for each predictor variable
data.frame(GF = contribution_GF, EU = contribution_EU)


# Combine the contributions into a data frame
contributions=data.frame(
  variable = c("GF", "EU"),
  contribution = c(contribution_GF, contribution_EU))

contributions_long = melt(contributions, varnames = c("index", "variable"), value.name = "contribution")

# Convert the index column to factor
contributions_long$inde = factor(contributions_long$index)

# Create a bar plot of the contributions
ggplot(contributions_long, aes(x = variable, y = contribution)) +
  geom_col(fill = "blue") +
  labs(x = "Predictor variable", y = "Contribution to prediction")



# Create a data frame of the factor contributions
contributions = data.frame(
  GF = predict(update(DE2y, . ~ . -EU)) - yhat_all,
  EU = predict(update(DE2y, . ~ . -GF)) - yhat_all)

# Reshape the data frame to long format
contributions_long <- melt(contributions, varnames = c("index", "variable"), value.name = "contribution")

# Create an area plot of the factor contributions
ggplot(contributions_long, aes(x = index, y = contribution, fill = variable)) +
  geom_area() +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"), 
                    labels = c("wt", "hp", "disp")) +
  labs(x = "Observation index", y = "Contribution to prediction",
       fill = "Predictor variable")

X1 = components$`pca_GL$x[, 1]`
X2 = components$`PCA_EU$x[, 1]`
mod <- lm(yld$GTDEM2Y.Govt~ X1 + X2)
tmp <- data.frame(t(coef(mod) * t(cbind(1, X1, X2))))
names(tmp) <- c("Intercept", "X1", "X2")
qplot(x=as.factor(1:6342), fill=variable, weight=value, geom="bar", data=melt(tmp)) +
  geom_point(aes(x=1:6342, y=predict(mod)))

plot(fit2)

#———————————————————–
# Yield level PCA using princomp()
#———————————————————–
pca = princomp(yield) #Cov.matrix

f_draw_factor_loading(pca$loadings[,1:3], 
                      round(100*pca$sdev[1:3]^2/sum(pca$sdev^2),2),
                      "Yield level PCA – princomp()")

#———————————————————–
# Yield change PCA using princomp()
#———————————————————–
pca = princomp(diff(yield, 1))

f_draw_factor_loading(pca$loadings[,1:3], 
                      round(100*pca$sdev[1:3]^2/sum(pca$sdev^2),2),
                      "Yield change PCA – princomp()")

#———————————————————–
# Yield level PCA using eigen()
#———————————————————–
eig = eigen(cov(yld))

f_draw_factor_loading(eig$vectors[,1:3], 
                      round(100*eig$values[1:3]/sum(eig$values),2),
                      "Yield level PCA – eigen()")

#———————————————————–
# Yield change PCA using eigen()
#———————————————————–
eig = eigen(cov((diff(yield, 1))))

f_draw_factor_loading(eig$vectors[,1:3], 
                      round(100*eig$values[1:3]/sum(eig$values),2),
                      "Yield change PCA – eigen()")

