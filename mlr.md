---
title: "**Multiple Linear Regression**"
author: "_**Hande Çelebi**_"
date: "06 05 2021"
output: 
  html_document:
    keep_md: true
    toc: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.align = 'center', echo = TRUE)
```


```{r codes first, include = FALSE}
library(prettydoc)
library(readxl)
library(ppcor)
library(MASS)
library(corrplot)
library(car)
library(tidyverse)
library(leaps)
library(olsrr)
library(fBasics)
library(lmtest)
library(ggplot2)
library(reshape2)
library(GGally)
library(dplyr)
library(kableExtra)
suc <- read_excel("suc.xlsx")
options(scipen = 99)
attach(suc)
```
## **Factors Affecting Crime - Case Study by Cities in Turkey**


The aim of this study is to evaluate the factors affecting crime according to the provinces in Turkey. The variables to be used in the analysis are as follows: 

* X1 : "Number of convicts entering the penal institution according to the person who committed the crime",
* X2 : "City Population",
* X3 : "Immigration from abroad to Turkey",
* X4 : "Literacy Rate"
* X5 : "GDP per capita"
* X6 : "As a dummy variable whether the city is Metropolitan or not"

As data set observations, all 81 cities in Turkey were evaluated in the analysis.

Before starting the analysis, we need to make some adjustments to the data set. First, we need to select only the variables we want to examine from the data set. In addition, the variable names in the original data have been changed to X1, ... X6 to increase the legibility of the models. Finally, being a metropolitan city is added to the data set as a dummy variable.

The summary view of the data set is as follows. 

```{r head of data}
suc_last <- suc %>%
  select(cities = `...1`, X1 = `Suç işlenen ile göre ceza infaz kurumuna giren hükümlü sayısı`, X2 = `İl Nüfusu`, X3 = `Yurt dışından Türkiye'ye gelen göç`,X4 = `Okuma Yazma Oranı`, X5 = `Kişi başına GSYH`)%>%
  mutate(X6 = ifelse(suc$Büyükşehir == "B",1,0))
kbl(head(suc_last)) %>%
  kable_minimal()
```

````{r code, include = FALSE}
attach(suc_last)
```
### **1. Linearity controls before modelling with plots** 
Before building a model with the data, we examine the relationship of all independent variables with the dependent variable. Thus, we observe how our data is distributed. We also have a preview of whether a linear model with variables is suitable.
```{r pressure}
df <- data.frame(X1,X2,X3,X4,X5)
dfrGraph <- gather(df, variable, value, -X1)
ggplot(dfrGraph) +
  geom_jitter(aes(value,X1, colour=variable)) + 
  geom_smooth(aes(value,X1, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x", labeller = as_labeller(c(X2 = "Population", X3 = "Immigration", X4 ="Literacy", X5 = "GDP percapita")))
 
```
The graphs show that we will have to make some transformations in the variables in order to analyze the models more properly. Because the variables are distributed over a small range, doing logarithmic transformations will solve this problem.
When the necessary logarithmic transformations are made (all variables except literacy rate), the model will be as follows. 
```{r log}
log_X1 <- log(X1)
log_X2 <- log(X2)
log_X3 <- log(X3)
log_X5 <- log(X5)
dflog <- data.frame(log_X1,log_X2,log_X3,X4,log_X5)
dfrGraph <- gather(dflog, variable, value, -log_X1)
ggplot(dfrGraph) +
  geom_jitter(aes(value,log_X1, colour=variable)) + 
  geom_smooth(aes(value,log_X1, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x", labeller = as_labeller(c(log_X2 = "Population", log_X3 = "Immigration", X4 ="Literacy", log_X5 = "GDP percapita")))
```
Thus, we can observe the linear relationship between all variables. We can say that they are suitable variables to establish a regression model. 
### **2. Regression Analysis**
#### **2.1. Regression Model**
```{r model}
model <- lm(log_X1 ~  log_X2 + log_X2 + log_X3 + X4 + log_X5 + X6)
summary(model)
```
Because of the Adjusted R-Square value is 0.9507, we can say that the model is generally significant. Also, the p-value is <0.05 so H0 is rejected and there is relationship between variables. 
We can evaluate the significance of the variables one by one  and also we measure whether it represents the population well according to the coefficient table.
However, since the study is carried out with the population (81 cities), the variables are not significant, which does not pose a problem in the model. When we examine the P-values, we can say that all variables except X5 and X6 are significant. 
Finally, estimate values can be evaluated as follows:
* 1% increase in X2 will increase X1 by 1.07%,
* 1% increase in X3 will decrease X1 by 0.07%,
* 1 unit increase in X4 will increase X1 by 0.08%,
* 1% increase in X5 will decrease X1 by 0.05%.
* 1 unit increase in X6 will decrease X1 by 0.03%.
(X1 = "Number of convicts entering the penal institution according to the person who committed the crime, X2 = City Population, X3 = Immigration from abroad to Turkey, X4 = Literacy Rate, X5 = GDP per capita, X6 = As a dummy variable whether the city is Metropolitan or not)
### **3. Assumptions of the Regression Model**
#### **3.1. Multicollinearity**
Multicollinearity problem occurs when the relationship between independent variables is high. Since there are too many variables in our model, we are likely to encounter this problem. 
To evaluate this, we must examine partial correlation coefficients. 
```{r multicol}
df_full <- data.frame(log_X1, log_X2, log_X3, X4, log_X5, X6)
partial_cor <- pcor(df_full)$estimate
kbl(partial_cor, caption = "Partial Correlation Matrix") %>%
  kable_minimal()
  
```
When we examine the matrice, we cannot observe the high relationship between the independent variables. 
We can use the corelation plot to better observe this high or low relationship. The correlation plot is as follows. 
```{r cor plot}
ggpairs(data.frame(partial_cor))
```
As seen in the plot,clearly seen that there is no relation between independent variable with both the corelation coefficients and the scatter plot. 
But to get more precise results, we need to examine the Variance inflation factor (VIF) values of the model. Variance inflation factor (VIF) is a measure of the amount of multicollinearity in a set of multiple regression variables. A high VIF indicates that the associated independent variable is highly collinear with the other variables in the model.According to most studies in the literature, if the VIF value is above 10, we can say that there is a multicollinearity problem.
The VIF values of the model are as follows.
```{r vif}
vif(model)
```
According to VIF values, there is no multicollinearity problem in the model.
#### **3.2. All Possible Models**
Thanks to the ols_step_all_possible function, we can examine the all possible models that can be set with the alternatives of all the variables. In this way, we can find answers to questions such as with which variables we reach the lowest error and how many variables should we keep in the model.
```{r all possible}
ols_step_all_possible(model)
```
When all possible models are examined, we can see that the lowest Mellow's CP value is the 16th model, the model with 3 independent variables (X2, X3, X4). From this point, it may make sense to exclude the X5 and X6 variables from the model.
The graphic representation of possible models is as follows. 
```{r plot all pos}
plot(ols_step_all_possible(model))
```
Errors increased after 16th model in CP, SBC and SBIC. 
As a result, the X5 and X6 variables were excluded from the model and the analysis continued. 
```{r new model}
new_model <- lm(log_X1 ~ log_X2 + log_X3 + X4)
```
#### **3.3. Autocorrelation**
The distribution of Y fitted values and standard errors gives us a clue as to whether there is autocorrelation or not. 
```{r autocor}
y_fit <- fitted(new_model)
stand_er <- rstandard(new_model)
plot(y_fit, stand_er)
```
When we examine the plot, we can say that there is no autocorrelation, but we should use the Durbin-Watson test to make a definite judgment. 
Durbin-Watson Test results is as follows:
```{r durbin watson}
durbinWatsonTest(new_model)
```
The fact that the D-W Statistic value is around 2 shows us that there is no autocorrelation. Also, p-value > 0.05 H0 is not rejected and there is no autocorrelation.
#### **3.4. Normality Test**
According to the assumption of normality, residuals should be distributed normally. To test normality, Jarque - Bera and Shapiro tests, which are widely used normality tests in the literature, were used. 
```{r normality}
res <- residuals(new_model)
shapiro.test(res)
jarqueberaTest(res)  
```
According to the results of both tests, p-value > 0.05 H0 not rejected and we reach the conclusion that residuals are distributed normally.
#### **3.5. Homoscedasticity Test**
Homoscedasticity requires a low relationship between absolute errors and independent variables. To test this, firstly, the correlation matrix of absolute errors and independent variables should be examined. 
```{r homoscedasticity}
abs_er <- abs(residuals(new_model))
df_abs <- data.frame(abs_er, log_X2, log_X3, X4)
kbl(cor(df_abs, method = "spearman"), caption = "Homoscedasticity") %>%
  kable_minimal()
```
According to the correlation matrix, we can say that the relationship between absolute errors and independent variables is low. Accordingly, we can conclude that the model is homoskedastic. However, in order to reach a definite conclusion, the Goldfeld-Quandt and Breush-Pagan tests which are widely used in the literature, should be applied to the model. 
```{r gq test}
gqtest(new_model, order.by = ~ log_X2 + log_X3 + X4, fraction = 16)
bptest(new_model)    
```
According to the results of both tests, the H0 is not rejected model is homoskedastic. 
### **4. Conclusion**
As a result of the assumptions applied, we can say that there is a relationship between the Number of convicts and City Population, Immigration from abroad to Turkey, Literacy Rate. 
As a result of the regression model, while City Population and Literacy Rate have an increasing effect on the Number of convicts, Immigration from abroad to Turkey has a decreasing effect.
### **5. Resources**
* https://data.tuik.gov.tr/Bulten/Index?p=Il-Bazinda-Gayrisafi-Yurt-Ici-Hasila-2019-33663
* https://data.tuik.gov.tr/Bulten/Index?p=Ceza-Infaz-Kurumu-Istatistikleri-2019-33625
* https://data.tuik.gov.tr/Bulten/Index?p=Adrese-Dayali-Nufus-Kayit-Sistemi-Sonuclari-2020-37210
* https://data.tuik.gov.tr/Bulten/Index?p=Uluslararasi-Goc-Istatistikleri-2019-33709
