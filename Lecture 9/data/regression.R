# Clear workspace
rm(list = ls())

# Clear plots
dev.off(dev.list()["RStudioGD"])

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc)

# Set locale to English
Sys.setlocale("LC_ALL","English")

library(tidyverse)

## Gujarati (2004), p. 56
# Load the data
expend <- read.csv("foodexp.csv")

# Make a scatterplot
fig1 <- ggplot(expend, aes(x = totalexp, y = foodexp)) + 
  geom_point(col = "red", size = 4, alpha = 0.5) + 
  theme_bw()

fig1

# Run the regression
mod1 <- lm(foodexp ~ totalexp, data = expend)

class(mod1)
mode(mod1)

# Add regression line to plot
fig2 <- fig1 +
  geom_smooth(method='lm', formula= y~x, se = T) 

fig2

print(mod1)
summary(mod1)

names(mod1)

anova(mod1)

confint(mod1, level = 0.95)

fitted(mod1)

resid(mod1)

predict(mod1)

new.df <- data.frame(totalexp=c(1:50))
predict(mod1, newdata=new.df)

# Load data
load("ceosal1.RData")
mod_ceosal <- lm(log(salary) ~ log(sales), data = data)

summary(mod_ceosal)

# Residual variance
sigmahat_sq <- sum((mod_ceosal$residuals)^2)/(length(data$salary) - 2)

# Residual standard error
resid_se <- sqrt(sigmahat_sq)

# Variance of alphahat
var_ahat <- sigmahat_sq * sum((log(data$sales)^2)) / 
  (sum((log(data$sales) - mean(log(data$sales)))^2) * length(data$salary))

# Standard error of alphahat
se_ahat <- sqrt(var_ahat)

# Variance of betahat
var_bhat <- sigmahat_sq / 
  sum((log(data$sales) - mean(log(data$sales)))^2)

# Standard error of betahat
se_bhat <- sqrt(var_bhat)

# Covariance of alphahat and betahat
cov_ahat_bhat <- -mean(log(data$sales)) * var_bhat

# Compare with:
vcov(mod_ceosal)

# t-statistics of coefficients
t_ahat <- coef(mod_ceosal)[1]/se_ahat
t_ahat

t_bhat <- coef(mod_ceosal)[2]/se_bhat
t_bhat

df_t <- length(data$salary) - 2

# p-values of t-statistics
pval_t_ahat <- 2 * (1 - pt(t_ahat, df_t))
pval_t_ahat

pval_t_bhat <- 2 * (1 - pt(t_bhat, df_t))
pval_t_bhat

# RSS 
RSS <- sum((mod_ceosal$residuals)^2)

# TSS
TSS <- sum((log(data$salary) - mean(log(data$salary)))^2)

# ESS
ESS <- TSS - RSS

# R-squared and adjusted R-squared
R_sq <- ESS/TSS

nobs <- length(data$salary)
ncoef <- length(coef(mod_ceosal)) - 1
R_sq_adj <- 1 - (1 - R_sq) * (nobs - 1) / (nobs - ncoef - 1)

smr <- summary(mod_ceosal)

smr$r.squared
smr$adj.r.squared

## Multiple regression
library(foreign)

# Load data
gradedata <- read.spss("data1_1.sav", to.data.frame = T)

# Change column names to lowercase:
names(gradedata) <- tolower(names(gradedata))

# Attach the data frame so you can use variable names directly:
attach(gradedata)

# Run the following regression model:
mod2 <- lm(grade ~ books + attend)

# View the model output summary:
summary(mod2)

# Run the model without an intercept term:
mod2a <- lm(grade ~ 0 + books + attend)

mod2b <- lm(grade ~ -1 + books + attend)

summary(mod2a)
summary(mod2b)

mod2c <- lm(grade ~ books * attend)
summary(mod2c)

mod2d <- lm(grade ~ books + attend + books:attend)
summary(mod2d)

mod2e <- lm(grade ~ books:attend)
summary(mod2e)

## Polynomial regression

# Load the data
poly <- read.csv("poly.csv")
names(poly) <- tolower(names(poly))
attach(poly)

# Display the values of $x$ and $y$ on a graph:
plot(x,y)

# Run the following models:
p1 <- lm(y ~ x)
summary(p1)

# Compare the fit and the actual values:
lines(x, fitted(p1), lwd=2, col="red")

p2 <- lm(y ~ x + I(x^2))
summary(p2)

plot(x,y)
lines(x, fitted(p2), lwd=2, col="red")

p3 <- lm(y ~ x + I(x^2) + I(x^3))
summary(p3)

plot(x,y)
lines(x, fitted(p3), lwd=2, col="red")

p4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
summary(p4)

plot(x,y)
lines(x, fitted(p4), lwd=2, col="red")

## Causality and regression: a spurious example
library(eurostat)
library(tidyverse)

# Population by educational attainment level, 
# sex and age (%) - main indicators 
edu_eu <- get_eurostat("edat_lfse_03")

# Tertiary education in Belgium, percentage
tert_edu_be <- edu_eu %>% 
  filter(sex == "T", 
         age == "Y15-64",
         unit == "PC",
         isced11 == "ED5-8",
         geo == "BE",
         time > as.Date("1994-01-01"),
         time < as.Date("2020-01-01")) %>% 
  mutate(time = substr(time, 1, 4), values = values * 100) %>% 
  arrange(time) %>% 
  select(time, values)

#  Government revenue, expenditure and main aggregates 
gov_eu <- get_eurostat("gov_10a_main")

rev_ee <- gov_eu %>% 
  filter(unit == "MIO_EUR", 
         sector == "S13", 
         na_item == "TR", 
         geo == "EE") %>% 
  mutate(time = substr(time, 1, 4)) %>% 
  arrange(time) %>% 
  select(time, values)

# Regression
spur_reg <- lm(tert_edu_be$values ~ rev_ee$values[1:25])
summary(spur_reg)

ggplot(rev_ee, aes(x = as.numeric(time))) + 
  geom_line(aes(y = values))

ggplot(tert_edu_be, aes(x = as.numeric(time))) + 
  geom_line(aes(y = values))
