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
library(openxlsx)

# Load data on purchases
purchases <- read_delim("data_purchases.csv", delim = ";")

# Store the values of k and n
k <- ncol(purchases)
n <- nrow(purchases)

# Create a copy of the data frame for later
purchases_copy <- purchases

# Calculate group means
means <- colMeans(purchases)

# Calculate grand mean
grand_mean <- sum(purchases)/(n*k)

# Calculate group variances
variances <- sapply(purchases, var)

# Add columns for squared deviations from means
purchases <- purchases %>% 
  mutate(sqdev1 = (Store1 - means[1])^2,
         sqdev2 = (Store2 - means[2])^2,
         sqdev3 = (Store3 - means[3])^2) 

# Calculate SS_w
SS_w <- sum(colSums(purchases[,4:6]))

# Calculate s^2_w
ssq_w <- SS_w/(k*(n-1))

# Calculate SS_b
SS_b <- sum((means - grand_mean)^2)

# Calculate s^2_b
ssq_b <- n*SS_b/(k-1)

# Calculate F-statistic
F_stat <- ssq_b/ssq_w

# Check p-value
p_val <- 1 - pf(F_stat, k-1, k*(n-1))

# Use aov from R
purchases_copy <- purchases_copy %>% 
  gather(store, value)

aov(purchases_copy$value ~ purchases_copy$store)

mod1 <- lm(purchases_copy$value ~ purchases_copy$store)
summary(mod1)

anova(mod1)

# Regression sum of squares
sum((mod1$fitted.values - grand_mean)^2)

# Error sum of squares
sum((mod1$residuals)^2)

###########################
# Kruskal-Wallis example
###########################

# Load the data (numbers are sales)
advert <- read.xlsx("type_of_advertising.xlsx")

# Convert to long format
advert_melt <- advert %>% 
  gather(advtype, values)

# Order by values
advert_melt <- advert_melt %>% 
  arrange(values)

# Assign ranks
advert_melt <- advert_melt %>% 
  mutate(rank = 1:length(advert_melt$values),
         check_tie = NA) 

# Check for ties and amend ranks
for(i in 2:length(advert_melt$values)){
  if(advert_melt$values[i] == advert_melt$values[i-1]){
    advert_melt$check_tie[i-1] <- paste0("tie", i-1)
    advert_melt$check_tie[i] <- paste0("tie", i-1)
  }
}

# Check number of ties
n_ties <- length(unique(advert_melt$check_tie))/2

advert_melt[2:3, 3] <- (advert_melt$rank[2] + 
                          advert_melt$rank[3])/2

# Calculate R_j
R_j <- advert_melt %>% 
  group_by(advtype) %>% 
  summarise(Rj = sum(rank))

# Test statistic
H <- 12 / (28*29) * sum((R_j$Rj)^2/7) - 3*29 

p_val <- 1 - pchisq(H, 3)

# Use R's function
kruskal.test(advert)
