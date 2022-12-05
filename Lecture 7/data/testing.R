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

tt <- read.csv("t-test.csv")
attach(tt)

t.test(var1, mu = 3)

xbar <- mean(var1)
s <- sd(var1)/sqrt(25)
t_0975 <- qt(0.975, 24)
lbound <- xbar - t_0975*s
ubound <- xbar + t_0975*s

t.test(var1, var2)

t.test(var1, mu = 3, conf.level = 0.99)

wilcox.test(var1, mu = 3)

wilcox.test(var1, var2)

var.test(var1, var2)

F_stat <- var(var1)/var(var2)
p_val <- 2*pf(F_stat, 24, 24) # Two-sided test

# Lower and upper bound of confidence interval
lbound <- F_stat*qf(0.025, 24, 24)
ubound <- F_stat*qf(0.975, 24, 24)

# Look at the corresponding F distribution
x <- seq(0, 5, by = 0.01)
y <- df(x, 24, 24)

plot(x, y, type = "l") # Note the location of 1 (the hypothesized ratio)

tt <- tt %>% 
  mutate(diff_pairs = var1 - var2)

se_diff <- sd(tt$diff_pairs)/sqrt(n)
n <- length(tt$var1)

t_stat <- (mean(tt$diff_pairs) - 0)/se_diff

p_val <- 2*pt(t_stat, 24)

t.test(var1, var2, paired = TRUE)

wilcox.test(var1, var2, paired = TRUE)

sexsmoke <- matrix(c(70,120,65,140),ncol=2,byrow=TRUE)
rownames(sexsmoke) <- c("male","female")
colnames(sexsmoke) <- c("smoke","nosmoke")
sexsmoke
prop.test(sexsmoke)
binom.test(486,1000)

cont_table <- matrix(c(27, 25, 24, 24), nrow = 2)
rownames(cont_table) <- c("Males", "Females")
colnames(cont_table) <- c("BA", "Economics")

cont_table
fisher.test(cont_table)

chisq.test(cont_table)

