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

# Set locale
Sys.setlocale("LC_ALL","English")
# Slide 12 examples
# Number of combinations of 49 elements of class 6
cb1 <- choose(49,6)
prob_6 <- 1/cb1

# # Number of combinations of 49 elements of class 6
# Divide by the number of combinations of 6 elements of class 5
# times the number of combinations of 43 elements of class 1

(choose(6,5)*choose(43,1))

cb2 <- choose(49,6)/(choose(6,5)*choose(43,1))
1/cb2

# Probability of getting 5 or 6
prob5_6 <- 1/cb1 + 1/cb2

# Probability of an ace
p_ace <- 4/52

# Probability of spade
p_spade <- 13/52

# Probability of ace or spade
p_ace_or_spade <- p_ace + p_spade - 1/52

# Slide 14 example 
# Probability of getting three sixes in three consecutive rolls
p_3_sixes <- 1/6*1/6*1/6

# Slide 16
# Probability that you get a six in the second dice roll 
# if in the first roll you got five
p_6_5 <- 1/6

# Slide 26
# Mean and variance
x <- c(1,7,18,45,9,27,65, NA)
m_x <- mean(x, na.rm = T)
sd_x <- sd(x, na.rm = T)
var_x <- var(x, na.rm = T)
sqrt(var_x) == sd_x

# Slide 29
# Binomial distribution
# Probability of 2 successes out of 10 trials, fair coin:
dbinom(2, 10, 0.5)

# Probability of not getting a pass (60%) if you
# answer randomly in an exam in which there are 20 multiple-choice
# questions (the number of options is 4)
pbinom(11, 20, 0.25)

# Quantiles ? find the number of successes that correspond to 
# the 75th percentile:
qbinom(0.75, 20, 0.25) 

# Plot of binomial distribution with p = 0.6 and n = 10
x <- seq(from = 0, to = 10)
p_x <- dbinom(x, 10, 0.6)
plot(x,p_x, type = "h", lwd = 2, col = "red")

# Slide 31
# Poisson distribution
# 15 cars arrive at a gas station on average per hour. What is the
# probability that 10 cars arrive during a chosen hour?
dpois(10, 15)

# Probability of 10 or less cars arriving
ppois(10, 15)

dpois(0, 15)

pois_rv <- rpois(1000, 15)

var(pois_rv)

sd(pois_rv)

# Slide 36: probability of being between 1.3 and 2.1
pnorm(2.1) - pnorm(1.3)

# Random number generation, standard normal case
x <- rnorm(1000)
plot(x, type = "l", col = "red")
hist(rnorm(100000000, mean = 2, sd = 5), breaks = 150, 
     col = "orange")

