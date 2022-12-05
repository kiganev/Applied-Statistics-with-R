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

dt(0,15)

dnorm(0)

x <- seq(-5, 5, by = 0.01)
y = dt(x, 9)
plot(x, y, type = "l")

y <- dnorm(x)

lines(x, y, col = "red")

pt(0, 15)

sample(1:100, 10)

smpl <- sample(1:100, 50, replace = TRUE)
smpl

table(smpl)

sample(1:49, 6)

sample(1:50, 5)

faircoin <- c("Heads", "Tails")
tosses <- sample(faircoin, size = 100000000, replace = TRUE)

50000000/60/24/365

table(tosses)

library(openxlsx)
mydataset <- read.xlsx("zzz.xlsx", sheet = "Sheet1")
mysample <- mydataset[sample(1:nrow(mydataset), 10, replace =
                               TRUE),]

