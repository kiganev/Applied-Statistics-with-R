# Clear wortkspace
rm(list = ls())

# Clear plots
check_dev <- dev.list()
if(!is.null(check_dev)){
  dev.off(dev.list()["RStudioGD"])  
}

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc, check_dev)

# Set locale to English
Sys.setlocale("LC_ALL","English")

library(readxl)

# Read data on population
pop <- read_xls("Pop_6.1.1_Pop_DR_EN.xls", 
                skip = 4, 
                na = "-",
                sheet = "2021")

# Remove columns with urban and rural data
pop <- pop[,1:4]

# Remove country totals
pop <- pop[-1,]

# Rename columns
colnames(pop)[2:4] <- c("Total", "Male", "Female")

# Count rows and columns
nrow(pop)
ncol(pop)

# Access a specific cell
pop[2,3]
pop[[2,3]]

# Access variables
pop$Total
pop$Male
pop$Female

# Descriptive stats: minima and maxima
max(pop$Total)
min(pop$Total)

range(pop$Male)

which.max(pop$Total)
pop$Municipalities[222]
which.min(pop$Total)
pop$Municipalities[108]

pop$Municipalities[which.max(pop$Total)]

## This program will display a message 
## stating the city with the maximum population
maxm <- which.max(pop$Total)
city <- pop[maxm, 1, drop = T]
cat("The Bulgarian municipality with the largest population (totalling",
    max(pop$Total), "inhabitants) is", city, ".")

# Median; quantiles
median(pop$Total, na.rm = T) # or
quantile(pop$Total, 0.5, na.rm = T)

quantile(pop$Total, probs=c(0, 0.25, 0.5, 0.75, 1.0),
         na.rm = T)

IQR(pop$Total, na.rm = T)

table(pop$Total)

# Mode
Mode <- function(x) {
  temp <- table(as.vector(x))
  as.numeric(names(temp)[temp == max(temp)])
} 

Mode(pop$Male)

library(modeest)
mlv(pop$Male, method = "mfv")

# Mean, variance, standard deviation
mean(pop$Total, na.rm = T)

var(pop$Total, na.rm = T)

sd(pop$Total, na.rm = T)

summary(pop) # Summary stats
str(pop) # Structure of frame

# Covariance and correlation
cov(pop$Male, pop$Female)

cor(pop$Male, pop$Female)

library(openxlsx)

pearson <- read.xlsx("pearson.xlsx", sheet = "Sheet1")
attach(pearson)
cor(var1, var2, method = "pearson")
cor(var1, var2, method = "spearman")

cor(var1, var2, method = "kendall")

# Histogram
infl <- read.csv("cpi_infl_avg.csv",
                 header = TRUE, stringsAsFactors = FALSE)

hist(infl$value, main="Distribution of Inflation Rates",
     xlab="Values", breaks = 120, freq = T)

hist(infl$value, main="Distribution of Inflation Rates",
     xlab="Values", breaks = 120, xlim = c(-35,35),
     col = "orange")

# Scatter plot
gdp <- read.csv2("gdp.csv")

plot(gdp$GDP, gdp$Cons)

plot(gdp$Year, gdp$Exports, type = "l", ylim = c(0,70))

points(gdp$Year, gdp$Exports)

lines(gdp$Year, gdp$Imports, col = "red")

points(gdp$Year, gdp$Imports, col = "red")

# Bar graphs and box plots
barplot(gdp$GDP, names.arg = gdp$Year, col = "blue")

boxplot(gdp$Cons, gdp$GDP)

# Plotting function curves
curve(x^2 + log(x))

ownf <- function(xvar) {
  1/(1 + exp(-5*xvar + 6))
}

curve(ownf, from=-10,to=10)

# ggplot2
library(ggplot2)

qplot(gdp$GDP,gdp$Cons)
qplot(GDP, Cons, data=gdp)

dev.copy(png, "scatter.png")
dev.off()

dev.copy2pdf(file = "pdf_plot.pdf", width = 8, height = 6)

qplot(Year, Cons, data=gdp, geom = c("line","point"))

ggplot(gdp, aes(x=Year,y=GDP))

ggplot(gdp, aes(x=Year,y=GDP)) +
  geom_line() + 
  geom_point()

ggplot(gdp, aes(x=Year,y=GDP)) + 
  geom_line(colour = "red", size=1.5) + 
  geom_point(colour="red", size=4)

ggplot(gdp, aes(x=Year,y=GDP)) + 
  geom_bar(stat="identity")

ggplot(gdp, aes(x=Year,y=GDP)) + 
  geom_bar(stat="identity", 
           fill="orange",
           colour = "blue")

ggplot(gdp, aes(Year)) + 
  geom_line(aes(y=GDP, color = "GDP"), size=1.5) + 
  geom_line(aes(y=Cons, color = "Cons"), size=1.5) + 
  scale_color_manual("Legend", values = c("blue", "orange")) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ggtitle("Some title")

library(reshape2)
meltgdp <- melt(gdp, id="Year")
ggplot(meltgdp, aes(x=Year, 
                    y=value, 
                    color = variable,
                    group=variable)) +
  geom_line(size=1.5)

func1 <- function(x){
  sin(x) + cos(x)
}

plot1 <- ggplot(data.frame(x=c(0, 20)), aes(x=x)) +
  stat_function(fun=func1, geom="line", size=1, color = "red" )	

plot1

func2 <- function(x) {
  2*sin(x)
}

plot2 <- plot1 + stat_function(fun=func2,size=1,color="blue") 

plot2

dev.copy2pdf(file="sines.pdf")

dome <- function(x,y){
  -(x^2 + y^2) 
}

x <- seq(from=-3, to = 3, by=0.1)
y <- seq(from=-3, to = 3, by=0.1)
z <- outer(x,y,dome)
persp(x,y,z,col="blue",theta=90,phi=-10)

library(lattice)
demo(lattice)

library(scatterplot3d)
attach(gdp)
scatterplot3d(Year,Cons,GDP)

