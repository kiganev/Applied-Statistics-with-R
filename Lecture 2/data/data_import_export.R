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

x <- c(1,2,3,4,5,6,7,8,9)

x <- scan()

y <- scan(what = character())

Age <- c(19,22,21,23,20)
Major <- c("Ec", "Ec", "BA", "BA", "Ec")
Degree <- c("BSc","MSc","BSc","MSc","BSc")
FName <- c("John","Mary","Maria","Juan","Jackie")
LName <- c("Hopkins","Jane","Curie","Carlos","Chan")

Students <- data.frame(FName,LName,Age,Major,Degree)

Students <- edit(Students)

Students <- fix(Students)

save(Students, file = "students.RData")

rm(Students)

load("students.RData")

covid_data <- read.table("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                         sep = ",",
                         header = T)


covid_data$date <- as.Date(covid_data$date)

book1 <- read.csv2("Book1.csv")


# Another example, 
# download gold prices from https://datahub.io/AcckiyGerman/gold-prices
download.file("https://raw.githubusercontent.com/datasets/gold-prices/master/data/monthly.csv",
              destfile = "gold_monthly.csv",
              method = "libcurl")

gold_price <- read.csv("gold_monthly.csv")
gold_price$Date <- paste0(gold_price$Date,"-01") # Day needed for Date format
gold_price$Date <- as.Date(gold_price$Date)

# Employees data, NSI of Bulgaria
library(readxl)
employees_bg <- read_excel("Labour_2.1.1_EN.xls", 
                           sheet = "2020NaceRev2",
                           skip = 4, na = "x")

colnames(employees_bg)[1] <- "Economic Activity"

class(employees_bg)

library(openxlsx)
bnb_data <- read.xlsx("s_ms_monetary_survey_a01_en.xlsx", 
                      startRow = 2)

bnb_data <- bnb_data[-c(54:56),]

library(foreign)
spss_data <- read.spss("survey.sav", to.data.frame = T)

library(haven)
spss_data2 <- read_spss("survey.sav")

# Write csv
write.csv(spss_data2, "spss_data.csv")

# Write xlsx
library(writexl)
write_xlsx(spss_data2, "excel1.xlsx")

write.xlsx(spss_data2, "excel2.xlsx")
