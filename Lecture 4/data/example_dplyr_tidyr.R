# Clear workspace
rm(list = ls())

# Clear plots
# dev.off(dev.list()["RStudioGD"])

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc)

# Check if packages are installed and install if not
packages_to_check <- c("tidyverse", "openxlsx", "readxl")
logical_check <- packages_to_check %in% installed.packages()[,"Package"]
packages_to_install <- packages_to_check[!logical_check]
if(length(packages_to_install) > 0){
  install.packages(packages_to_install)
}

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(openxlsx)
library(readxl)
library(eurostat)

# Income by source
income_by_source <- read_excel("income_by_source_en.xlsx", 
                               sheet = 1, 
                               skip = 2, 
                               na = c("-", "/"))

colnames(income_by_source)[1:2] <- c("region", "source")

income_by_source <- income_by_source %>%
  mutate_if(is.character, 
            str_replace_all,
            pattern = "\\(",
            replacement = "") %>%
  mutate_if(is.character, 
            str_replace_all,
            pattern = "\\)",
            replacement = "") %>%
  select(region, source, contains("per capita")) %>%
  slice(1:560)

  income_by_source <- income_by_source %>% 
    mutate(`Average per capita - BGN...34` = 
             as.numeric(`Average per capita - BGN...34`))

# Distribution of children (aged less than 18) by 
# educational attainment level of their parents and income group
children_lt18_all <- get_eurostat("ilc_lvps25", time_format = "date")

children_lt18_bg <- children_lt18_all %>%
  filter(geo == "BG" & age == "Y_LT18" & incgrp != "TOTAL") %>%
  select(-c(unit, age, geo))

children_lt18_bg_bmd60 <- children_lt18_bg %>%
  filter(incgrp == "B_MD60") %>%
  select(-incgrp) %>%
  mutate(time = substr(time,1,4)) %>%
  spread(isced11, values) %>%
  select(-contains("200"))


