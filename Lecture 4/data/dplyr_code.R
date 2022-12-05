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

# install.packages("eurostat")
library(eurostat)
library(tidyverse)
library(readxl)

query <- search_eurostat("GDP")

gdp_all <- get_eurostat("nama_10_gdp")

dict <- get_eurostat_dic("na_item")

dict2 <- get_eurostat_dic("geo")

gdp_bg <- gdp_all %>% 
  filter(geo == "BG")

gdp_bg <- gdp_all %>% 
  filter(geo == "BG" & unit == "CLV10_MEUR" & 
           na_item == "B1GQ")

gdp_bg <- gdp_bg %>% 
  mutate(time = as.integer(substr(time, 1, 4))) %>% 
  mutate(nonsense = 2 * values - 1000) %>% 
  arrange(time) %>% 
  mutate(gdp_growth_rate = (values/lag(values) - 1)*100)

gdp_bg <- gdp_bg %>% 
  select(c(time:values, gdp_growth_rate)) 

ggplot(gdp_bg, aes(x = time)) + 
  geom_line(aes(y = gdp_growth_rate)) +
  theme_bw()

gdp_bg <- gdp_bg %>% 
  slice(21, 6:15)

# Untidy data
income_by_source <- read_excel("income_by_source_en.xlsx",
                               sheet = 1, skip = 2, na = c("-", "/"))
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

income_by_source[,3:ncol(income_by_source)] <- sapply(income_by_source[,3:ncol(income_by_source)], as.numeric)

colnames(income_by_source) <- c("region", "source", 2008:(ncol(income_by_source)+2008-3))

income_by_source <- income_by_source %>%
  select(-contains("200"))

income_by_source_Sofia <- income_by_source %>% 
  filter(region == "Sofia") %>% 
  select(-region)
