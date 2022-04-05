#### Preamble ####
# Purpose: Load the data from Statistics Canada
# Author: Rayhan Walia
# Data: April 2022
# Contact: rayhan.walian@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - statcanR package

library(statcanR)
library(tidyverse)

data <- statcan_data("10-10-0125-01", "eng")

write_csv(data, "inputs/data/raw_stock.csv")
