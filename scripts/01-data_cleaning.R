#### Preamble ####
# Purpose: Clean the data from StatCan
# Author: Rayhan Walia
# Data: April 2021
# Contact: rayhan.walia@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Must have raw_stock.csv file (inputs/data) from data_load script

#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- readr::read_csv("inputs/data/raw_stock.csv")




         