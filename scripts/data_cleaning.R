#### Preamble ####
# Purpose: Clean the raw data, create readable data frames
# Author: Rayhan Walia
# Data: April 2021
# Contact: rayhan.walia@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Must have raw_stock.csv file (inputs/data); from data_load script

#### Workspace setup ####
library(haven)
library(tidyverse)
library(stringr)
library(car)
library(janitor)
library(visdat)
library(gridExtra)
library(reshape2)

# Read in the raw data. 
raw_data <- readr::read_csv("inputs/data/raw_stock.csv")

clean <- raw_data %>% clean_names()

#selecting columns
remove_date = c() #dates to remove: NA observations
data <- clean %>% 
  mutate(date = as.integer(substr(ref_date,1,4))+(as.integer(substr(ref_date,6,7))-1)/12) %>% 
  select(date, toronto_stock_exchange_statistics, value) %>% 
  filter(date >= 1998) %>% #since index information is only available after this year
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, oil and gas, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, Toronto 35 index, closing quotations (May 27 1987=100)") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, financial services, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, utilities, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Standard and Poor's/Toronto Stock Exchange Composite Index, low") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, gold and silver, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Standard and Poor's/Toronto Stock Exchange Composite Index, high") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, metals and minerals, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, paper and forest products, closing quotations") %>% 
  filter(toronto_stock_exchange_statistics != "Toronto Stock Exchange, merchandising, closing quotations")

names(data)[names(data) == 'toronto_stock_exchange_statistics'] = 'index'

for (i in 1:length(data$index)){
  if (is.na(data$value[i])){
    remove_date <- append(remove_date, data$date[i])
  }
}

remove_date <- unique(remove_date)

data <- data %>% 
  filter(!date %in% remove_date)

data <- data.frame(data)

#missing data
vis_miss(data$index)

market <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Composite Index, close")

tsx_sixty <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange 60 Index")

discretionary <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Consumer Discretionary Index")
         
staples <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Consumer Staples Index")

energy <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Energy Index")

financial <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Financial Index")

gold <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Gold Index")

industrial <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Industrial Index")

it <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Information Technology Index")

materials <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Materials Index")

metals <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Diversified Metals and Mining Index")

comm <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Telecommunication Service Index")

utilities <- data %>% 
  filter(index == "Standard and Poor's/Toronto Stock Exchange Canadian Utilities Index")
  
pe <- data %>% 
  filter(index == "Toronto Stock Exchange, price earnings ratio, closing quotations")

dividend <- data %>% 
  filter(index == "Toronto Stock Exchange, stock dividend yields (composite), closing quotations")

#creating relative variables to compare
max_val <- max(market$value)
market <- market %>% 
  mutate(rel_value = value/max_val)

max_val <- max(tsx_sixty$value)
tsx_sixty <- tsx_sixty %>% 
  mutate(rel_value = value/max_val)

max_val <- max(discretionary$value)
discretionary <- discretionary %>% 
  mutate(rel_value = value/max_val)

max_val <- max(staples$value)
staples <- staples %>% 
  mutate(rel_value = value/max_val)

max_val <- max(energy$value)
energy <- energy %>% 
  mutate(rel_value = value/max_val)

max_val <- max(financial$value)
financial <- financial %>% 
  mutate(rel_value = value/max_val)

max_val <- max(gold$value)
gold <- gold %>% 
  mutate(rel_value = value/max_val)

max_val <- max(industrial$value)
industrial <- industrial %>% 
  mutate(rel_value = value/max_val)

max_val <- max(it$value)
it <- it %>% 
  mutate(rel_value = value/max_val)

max_val <- max(materials$value)
materials <- materials %>% 
  mutate(rel_value = value/max_val)

max_val <- max(metals$value)
metals <- metals %>% 
  mutate(rel_value = value/max_val)

max_val <- max(comm$value)
comm <- comm %>% 
  mutate(rel_value = value/max_val)

max_val <- max(utilities$value)
utilities <- utilities %>% 
  mutate(rel_value = value/max_val)

max_val <- max(pe$value)
pe <- pe %>% 
  mutate(rel_value = value/max_val)

max_val <- max(dividend$value)
dividend <- dividend %>% 
  mutate(rel_value = value/max_val)

### testing - will be omitted in final script

total_plot <- rbind(market, tsx_sixty, discretionary, staples, energy, financial, 
               gold, industrial, it, materials, metals, comm, utilities, dividend) #no pe ratio

ggplot(data = total_plot, aes(y=rel_value, x=date))+geom_point(aes(col=index))+
  scale_color_discrete(name = "Index", labels = c("S&P500", "TSX60", "Discretionary", 
                                                  "Staples", "Energy", "Financial", "Gold",
                                                  "Industrial", "IT", "Materials", "Metals", 
                                                  "Communication", "Utilities", "Dividends"))



total <- cbind(market$value,market$date, energy$value,
               industrial$value, materials$value, utilities$value)
colnames(total) <- c('sptsx','date','energy','industrial','materials','utilities')
total <- data.frame(total)

write_csv(total, "outputs/data/data_clean.csv")


total2 <- cbind(market$value,market$date,
               industrial$value, materials$value, utilities$value)
colnames(total2) <- c('sptsx','date','industrial','materials','utilities')

#for summary statistics
#making relative value data
rel_total <- cbind(market$date, market$rel_value, energy$rel_value,
               industrial$rel_value, materials$rel_value, utilities$rel_value)
colnames(rel_total) <- c('date','sptsx','energy','industrial','materials','utilities')
rel_total <- data.frame(rel_total)

write_csv(rel_total, "outputs/data/data_rel.csv")


##plotting relative values of all indices
col_plot <- c('sptsx', 'energy', 'industrial', 'materials', 'utilities')
dlong <- melt(rel_total[,c("date", col_plot)], id.vars="date")

#"value" and "variable" are default output column names of melt()
ggplot(dlong, aes(date,value, col=variable)) +
  geom_point()+geom_line()+
  labs(y='Relative value')
