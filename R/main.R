## ---- load
source("R/theme.R")
library(tidyverse)
library(sugrrants)
library(tsibble)
library(gravitas)

smart_meter50 <- read_rds("~/Documents/paper-gravitas/data/sm_cust50.rds") %>%  
  select(customer_id, 
         reading_datetime,
         general_supply_kwh, 
         everything())

smart_meter50
