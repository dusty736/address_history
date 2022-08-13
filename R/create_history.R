# This script creates the address history

################################################################################
# Import Libraries -------------------------------------------------------------
################################################################################

library(tidyverse)

################################################################################
# Read in Data -----------------------------------------------------------------
################################################################################

address_file <- "./data/test_data_1.csv" # To replace with user input
address_data <- read.csv(address_file)

################################################################################
# Address Cleaning -------------------------------------------------------------
################################################################################

# Collapse redundant data
address_data <- address_data %>% 
  mutate(full_address = paste(street, city, zip, state)) %>% 
  mutate(lead_ppt_id = lead(ppt_id),
         lead_full_address = lead(full_address),
         lead_start_date = lead(start_date),
         lead_end_date = lead(end_date)) %>% 
  mutate(address_dist = stringdist::stringdist(full_address,
                                               lead_full_address)) %>% 
  mutate(end_date = ifelse(ppt_id == lead_ppt_id & address_dist == 0,
                           lead_start_date,
                           end_date),
         updated = ifelse(ppt_id == lead_ppt_id & address_dist == 0,
                          1,
                          0)) %>% 
  select(-lead_ppt_id, -lead_full_address, 
         -lead_start_date, -lead_end_date, 
         -address_dist)



















