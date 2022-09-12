# This script creates the address history

################################################################################
# Import Libraries -------------------------------------------------------------
################################################################################

library(tidyverse)

################################################################################
# Source Files -----------------------------------------------------------------
################################################################################

source("./R/functions.R")

################################################################################
# Read in Data -----------------------------------------------------------------
################################################################################

address_file <- "./data/test_data_1.csv" # To replace with user input
address_data <- read.csv(address_file)

################################################################################
# Address Cleaning -------------------------------------------------------------
################################################################################

# Format Data
address_data <- format_data(address_data, "ppt_id")

# Remove Duplicate Rows
address_data <- dedup_addresses(address_data)

# Set min and max dates for first and last addresses
address_data <- set_beginning_end(address_data)

# Backfill fill missing gaps
address_data <- fill_backward(address_data)

# Forward fill missing gaps
address_data <- fill_forward(address_data)

# Fill blocks of missing data
address_data <- fill_block(address_data)

# Remove potential date overlap
address_data <- remove_overlap(address_data)

# Fill date gaps if wanted
address_data <- fill_dates(address_data)





