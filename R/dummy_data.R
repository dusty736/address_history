# This script creates dummy data that can be used to in the creation
# of the address history 

################################################################################
# Import Libraries -------------------------------------------------------------
################################################################################

library(tidyverse)

################################################################################
# Create Dummy Set -----------------------------------------------------------
################################################################################

# Dummy set 1 will consist of:
#   - 1 Unique ID
#   - 4 Address Fields
#   - 2 Date Columns
#   - 10 people with 27 IDs
set.seed(42)
ppt_id <- paste0("ppt_", 
                 str_pad(as.character(sample.int(n=100000000, 
                                               size=10, 
                                               replace=FALSE)),
                        width=9,
                        side='left',
                        pad="0"))

# These addresses are landmarks in the United States of America
ppt_street <- c('400 Broad St', # Space Needle
                '233 S Wacker Dr', # Willis Tower
                '20 W 34th St.', # Empire State Building
                '2 15th St NW', # Washington monument
                '526 Market St', # Liberty Hall
                '301 Mission St', # Millennium Tower
                '3911 Figueroa St', # LA Memorial Coliseum
                'Monument Sq', # Bunker Hill
                '4225 Roosevelt Way NE', # My work
                '1234 Blah Ave S') # Ran out of ideas
                
ppt_city <- c('Seattle',
              'Chicago',
              'New York',
              'Washington DC',
              'Philadelphia',
              'San Francisco',
              'Los Angeles',
              'Boston',
              'Seattle',
              'Bikini Bottom')

ppt_zip <- c('98109',
             '60606',
             '10001',
             '20024',
             '19106',
             '94105',
             '90037',
             '02129',
             '98105',
             '00001')

ppt_state <- c('Washington',
               'Illinois',
               'New York',
               '',
               'Pennsylvania',
               'California',
               'California',
               'Massachusetts',
               'Washington',
               'Atol')

# Put together address data

# Gnerate the number of times each address repeats itself
set.seed(2022)
number_of_addresses <- sample.int(n=10, size=27, replace=TRUE)
address_counts <- as.numeric(table(number_of_addresses))

# Make sure each address is used
stopifnot(length(unique(number_of_addresses)) == 10)

# Create final dataframe
final_id <- c()
final_street <- c()
final_city <- c()
final_zip <- c()
final_state <- c()

for (i in 1:length(address_counts)) {
  n_addr <- address_counts[i] # Select how many times to repeat the address
  
  final_id <- c(final_id, rep(ppt_id[i], n_addr))
  final_street <- c(final_street, rep(ppt_street[i], n_addr))
  final_city <- c(final_city, rep(ppt_city[i], n_addr))
  final_zip <- c(final_zip, rep(ppt_zip[i], n_addr))
  final_state <- c(final_state, rep(ppt_state[i], n_addr))
}

address_df <- data.frame(ppt_id = final_id,
                            street = final_street,
                            city = final_city,
                            zip = final_zip,
                            state = final_state)

# Create timeframe
start_dates <- c('1995-03-15', # 1
                 '2003-06-16',
                 '2002-10-01', # 2
                 '2007-01-02',
                 '',
                 '1999-02-23', # 3
                 '2000-08-02',
                 '2020-04-10',
                 '2000-03-10', # 4
                 '2007-09-12',
                 '',
                 '2017-12-03',
                 '1993-11-09', # 5
                 '1998-12-29',
                 '2006-07-18', # 6
                 '2009-01-31',
                 '2021-09-01',
                 '2021-10-06',
                 '1996-03-15', # 7
                 '2002-08-22',
                 '',
                 '',
                 '2015-04-11',
                 '2004-07-19', # 8
                 '2009-04-01',
                 '2000-01-30', # 9
                 '1998-07-03') # 10

end_dates <- c('2003-06-15', # 1
               '',
               '2007-01-01', # 2
               '2013-05-22',
               '2018-08-11',
               '2000-06-13', # 3
               '2020-04-10',
               '2021-05-05',
               '2003-05-02', # 4
               '2007-11-15',
               '',
               '',
               '1995-04-14', # 5
               '',
               '2009-01-30', # 6
               '2015-07-25',
               '',
               '2022-01-17',
               '2002-08-21', # 7
               '2008-07-25',
               '',
               '',
               '2022-01-17',
               '', # 8
               '2013-10-01',
               '2005-11-30', # 9
               '2012-12-21') # 10

# Add on dates
address_df <- address_df %>% 
   mutate(start_date = as.Date(start_dates, format="%Y-%m-%d"),
          end_date = as.Date(end_dates, format="%Y-%m-%d"))

# create data with just dates and months
alternative_address_df <- address_df %>% 
  mutate(start_month = lubridate::month(start_date),
         start_year = lubridate::year(start_date),
         end_month = lubridate::month(end_date),
         end_year = lubridate::year(end_date)) %>% 
  select(-start_date, -end_date)

# Tests
stopifnot(sum(address_df$end_date < address_df$start_date))

# Create data directory
dir.create(file.path("./data"), showWarnings = FALSE)

# Save Data
write.csv(address_df, "./data/test_data_1.csv", row.names=FALSE)
write.csv(alternative_address_df,  "./data/test_data_2.csv", row.names=FALSE)
