# This script contains needed functions

#' Title
#'
#' @param text 
#'
#' @return
#' @export
#'
#' @examples
address_normalize <- function(text) {
  # Get text in all uppercase
  text <- stringr::str_squish(as.character(text))
  text <- toupper(text)
  
  # Make sure common language is being used
  text <- gsub(" AVENUE", " AVE", text)
  text <- gsub(" STREET", " ST", text)
  text <- gsub(" ROAD", " RD", text)
  text <- gsub(" CIRCLE", " CIR", text)
  text <- gsub(" PLACE", " PL", text)
  text <- gsub(" DRIVE", " DR", text)
  text <- gsub(" LANE", " LN", text)
  text <- gsub(" BOULEVARD", " BLVD", text)
  text <- gsub(" TERRACE", " TER", text)
  text <- gsub(" COURT", " CT", text)
  text <- gsub(" TRAIL", " TR", text)
  text <- gsub("^S\\s|\\sS$|\\sSOUTH\\s|\\sS\\s|\\sSOUTH$", " S ", text)
  text <- gsub("^E\\s|\\sE$|\\sEAST\\s|\\sE\\s|\\sEAST$",  " E ",  text)
  text <- gsub("^W\\s|\\sW$|\\sWEST\\s|\\sW\\s|\\sWEST$",  " W ",  text)
  text <- gsub("^N\\s|\\sN$|\\sNORTH\\s|\\sN\\s|\\sNORTH$", " N ", text)
  text <- gsub("^SW\\s|\\sSW$|^SOUTHWEST\\s|\\sSOUTHWEST$|\\sSOUTHWEST\\s|\\sSW\\s|\\sSOUTHWEST\\s", " SW ", text)
  text <- gsub("^SE\\s|\\sSE$|^SOUTHEAST\\s|\\sSOUTHEAST$|\\sSOUTHEAST\\s|\\sSE\\s|\\sSOUTHEAST\\s", " SE ", text)
  text <- gsub("^NW\\s|\\sNW$|^NORTHWEST\\s|\\sNORTHWEST$|\\sNORTHWEST\\s|\\sNW\\s|\\sNORTHWEST\\s", " NW ", text)
  text <- gsub("^NE\\s|\\sN$E|^NORTHEAST\\s|\\sNORTHEAST$|\\sNORTHEAST\\s|\\sNE\\s|\\sNORTHEAST\\s", " NE ", text)
  text <- gsub(" APT\\s.+|\\s#.+", " ", text)
  text <- gsub("APARTMENTS", " ", text)
  
  return(text)
}


#' Title
#'
#' @param text 
#' @param as_abbr 
#'
#' @return
#' @export
#'
#' @examples
normalize_state <- function(text, as_abbr=TRUE) {
  if (as_abbr == TRUE) {
    text <- tools::toTitleCase(text)
    text <- plyr::mapvalues(text, state.name, state.abb, warn_missing = FALSE)
  } else {
    text <- toupper(text)
    text <- plyr::mapvalues(text, state.abb, state.name, warn_missing = FALSE)
  }
  
  return(text)
}


#' Title
#'
#' @param date_field 
#'
#' @return
#' @export
#'
#' @examples
normalize_dates <- function(date_field) {
  
  date_field[date_field == ""] <- NA
  
  formatted_date_field <- as.Date(date_field,
                                  format = "%Y-%m-%d",
                                  origin = "1970-01-01")
  
  if (sum(is.na(formatted_date_field)) > sum(is.na(date_field))) {
    stop("Date column must be in format YYYY-MM-DD")
  }
  
  return(date_field)
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
check_column_names <- function(df) {
  df_column_names <- colnames(df)
  
  if (!"address" %in% df_column_names) {
    stop("Address text field must be named 'address'")
  }
  
  if (!"city" %in% df_column_names) {
    stop("City text field must be named 'city'")
  }
  
  if (!"zip" %in% df_column_names) {
    stop("Zipcode text field must be named 'zip'")
  }
  
  if (!"state" %in% df_column_names) {
    stop("State text field must be named 'state'")
  }
  
  if (!"start_date" %in% df_column_names) {
    stop("Start Date field must be named 'start_date'")
  }
  
  if (!"end_date" %in% df_column_names) {
    stop("Start Date field must be named 'end_date'")
  }
}


#' Title
#'
#' @param df 
#' @param id_col 
#'
#' @return
#' @export
#'
#' @examples
format_data <- function(df, id_col) {
  # Check that the column names are correct
  check_column_names(df)
  
  # Normalize the text
  df['address'] <- address_normalize(df[['address']])
  
  # Normalize state
  df['state'] <- normalize_state(df[['state']])
  
  # Format dates
  df['start_date'] <- normalize_dates(df[['start_date']])
  df['end_date'] <- normalize_dates(df[['end_date']])
  
  df <- df %>% 
    mutate(start_date = as.Date(start_date, origin = "1970-01-01"),
           end_date = as.Date(end_date, origin = "1970-01-01"),)
  
  # Order
  df %>% 
    arrange(id_col, start_date, end_date)
  
  return(df)
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
dedup_addresses <- function(df) {
  
  # Make sure the data is structured chronologically
  df <- df %>% 
    arrange(ppt_id, start_date)
  
  # Create running counts of addresses
  df <- df %>% 
    group_by(ppt_id, grp = with(rle(paste(address, city, state, zip)), rep(seq_along(lengths), lengths))) %>%
    mutate(Counter = max(seq_along(grp))) %>%
    ungroup() %>%
    select(-grp) %>% 
    mutate(compare_text = paste(ppt_id, address, city, state, zip),
           last_compare_text = lag(paste(ppt_id, address, city, state, zip)))
  
  # Add column to count address appearance count
  comb_vec <- c()
  comb_count <- c()
  output <- c()
  
  for (row in 1:dim(df)[1]) {
    tmp <- df[row, 'compare_text'][[1]]
    tmp_prev <- df[row, 'last_compare_text']
    
    if (!tmp %in% comb_vec) {
      comb_vec <- c(comb_vec, tmp)
      comb_count[eval(tmp)] <- 1
      output <- c(output, 1)
    } else if (tmp %in% comb_vec & tmp != tmp_prev) {
      comb_count[eval(tmp)] <- comb_count[eval(tmp)] + 1
      output <- c(output, comb_count[tmp])
    } else {
      output <- c(output, comb_count[tmp])
    }
  }
  
  df <- df %>% 
    mutate(appearance_count = output) %>% 
    select(-compare_text, -last_compare_text)
  
  df <- df %>% 
    group_by(ppt_id, address, city, state, zip, Counter, appearance_count) %>% 
    summarize(start_date = min(start_date, na.rm=TRUE),
              end_date = max(end_date, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(ppt_id, start_date)
  
  return(df)
}

#' Title
#'
#' @param df 
#' @param min_date 
#' @param max_date 
#'
#' @return
#' @export
#'
#' @examples
set_beginning_end <- function(df, min_date = "1960-01-01", max_date = "9999-12-31") {
  # Add column for consecutive ppt rows
  df <- df %>% 
    arrange(ppt_id, start_date) %>% 
    mutate(consecutive_ppt = sequence(rle(as.character(df$ppt_id))$lengths),
           comb_addr = paste(address, city, state, zip))
  
  unique_address_counts <- df %>% 
    group_by(ppt_id) %>% 
    summarize(unique_addresses=n())
  
  # Handle the beginning and end of the timeframe
  df <- df %>% 
    mutate(consecutive_ppt = sequence(rle(as.character(df$ppt_id))$lengths)) %>% 
    left_join(., unique_address_counts[c('ppt_id', 'unique_addresses')], by=c('ppt_id')) %>% 
    mutate(start_date = as.Date(ifelse(consecutive_ppt == 1, as.Date(min_date, format='%Y-%m-%d'), start_date), format='%Y-%m-%d', origin='1970-01-01'),
           end_date = as.Date(ifelse(consecutive_ppt == unique_addresses, as.Date(max_date, format='%Y-%m-%d'), end_date), format='%Y-%m-%d', origin='1970-01-01'))
  
  return(df)
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fill_backward <- function(df) {
  df <- df %>% 
    mutate(lead_ppt = lead(ppt_id),
           lead_start_date = lead(start_date),
           lagged_ppt = lag(ppt_id),
           lagged_end_date = lag(end_date)) %>% 
    mutate(start_date = as.Date(ifelse(ppt_id == lagged_ppt & (is.na(start_date) | is.infinite(start_date)) & !is.na(lagged_end_date),
                                       lagged_end_date + 1,
                                       start_date),
                                origin="1970-01-01"))
  
  return(df)
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fill_forward <- function(df) {
  df <- df %>% 
    mutate(lead_ppt = lead(ppt_id),
           lead_start_date = lead(start_date),
           lagged_ppt = lag(ppt_id),
           lagged_end_date = lag(end_date)) %>% 
    mutate(end_date = as.Date(ifelse(ppt_id == lead_ppt & (is.na(end_date) | is.infinite(end_date)) & !is.na(lead_start_date),
                                     lead_start_date - 1,
                                     end_date),
                              origin="1970-01-01"))
  
  return(df)
}


#' Title
#'
#' @param df 
#' @param max_date 
#'
#' @return
#' @export
#'
#' @examples
fill_block <- function(df, max_date="9999-12-31") {
  df <- df %>% 
    dplyr::select(ppt_id, address, city, state, zip, start_date, end_date) %>% 
    mutate(start_date = as.Date(ifelse(is.infinite(start_date), as.Date(NA), start_date),
                                origin="1970-01-01"),
           end_date = as.Date(ifelse(is.infinite(end_date), as.Date(NA), end_date),
                              origin="1970-01-01")) %>% 
    mutate(na_count = is.na(start_date) + is.na(end_date),
           is_gap = is.na(start_date) | is.na(end_date)) %>% 
    group_by(ppt_id, grp = with(rle(is_gap), rep(seq_along(lengths), lengths))) %>%
    mutate(counter = seq_along(grp),
           max_counter = max(seq_along(grp)),
           min_date = min(start_date, na.rm=TRUE),
           max_date = as.Date(max(ifelse(end_date == max_date, Sys.Date(), end_date),
                                  na.rm=TRUE),
                              origin="1970-01-01")) %>%
    ungroup() %>%
    dplyr::select(-grp) %>% 
    mutate(date_sum = as.numeric(min_date) + as.numeric(max_date),
           date_diff = as.numeric(max_date) - as.numeric(min_date)) %>% 
    # Fill small gaps
    mutate(end_date = as.Date(ifelse(is_gap == TRUE & max_counter == 2 & is.na(end_date),
                                     date_sum / 2,
                                     end_date),
                              origin='1970-01-01'),
           start_date = as.Date(ifelse(is_gap == TRUE & max_counter == 2 & is.na(start_date),
                                       date_sum / 2 + 1,
                                       start_date),
                                origin='1970-01-01')) %>% 
    # big gaps
    mutate(start_date = as.Date(ifelse(na_count == 2,
                                       min_date + (counter * date_diff / max_counter),
                                       start_date),
                                origin="1970-01-01"),
           end_date = as.Date(ifelse(na_count == 2,
                                     min_date + ((counter + 1) * date_diff / max_counter),
                                     end_date),
                              origin="1970-01-01"))
  
  # Backfill Fill Gaps
  df <- fill_backward(df)
  
  # Forward Fill Gaps
  df <- fill_forward(df)
  
  # Select Columns
  df <- df %>% 
    dplyr::select(ppt_id,
                  address,
                  city,
                  state,
                  zip,
                  start_date,
                  end_date)
  
  return(df)
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
remove_overlap <- function(df) {
  df <- df %>% 
    mutate(row_number = row_number(),
           lead_ppt = lead(ppt_id),
           lead_start_date = lead(start_date)) %>% 
    mutate(end_date = as.Date(ifelse(end_date == lead_start_date & row_number != dim(address_data)[1],
                                     as.numeric(end_date - 1),
                                     end_date),
                              origin="1970-01-01")) %>% 
    dplyr::select(-row_number, -lead_ppt, -lead_start_date)
  
  return(df)
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fill_dates <- function(df) {
  df <- df %>% 
    mutate(row_number = row_number(),
           lead_ppt = lead(ppt_id),
           lead_start_date = lead(start_date)) %>% 
    mutate(forward_diff = ifelse(row_number != dim(df)[1] & ppt_id == lead_ppt & as.numeric(lead_start_date) - as.numeric(end_date) > 1,
                                 as.numeric(lead_start_date) - as.numeric(end_date),
                                 NA)) %>% 
    mutate(midpoint = as.Date(ifelse(!is.na(forward_diff),
                                     (as.numeric(end_date) + as.numeric(lead_start_date)) / 2,
                                     NA),
                              origin="1970-01-01")) %>% 
    mutate(lagged_midpoint = lag(midpoint)) %>% 
    mutate(end_date = as.Date(ifelse(!is.na(midpoint),
                                     midpoint,
                                     end_date),
                              origin="1970-01-01"),
           start_date = as.Date(ifelse(!is.na(lagged_midpoint),
                                       lagged_midpoint + 1,
                                       start_date),
                                origin="1970-01-01")) %>% 
    mutate(lead_start_date = lead(start_date)) %>% 
    mutate(date_diff = ifelse(ppt_id == lead_ppt,
                              as.numeric(lead_start_date) - as.numeric(end_date),
                              NA)) %>% 
    dplyr::select(ppt_id,
                  address,
                  city,
                  state,
                  zip,
                  start_date,
                  end_date)
  
  return(df)
}





