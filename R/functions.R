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
  
  # Order
  df %>% 
    arrange(id_col, start_date, end_date)
}













