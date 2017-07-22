library(magrittr)
library(plyr)
library(lubridate)

# Main Cleaning Function ---------------------------------------------------
cleanData <- function(df, columns, tests, prescriptions, diagnoses) {
  # dyanmically clean imported csv data using definition tables
  # 4 definition tables are tests, diagnosis, prescriptions, and columns
  # columns table used first to standardize column naming conventions
  # upload a new definition file to change how data are handled
  # WARNING: Do not change existing column names as code is written 
  # specifically for those names (e.g. don't change age to Age or consult_start to something else)
  # you CAN add new column names for those that do not currently exist.
  cat('Cleaning data....\n')
  
  df %<>% cleanColumns(columns)
  
  # Now we just have the columns we want. let's start cleaning things up.
  # check dates -----------
  df %<>% cleanDates() 
  
  # Clean facility names --------------
  df %<>% cleanFacility()

  # clean demographics 
  df %<>% cleanDemographics()

  # Clean times
  df %<>% cleanTimes()

  cat('...count labs and prescriptions\n')
  labs <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')
  rx <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
  
  df %<>% calcQuantities(labs, 'num_tests')
  df %<>% calcQuantities(rx, 'num_rx')
  
  
  # rename tests, prescriptions, and diagnoses if one is missing, it will skip
  if (!missing(tests)) {
    cat('...standardize labs/tests\n')
    df %<>% cleanCharacter(labs, tests)
  }
  if (!missing(prescriptions)) {
    cat('...standardize prescriptions\n')
    df %<>% cleanCharacter(rx, prescriptions)
    
  }
  if (!missing(diagnoses)) {
    cat('...standardize diagnoses\n')
    df %<>% cleanCharacter('diagnosis', diagnoses)
  }

  cat('Data cleaning completed!') 
  return(df)
}

# Supporting Functions -----------------------------------------------------------------------
cleanColumns <- function(df, columns) {
  # Rename columns and drop other columns based on a columns definition file
  # with one column stating possible original values and the second defining 
  # replacement values to standardize.  This is necessary because of the 
  # inconsistency of the source data file. 
  # Function will kick an error if it fails to find all names defined in
  # the replace column of the definition file. 
  #
  # Ex.
  # > partially_clean <- cleanColumns(raw_data, columns)
  # ...rename columns
  # ...drop unnecessary columns
  # ...check for missing columns  Passed!
  
  
  cat('...rename columns\n')
  # Convert the string NULL into an actual NA value
  columns$replace[columns$replace == "NULL"] <- NA
  
  # check that column table is character vectors not factors
  if (class(columns$original) == "factor") {
    columns %<>% apply(2, as.character) %>% as.data.frame()
  }
  
  # Make key/value pair for what columns we want to keep 
  # revalue() will look for the key and replace it with the value
  # for column names.  This is to standardize how some of the messy data 
  # was received from the field. 
  keep_columns <- columns$replace # the values
  names(keep_columns) <- columns$original # the key we're looking for
  
  # Pipe the names of the dataframe to revalue and use keep_columns to 
  # rename the columns.  Result is placed back into the column names 
  # (using the %<>% operator allows for this)
  # revalue might kick some warnings about values not found, that's ok.
  # there are more potential columns than output columns that we want. 
  names(df) %<>% revalue(keep_columns, warn_missing = F)
  
  
  ### drop other columns
  cat('...drop unnecessary columns\n')
  
  # Only keep the non-NA columns we find in keep_columns.  Everything else
  # will be dropped
  keep_columns %<>% .[!is.na(.)] %>% unique() # unique values of the final column names we want
  
  df <- df[, names(df) %in%  keep_columns] 
  
  # do we have all the columns we need?
  cat('...check for missing columns\t')
  
  # Compare the names of the data frame vs the columns we know we want
  # Stop the cleaning process if not all columns are found.  In that case,
  # the columns definition table may need to be updated or reviewed.
  missingCols <- keep_columns[!(keep_columns %in% names(df))]
  
  if (length(missingCols) > 0) { # if there are columns missing, this will throw an error
    cat('Failed!\n')
    stop(paste("Missing column(s):", paste(missingCols, collapse=", "),"\nPlease check raw data set and column definition file!"))
  }
  else ('Passed!\n')
  
  return(df)
}

cleanFacility <- function(df) {
  # Since the facility names tend to have quite a lot of typos and 
  # variation on how the facilities were described. This could
  # have been expanded to use a similar definitions file, but we 
  # left it as a hard coded process.  
  df$facility <- as.character(df$facility)
  # save original facility name
  df$original_facility <- df$facility
  
  # check for inconsistencies with location name first
  cat('...clean facility names\n')

  locations <- c('Lacolline', 'Belladere', 'Hinche', 'HSN', 'Boucan Carre')
  
  # search for each location, in list above and standardize naming
  # agrep uses levenschtein distance to find the closest match with a strict
  # cost penalty (only matches we're pretty sure get overwritten)
  for (l in locations) {
    rows <- agrep(l, df$facility, ignore.case = FALSE, max=list(cost=.1))
    df$facility[rows] <- l
  }
  
  # One data clear consistently entered "Sante Fanm" instead of Lacolline
  # So we standardized those. 
  rows <- agrep("Sante Fanm", df$facility, ignore.case = TRUE)
  df$facility[rows] <- 'Lacolline'
  
  # HSN (Hopital St. Nicolas) had some similar issues with typos in 
  # facility names. Based on the input data, we found this to work best. 
  rows <- agrep('Saint Nicolas umf', df$facility, ignore.case=TRUE, max=list(cost=.4))
  df$facility[rows] <- "HSN"
  
  # over write sante fanm and any blanks or NAs with Lacolline
  # this was based on a discussion with the researcher and our 
  # investigation into the source data.  Due to the high number of Maternal Health
  # consultations and consistent missing names from Lacolline, we decided to 
  # blanket overwrite empty facility names with Lacolline. 
  df$facility[df$facility %in% c('N/A',"") |
                is.na(df$facility)] <- "Lacolline"
  
  # Mark all other entered names as "Unknown (List of facility names we couldn't match)" so we don't lose them
  # 
  unknowns <- unique(df$facility[!(df$facility %in% locations)])
  # Overwrite those records that matched. 
  df$facility[!(df$facility %in% locations)] <- paste0("Unknown (",paste(unknowns, collapse=','),")")

  cat('Cleaning Comparison (original vs standardized):\n')
  print(table(df$original_facility, df$facility))
  return(df)
}


cleanNumeric <- function(vector, search, reduce = 1, verbose=F) {
  # for given dataframe, find values within stated column with a specified
  # character value.  Strip numeric values out and divide by reduce object (default 1)
  # creates new column named columnOld with original values, new values are written into current column.
  # Returns dataframe object.
  
  # Ex.
  # > d <- cleanNumeric(d, 'age', 'm', reduce = 12) # find values in the age column with 'm' (months) divides by 12
  # > d <- cleanNumeric(d, 'age', 'w', reduce = 52) # find weeks
  # currently does not account for days (searching for 'y' would return anything with days)
  vector <- as.character(vector)
  vectorNew <- suppressWarnings(as.numeric(vector))
  
  locations <- grep(search, vector, ignore.case = TRUE)
  # find just the numeric vals. assuming number comes first
  for (na in locations) {
    # look at each value that came out with text of 'y'
    end <-
      nchar(vector[na]) # total number of characters in current value
    # loop until find nonNA value when converting to numeric
    # starts from right and moves left. stops when end == 0 and ignores
    while (is.na(suppressWarnings(as.numeric(substr(vector[na], 1, end))))) {
      end <- end - 1
      if (end <= 0) {
        if (verbose==T) {
          warning(paste("No number found for value:", vector[na]))
          
        }
        break
      }
    }
    vectorNew[na] <- as.numeric(substr(vector[na], 1, end)) / reduce
  }
  
  # return cleaned vector
  if (verbose==T) {
    cat(
      'Search:', search, '\nCleaned Values:',vector[locations],'\nReturned values:',vectorNew[locations],'\n\n'
    )
  }
  
  return(vectorNew)
}

cleanCharacter <- function(df, columns, definitions) {
  # Cleans columns of string values using a definitions dataframe
  # defined by the user similar to the columns definition file
  # Function will attempt to replace an Original key with a Replace.with value
  # as defined in a row of the definition dataframe. 
  # returns the full dataframe with cleaned columns
  
  replace_map <- definitions$replace
  names(replace_map) <- definitions$original
  
  df %<>% apply(2, function(x) revalue(x, replace_map, warn_missing = F))
  
  return(df)
  
}

cleanDates <- function(df, formats = c('mdy', 'dm', 'dmy'), start_date = '2015-03-01', end_date='2015-10-31') {
  # Due to differences in how dates were entered, column needs to be cleaned.
  # Excel formatted some as dates, which starts number dates in 1900-01-01, and 
  # others still came in as characters.  We need to standardize that.
  # start_date and end_date set bounds for acceptable dates.  If they fall outside of 
  # those bounds, we attempt to swap month and day since we most likely guessed the wrong
  # format while trying to intuit date formats. 
  # Also creates grouping columns for month and weeks for easier analysis later
  # returns cleaned dataframe with updated columns
  
  cat('...standardize dates\n')
  
  # make an original column in case we wonder how they were cleaned
  df$original_date <- df$date
  
  # Just find the numeric values right now and convert those to the proper date format
  numeric_dates <- suppressWarnings(as.numeric(df$date)) # we know some NAs will be introduced
  numeric_dates %<>% as.Date(origin='1900-01-01')

  alternate_format_dates <- parse_date_time(df$date[is.na(numeric_dates)], orders=formats)
  
  # Dates have been parsed, stick them back together. 
  df$date <- numeric_dates 
  df$date[is.na(numeric_dates)] <- alternate_format_dates
  
  # Check that the dates are within the proper bounds.  It's possible that 
  # as we're guessing the format, we switch something like 6/1/2015 (mdy) as 1/6/2015 (dmy) 
  # Switch the month and day but keep the year. We still might lose some dates with
  # years that are outside of the bounds, but this will clean most properly.
  df$date[which(df$date < start_date)] %<>% ydm()
  df$date[which(df$date > end_date)] %<>% ydm()
  
  # drop any that are still NA
  # df %<>% .[!is.na(.$date),]
  
  # make two new columns for easier aggregation later on based on date
  df$month <- format(as.Date(df$date), '%Y-%m')
  df$week <- as.Date(cut(df$date, 'week')) # this gives the monday of the week
  
  return(df)  
}

cleanDemographics <- function(df, impute_columns=c('facility', 'diagnosis')) {
  # Clean ages and sex values.  impute_columns is used to find the most common 
  # sex for the combination of values and overwrites NA values.  This could breakdown
  # if the combinations get too specific and there are no other values than NA for 
  # sex in that specific combination. 
  # returns full data frame
  cat('...clean ages\n')
  cat('Original ages:\n')
  df$original_age <- df$age
  print(as.data.frame(table(df$age)))
  df$age <- cleanNumeric(df$age, 'm', 12)
  cat('\nCleaned ages:\n')
  print(as.data.frame(table(df$age)))
  
  # check sex and impute missing
  cat('Columns used for sex impute:', paste(impute_columns, collapse=', '))
  cat('Showing facility vs sex for simplicity of printing.\n')
  cat('Current sex values:\n')
  df$original_sex <- df$sex
  print(table(df[,c('facility', 'sex')], useNA = "always"))
  
  # first there are some that might have a space or lowercase
  df$sex[grep('F', df$sex, ignore.case = T)] <- "F"
  df$sex[grep('M', df$sex, ignore.case = T)] <- "M"
  
  # Assign the most frequent sex based on the facility 
  impute_table <- as.data.frame(table(df[,c('sex',impute_columns)]))
  
  # Impute values for missing sex based on the columns named in 
  # impute_columns.  Default is facility and diagnosis
  for (i in which(is.na(df$sex))) {
    # find the impute_column values for this instance
    impute_vals <- df[i, impute_columns]
    
    # find all of the rows that match and then count the sex values 
    sex <- df$sex[apply(df[,impute_columns], 1, function(x) x== impute_vals)] %>% table() 
    
    # insert the most common value replacing the NA
    df$sex[i] <- names(sex[sex == max(sex)])

  }
  cat('Cleaned sex values:\n')

  print(table(df$facility, df$sex, useNA = 'always'))
  return(df)  

}

cleanTimes <- function(df) {
  cat('...calculate wait times\n')
  # Calc wait  times, because the times are registered in Excel, it stores them as decimals
  # add it to today's date so we can do some math against them
  for (i in c('checkin', 'consult_start', 'consult_end')) {
    df[,i] <- suppressWarnings(as.POSIXct(as.numeric(df[,i]) + Sys.Date())) # we know some might not come across
  }
  
  df$wait_time <- suppressWarnings(as.numeric(df$consult_start) - as.numeric(df$checkin)) / 60 # results are in seconds
  
  cat('...calculate consultation times\n')
  # Consultation Time
  df$consultation_time <- suppressWarnings(as.numeric(df$consult_end) - as.numeric(df$consult_start)) / 60 # results are in seconds
  
  # clean travel time
  # clean out minutes- remove any text in column, don't multiply by anything
  cat('...clean travel times column\n')
  df$travel_time <- cleanNumeric(df$arrival_time, 'm', 1)
  # clean out hours- remove any remaining values containing 'h' and multiply by 60 to convert to minutes value
  df$travel_time[is.na(df$travel_time)] <- cleanNumeric(df$arrival_time[is.na(df$travel_time)],'h', 1 / 60)
  
  return(df)
}

calcQuantities <- function(df, cols, col_name) {
  # Count the number of non NA cells in a set of columns 
  # adds a new column name as defined by col_name with the count
  
  # When we import, R thinks blank cells are "", overwrite them as NA
  # overwrite blank cells with NA for all of the above listed columns
  df[,cols][df[,cols] ==""] <- NA
  
  # count the number of tests and rxs by row and add new count columns
  df[[col_name]] <- apply(df[,cols], 1, function(a) length(a[!is.na(a)])) # 1 does by row and counts only nonNA values
  
  return(df)
}

