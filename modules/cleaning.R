## Import/Export Functions -------------------------------------------------

# Main Cleaning Function ---------------------------------------------------
cleanData <- function(df, definitions, columns) {
  # dyanmically clean imported csv data using definition tables
  # 4 definition tables are tests, diagnosis, prescriptions, and columns
  # columns table used first to standardize column naming conventions
  # upload a new definition file to change how data are handled
  # WARNING: Do not change existing column names as code is written 
  # specifically for those names (e.g. don't change age to Age or consult_start to something else)
  # you CAN add new column names for those that do not currently exist.
  cat('Cleaning data....\n')
  cat('...rename columns\n')
  columns$replace[columns$replace == "NULL"] <- NA
  # check that column table is character vectors not factors
  if (class(columns$original) == "factor") {
    columns$original <- as.character(columns$original)
    columns$replace <- as.character(columns$replace)
  }
  
  for (col in 1:ncol(df)) {
    # find column name
    n <- names(df)[col]
    # find matching replacement column name
    r <- grep(n, columns$original)[1] # 1 index takes first value if there are multiples
    # assign replacement value to r
    r <- columns$replace[r]
    
    if (!is.null(r)) { # if we found a match, overwrite the original
      names(df)[col] <- r
    }
    else { # otherwise drop in NULL
      names(df)[col] <- NA
    }
    
  }
  # drop other columns
  cat('...drop unnecessary columns\n')
  df <- df[,!is.na(names(df))]
  
  # do we have all the columns we need?
  cat('...check for missing columns\n')
  cols <- unique(columns$replace[!is.na(columns$replace)])
  dataCols <- names(df)
  missingCols <- cols[!(cols %in% dataCols)]
  
  if (length(missingCols) > 0) { # if there are columns missing, this will throw an error
    stop(paste("Missing column(s):", paste(missingCols, collapse=", "),"\nPlease check raw data set and column definition file!"))
  }
  # Now we just have the columns we want. let's start cleaning things up.
  
  # Clean facility names --------------
  # check for inconsistencies with location name first
  cat('...clean facility names\n')
  df$facility <- as.character(df$facility)
  locations <- c('Lacolline', 'Belladere', 'Hinche', 'HSN', 'Boucan Carre')
  
  # search for each location, in list above and standardize naming
  for (l in locations) {
    rows <- agrep(l, df$facility, ignore.case = FALSE, max=list(cost=.1))
    df$facility[rows] <- l
  }
  
  # over write sante fanm and any blanks or NAs with Lacolline
  rows <- agrep("Sante Fanm", df$facility, ignore.case = TRUE)
  df$facility[rows] <- 'Lacolline'
  
  rows <- agrep('Saint Nicolas umf', df$facility, ignore.case=TRUE, max=list(cost=.4))
  df$facility[rows] <- "HSN"
  
  # anything that is blank or N/A is replaced with Lacolline
  df$facility[df$facility %in% c('N/A',"") |
                is.na(df$facility)] <- "Lacolline"
  
  # 
  unknowns <- unique(df$facility[!(df$facility %in% locations)])
  
  # other unknown site names will remain
  df$facility[!(df$facility %in% locations)] <- paste0("Unknown (",paste(unknowns, collapse=','),")")
  
  cat('...standardize dates\n')
  # check dates -----------
  df$date <- parse_date_time(df$date, guess_formats(df$date, c('dm', 'dmy')))
  df <- df[!is.na(df$date),] # kick out NA date rows
  df$date <- as.Date(df$date)
  
  # make two new columns for easier aggregation later on based on date
  df$month <- format(as.Date(df$date), '%Y-%m')
  df$week <- as.Date(cut(df$date, 'week')) # this gives the monday of the week
  
  # check sex
  df$sex[!(df$sex %in% c('M',"F"))] <- 'F' # overwrite any blank values with F
  
  cat('...clean ages\n')
  df$age <- cleanNumeric(df$age, 'm', 12)
  
  
  cat('...calculate wait times\n')
  # Calc wait  times
  df$wait_time <-
    calcMinutes(df$consult_start) - calcMinutes(df$checkin)
  
  cat('...calculate consultation times\n')
  # Consultation Time
  df$consultation_time <-
    calcMinutes(df$consult_end) - calcMinutes(df$consult_start)
  
  # clean travel time
  # clean out minutes- remove any text in column, don't multiply by anything
  cat('...clean travel times column\n')
  df$travel_time <-
    cleanNumeric(df$arrival_time, 'm', 1)
  # clean out hours- remove any remaining values containing 'h' and multiply by 60 to convert to minutes value
  df$travel_time[is.na(df$travel_time)] <-
    cleanNumeric(df$arrival_time[is.na(df$travel_time)],'h', 1 /
                   60)
  
  cat('...count labs and prescriptions\n')
  # Labs and Prescriptions ------------------------
  # count tests and Rx
  tests <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')
  rx <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
  
  
  # When we import, R thinks blank cells are "", overwrite them as NA
  for(col in c(tests, rx)) {
    # overwrite blank cells with NA for all of the above listed columns
    df[,col][df[,col] ==""] <- NA
  }
  
  # count the number of tests and rxs by row and add new count columns
  df$num_tests <- apply(df[,tests], 1, function(a) length(a[!is.na(a)])) # 1 does by row and counts only nonNA values
  df$num_rx <- apply(df[,rx], 1, function(a) length(a[!is.na(a)])) # 1 does by row
  
  
  # rename tests, prescriptions, and diagnoses if those are present, if not this will skip
  if (!missing(definitions)) {
    cat('...standardize labs/tests\n')
    for (col in tests) { # 5 columns we defined on line 466
      for (i in 1:length(definitions$test)) { # go through each original/replace pair
        if (length(definitions$test$Original[i] %in% df[,col]) > 0) { # if that original exists in the data set
          df[,col][df[,col] %in% definitions$test$Original[i]] <- as.character(definitions$test$Replace.with[i]) # overwrite it with the replace value
        } 
        # if it doesn't exist in the dataset, do nothing
      }
    }
    
    cat('...standardize prescriptions\n')
    
    for (col in rx) {
      for (i in 1:length(definitions$prescription)) {
        if (length(definitions$prescription$Original[i] %in% df[,col]) > 0) {
          df[,col][df[,col] %in% definitions$prescription$Original[i]] <- as.character(definitions$prescription$Replace.with[i])
        }
      }
    }
    
    cat('...standardize diagnoses\n')
    
    col <- "diagnosis"
    for (i in 1:length(definitions$diagnosis)) {
      if (length(definitions$diagnosis$Original[i] %in% df[,col]) > 0) {
        df[,col][df[,col] %in% definitions$prescription$Original[i]] <- as.character(definitions$prescription$Replace.with[i])
      }
    }
  }
  # replace diagnosis, tests, and prescriptions with definition docs
  
  cat('Data cleaning completed!') 
  return(df)
}

# Supporting Functions -----------------------------------------------------------------------
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
  vectorNew <- as.numeric(vector)
  
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

calcMinutes <- function(timeVector) {
  # first we need to split the time vectors into component parts
  col1 <-
    strsplit(as.character(timeVector), ":") # hour and min split
  
  for (i in 1:length(col1)) {
    # now we need to split off AM/PM (if it's there)
    split2 <- try(unlist(strsplit(col1[[i]][2]," ")))
    
    # let's convert everything to 24 hour time while we're at it
    if (split2[2] == "PM" &
        length(split2) == 2 & as.numeric(col1[[i]][1]) < 12) {
      col1[[i]][1] <- as.numeric(col1[[i]][1]) + 12
    }
    
    col1[i] <- as.numeric(col1[[i]][1]) * 60 + as.numeric(split2[1])
  }
  return(unlist(col1))
}


