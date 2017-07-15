# Import/Export Functions for dealing with Excel source files and the SQlite database
importExcel <- function(filename, sheet = NULL, range = NA) {
  # import excel file into object
  wb <- loadWorkbook(filename)
  if (is.null(sheet) &
      length(getSheets(wb)) == 1) {
    # if no sheet specified and only one sheet exists use that one
    sheet <- getSheets(wb)
  } else if (is.null(sheet) &
             length(getSheets(wb)) > 1) {
    # if no sheet specified and multiple exist, prompt which one
    cat("\rMultiple worksheets found!  Please specify which one to load.")
    sheets <- getSheets(wb)
    
    l <- paste0(sheets, collapse = ", ")
    cat('\nAvailable worksheets:', l)
    sheet <- readline(prompt = 'Specify which to use: ')
  } # if sheet is specified that will be used.
  if (is.na(range)) {
    sheet <- readWorksheet(wb, sheet)
  } else {
    sheet <- readWorksheet(wb, sheet, region = range)
  }
  return(sheet)
}

exportPlot <- function(plot, filename, plotWidth=700, plotHeight=440, workbook=NA, plotLocation=NA, plotFolder='plots/') {
  if (!file.exists("plots") & plotFolder=='plots/') {
    dir.create(file.path("./plots"))
  }
  # initalize png device
  file <- file.path(paste0(plotFolder,filename,".png"))
  png(filename = file, width= plotWidth, height=plotHeight)
  # print the plot to the device
  print(plot)
  # close the device
  dev.off()
  
  if (!missing(workbook) & !is.na(plotLocation)) {
    cat('Exporting image to workbook\n')
    createName(wb, filename, formula= plotLocation, overwrite=TRUE)
    addImage(wb, filename = file, name = filename, originalSize = TRUE)
  } 
  else {
    cat('Exporting image to file only\n')
  }
}

exportData <- function(wb, summaryData, filteredData= NULL, originalData= NULL) {
  #   filename <- paste0(filename, '.xlsx')
  #   wb <- loadWorkbook(filename, create=TRUE)
  start <- 1
  cat('Exporting Summary Data\n')
  createSheet(wb, 'Summary')
  for (i in 1:length(summaryData)) {
    cat(names(summaryData)[i],'\n')
    exportPlot(summaryData[[i]]$plot, # plot object
               gsub(" ", "", names(summaryData)[i]), # name for file
               workbook=wb, # workbook object
               plotLocation=paste0('Summary!$A$',start), # location within workbook
               plotWidth=600) # plot width
    XLgeneric(wb, 'Summary', summaryData[[i]]$table, row1=start, col1=12, title=names(summaryData)[i]) # will drop in column I
    start <- start + 25
  }
  cat('\n')
  # clean up plot directory so we don't get a bunch of crap
  unlink('plots', recursive=TRUE)
  
  # drop in filteredData
  if (!is.null(filteredData)) {
    cat('Filtered Dataset\n')
    XLgeneric(wb, 'Filtered Data', filteredData)
  }
  # drop in originalData
  if (!is.null(originalData)) {
    cat('Original Dataset\n')
    XLgeneric(wb, 'Original Data', originalData)
  }
  
  saveWorkbook(wb)
}

importDefinitions <- function(db, filename) {
  # import excel file that contains renaming conventions for
  # diagnoses, tests, and prescriptions
  # this will save to a sqlite db object and overwrite
  # tables with the same names.  sheets must be named
  # 'tests, 'prescriptions', 'diagnosis'
  wb <- loadWorkbook(filename)
  for (s in getSheets(wb)) {
    cat('\r',paste("Importing Sheet",s))
    tmp <- readWorksheet(wb, s)
    dbWriteTable(db, tolower(s), tmp, overwrite=TRUE)
    Sys.sleep(.1)
    flush.console()
  }
  flush.console()
  cat('All sheets successfully imported')
  # return nothing since data are in db object now
}

changeEncoding <- function(df) {
  # Convert encoding of imported csv file to UTF-8 no matter what the locale is
  # this is very important for the shiny app. 
  # This is just a wrapper function around apply and enc2utf8
  # ex.
  # > changeEncoding(df)
  
  return(as.data.frame(apply(df, 2, enc2utf8)))
}

getDefs <- function(db) {
  defs<- list('test' = dbGetQuery(db, "select * from test"),
              'diagnosis' = dbGetQuery(db, "select * from diagnosis"),
              'prescription' = dbGetQuery(db, 'select * from prescription')
  )
  
  return(defs)
  
}