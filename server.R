
shinyServer(function(input, output, session) {
    
    
## Data Loading and Filtering ==============   

    rawNumbers <- reactive({ 
#       # this will pull from the database if no new file is present, 
#       # or upload a new file to the db and then present that new data
#       # as the raw file
#       if (!is.null(input$prescription)) {
#         tmp <- changeEncoding(read.csv(input$prescription$datapath, as.is = TRUE))
#         dbWriteTable(db, 'prescription', tmp, overwrite=TRUE)
#         print('Prescription table written')
#       }
#       if (!is.null(input$diagnosis)) {
#         tmp <- changeEncoding(read.csv(input$diagnosis$datapath, as.is = TRUE))
#         dbWriteTable(db, 'diagnosis', tmp, overwrite=TRUE)
#         print('Diagnosis table written')
#         
#       }
#     if (!is.null(input$test)) {
#         tmp <- changeEncoding(read.csv(input$test$datapath, as.is = TRUE))
#         dbWriteTable(db, 'test', tmp, overwrite=TRUE)
#         print('Labs/test table written')
#         
#       }
#       if (!is.null(input$columns)) {
#         tmp <- changeEncoding(read.csv(input$columns$datapath, as.is = TRUE))
#         dbWriteTable(db, 'columns', tmp, overwrite=TRUE)
#         print('Columns table written')
#         
#       }
#         
#       defs <- list('test' = dbGetQuery(db, 'select * from test'),
#            'prescription' = dbGetQuery(db, 'select * from prescription'),
#            'diagnosis' = dbGetQuery(db, 'select * from diagnosis')
#       )
#       cols <- dbGetQuery(db, 'select * from columns')
#       
#       if (!is.null(input$raw)) { # if a new file is present, upload it to db
#         # raw <- changeEncoding(read.csv(input$raw$datapath, as.is = TRUE))
#         raw <- raw[,-grep('nom', names(raw), ignore.case=TRUE)]
#         dbWriteTable(db, 'raw', raw, overwrite=TRUE)
#         print('Raw table written')
#       } 
# 
#       if (!is.null(input$raw) | !is.null(input$prescription) | 
#           !is.null(input$test) | !is.null(input$diagnosis) | !is.null(input$columns)) {
#         cleaned <- cleanData(dbGetQuery(db, 'select * from raw'), defs, cols)
#         dbWriteTable(db, 'cleaned', cleaned, overwrite=TRUE)
#       }

      list('rawN' = dbGetQuery(db, 'select count(Facilities_name) from raw')[1,1],
        'cleanN' = dbGetQuery(db, 'select count(facility) from cleaned')[1,1],
        'cleaned' = dbGetQuery(db, 'select * from cleaned')
        )
    })
    
#     rawMgmt <- reactive({
#       capture.output(rawNumbers())
#       })
    
    # output$rawPrint <- renderPrint({rawMgmt()})
    # Raw data download buttons
    output$rawDown <- downloadHandler(
      filename='rawData.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from raw'), file)
      }
    )
    output$downloadCleaned <- downloadHandler(
      filename = 'cleaned.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from cleaned'), file)
      }
      # This isn't working at the moment. :(
#       content= function(file) {
#         fname <- paste0(file,'.xlsx') # we'll set the workbook here
#         wb <- loadWorkbook(fname, create=TRUE)
#         local({
#           filters <- rbind(cbind('Facilities', paste(input$location, collapse=', ')), # location filters
#                            cbind('Date Range', paste(input$dateRange, collapse=' to ')),
#                            cbind('Age Range', paste(input$ageRange, collapse=" to ")),
#                            cbind('Allowed wait time range', paste(input$waitTime, collapse=" to ")),
#                            cbind('Absolute wait times', input$absWait),
#                            cbind('Allowed consult time range', paste(input$consTime, collapse=' to ')),
#                            cbind('Absolute consult times', input$absCons)
#           )
#           colnames(filters) <- c('Filter', 'Values')
#           createSheet(wb, 'filters')
#           XLgeneric(wb, 'filters', filters, title = 'Filter Criteria')
#           global()$date <- as.Date(global()$date, origin='1970-01-01')
#           global()$week <- as.Date(global()$week, origin='1970-01-01')
#           
#           XLgeneric(wb, 'data', global())
#           
#           saveWorkbook(wb)          
#         })
#         file.rename(fname, file)
#       }
    )
    output$columnsDown <- downloadHandler(
      filename='columns.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from columns'), file)
      }
    )
    output$testDown <- downloadHandler(
      filename='tests.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from tests'), file)
      }
    )
    output$rxDown <- downloadHandler(
      filename='prescriptions.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from prescription'), file)
      }
    )
    output$diagnosisDown <- downloadHandler(
      filename='diagnosis.csv',
      content= function(file) {
        write.csv(dbGetQuery(db, 'select * from diagnosis'), file)
      }
    )
    
    global <- reactive({
      temp <- rawNumbers()$cleaned
      if (input$absWait == TRUE) {
        temp$wait_time <- abs(temp$wait_time)
      }
      if (input$absCons == TRUE) {
        temp$consultation_time <- abs(temp$consultation_time)
      }
      if (input$strip == TRUE) {
        tests <- c('lab1', 'lab2', 'lab3', 'lab4', 'lab5')
        rx <- c('rx1', 'rx2', 'rx3', 'rx4', 'rx5')
        
        temp[,c(tests,rx, 'diagnosis')] <- apply(temp[,c(tests,rx, 'diagnosis')], 2, function (a) iconv(a, to='ASCII//TRANSLIT'))
      }
      

      # temp$date <- as.POSIXlt.character(temp$date)
      temp[
           temp$date >= input$dateRange[1] & 
           temp$date <= input$dateRange[2] &
           temp$age >= input$ageRange[1] & temp$age <= input$ageRange[2] &
           temp$wait_time >= input$waitTime[1] & 
           temp$wait_time <= input$waitTime[2] &
           temp$consultation_time >= input$consTime[1] &
           temp$consultation_time <= input$consTime[2],
         ]
      
      })

    output$viewTable <- DT::renderDataTable(dbGetQuery(db, 'select * from cleaned'), options=list(pageLength=10), selection='none', server=TRUE) 
    
  

## OBSERVER FUNCTION WILL MONITOR ALL CHANGES TO DATA AND UPDATE INPUT CONTROLS    
    observe({
      locs <- dbGetQuery(db, 'select distinct facility from cleaned')[,1]
      locs <- locs[order(locs)]
      updateCheckboxGroupInput(session, 'location', 
                                 choices = locs,
                                 selected = locs
      )
      updateSelectInput(session,'summary', choices= c('All Sites' = 'all', locs),
                          selected= 'all'
      )

    })
      
    output$availableRecords <- renderText({
      paste0('Filtered dataset contains ',nrow(global()),' of ', rawNumbers()$rawN, ' total records. ')})
    
## Summary Tabset data -----
    top <- reactive({
      topDiagsSite(global(), input$num_diags)
    })
    

    output$summaryPlot <- renderPlot({eval(parse(text=paste0("top()$`",input$summary,'`$plot')))})
    output$summaryTable <- renderTable({eval(parse(text=paste0("top()$`",input$summary,'`$table')))})
    output$downloadSummary <- downloadHandler(
      
      filename = function () {'summary.xlsx'},
      content= function(file) {
        fname <- paste0(file, '.xlsx')
        summaryData <- top()
        wb <- loadWorkbook(fname, create=TRUE)
        local({ # this needs to be here so the loop happens
          start <- 1
          jump <- if(input$num_diags < 25) {25} else {input$num_diags + 5}
          cat('Exporting Summary Data\n')
          createSheet(wb, 'Summary')
          
          plotFolder <- 'plots/'
          if (!file.exists("plots") & plotFolder=='plots/') {
            dir.create(file.path("./plots"))
          }
          
          for (i in 1:length(summaryData)) {
            cat(names(summaryData)[i],'\n')
            # initalize png device
            filename <- gsub(" ", "", names(summaryData)[i])
            pimage <- file.path(paste0(plotFolder,filename,".png"))
            png(filename = pimage, width= 600, height=440)
            # print the plot to the device
            print(summaryData[[i]]$plot)
            # close the device
            dev.off()
            
            cat('Exporting image to workbook\n')
            createName(wb, filename, formula= paste0('Summary!$A$',start), overwrite=TRUE)
            addImage(wb, filename = pimage, name = filename, originalSize = TRUE)
            
            XLgeneric(wb, 'Summary', summaryData[[i]]$table, row1=start, col1=12, title=names(summaryData)[i]) # will drop in column I
            start <- start + jump
          }
          cat('\n')
          # clean up plot directory so we don't get a bunch of crap
          unlink('plots', recursive=TRUE)

        })
        file.rename(fname, file)
      },
      contentType= 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      
    )


    

#################################################  DYNAMIC DASBOARD  #############################
    diagOptions <- reactive({
      # this will react based on the locations selected and find the top occuring diagnoses
      dg <- as.data.frame(table(global()$diagnosis))

      dg <- as.data.frame(dg[order(dg$Freq, decreasing=TRUE),])
      names(dg) <- 'Diagnosis'
      dg
    })
    
    sel <- reactive({
      if (input$allDiags != TRUE) {
        return(input$diagsTable_rows_selected)
      }
      else {
        return(1:nrow(diagOptions()))
      }
    })
    
    output$diags <- renderUI({
      if (input$allDiags == FALSE) {
        output$diagsTable <- DT::renderDataTable(diagOptions(), server=FALSE, 
                            options= list(selection=list(selected=list(rows=1:700)),
                                          searchHighlight= TRUE,
                                          pageLength=25
                            )
        )
        tagList(
          helpText('Diagnoses listed in order of decreasing frequency for the entire dataset.'),
          DT::dataTableOutput('diagsTable')
        )

      }
    })

    output$diagsSelected <- renderText({
      i <- as.character(diagOptions()[sel(),1])
      n <- sum(table(d()$diagnosis[d()$diagnosis %in% i]))
      paste('Diagnosis analysis includes', n, 'records.')})



    
## DYNAMIC OBSERVERs FOR DASHBOARD 
    d <- reactive({
      filtered <- diagOptions()[sel(), 'Diagnosis']
      global()[global()$facility %in% input$location &
                global()$diagnosis %in% filtered,]
    })
    output$dashboardTitle <- renderText({
      if (input$allDiags != TRUE) {
        return(paste0("Dashboard report: ",paste(diagOptions()[input$diagsTable_rows_selected,1], collapse=', ')))
      }
      else {
        return('Dashboard report: All Diagnoses')
      }
    })
    output$fullDiagsData <- DT::renderDataTable(d(), server=TRUE)
    
## Patient visits by site ======
    visits <- reactive({
      patientVisits(d(), by= input$visitsAggregation,  smooths= input$visitsSmooth, 
                    title=NULL, detail=input$visitsDetail)
    })
    
    output$visitsPlot <- renderPlot({
      visits()$plot
    })

    output$visitsTable <- renderTable({
      if (input$visitsTable == TRUE) {
        visits()$table
      }
    })
    
## Sex dist
    sexDistribution <- reactive({
      sexDist(d(),title=NULL, percents=input$sexPercent)
    })
    output$sexDist <- renderPlot({sexDistribution()$plot})
    output$sexDistTable <- renderTable({
      if (input$sexShowTable == TRUE) {
        sexDistribution()$table
      }
    })
    
## Age Distribution ============    
    ageDistribution <- reactive({
      calcDensity(d(), 'age', 
                  linePlot=input$ageStatistic, 
                  xlab='Patient Age')
    })
    output$ageDensityTable <- renderTable({
        if (input$ageShowTable == TRUE) {
          ageDistribution()$table
        }
      })
    output$ageDensity <- renderPlot({ageDistribution()$plot})

## Wait Distribution ===========
    waitDistribution <- reactive({
      bounds <- as.numeric(input$waitBounds)
      calcDensity(d(), 'wait_time', 
                  linePlot=input$waitStatistic, 
                  xlab='Wait time')
    })
    output$waitDensityTable <- renderTable({
      if (input$waitShowTable == TRUE) {
        waitDistribution()$table

      }
    })
    output$waitDensity <- renderPlot({waitDistribution()$plot})
    
    ## Consult time Distribution ===========
    consultDistribution <- reactive({
      bounds <- as.numeric(input$consultBounds)
      
      calcDensity(d(), 'consultation_time', 
                  linePlot=input$consultStatistic, 
                  xlab='Consultation time')
    })
    output$consultDensityTable <- renderTable({
      if (input$consultShowTable == TRUE) {
        consultDistribution()$table
        }
    })
    output$consultDensity <- renderPlot({consultDistribution()$plot})
    
    ## Average Lab and Rx per patient
    output$avg_rx_lab <- renderTable({
      avg_Rx_Test(d())
    })
    
    ## Top labs  =========================
    labs <- reactive({
      topTest(d(), top=input$labInclude, percents=input$labPerc)
    })
    output$labBars <- renderPlot({labs()$plot})
    output$labTable <- renderTable({
        if (input$labShowTable == TRUE) {
          labs()$table
        } 
      })
    
    ## Top Rx  =========================

    rx <- reactive({
      topRx(d(), top=input$rxInclude, percents=input$rxPerc)
    })
    output$rxBars <- renderPlot({rx()$plot})
    output$rxTable <- renderTable({
      if (input$rxShowTable == TRUE) {
        rx()$table
      }
    })
    
    ## Export Dynamic to excel =========
    # this will be fun
    output$downloadDynamic <- downloadHandler(
      filename = 'dynamicSummary.xlsx',
      content= function(file) {
        fname <- paste0(file,'.xlsx') # we'll set the workbook here
        wb <- loadWorkbook(fname, create=TRUE)
        local({
          if (input$allDiags != TRUE) {
            if (input$allDiags != TRUE) {
              title <- paste0("Dashboard report: ",paste(diagOptions()[input$diagsTable_rows_selected,1], collapse=', '))
            }
            else {
              title <- ('Dashboard report: All Diagnoses')
            }
          }
          
          filters <- rbind(cbind('Facilities', paste(input$location, collapse=', ')), # location filters
                           cbind('Date Range', paste(input$dateRange, collapse=' to ')),
                           cbind('Age Range', paste(input$ageRange, collapse=" to ")),
                           cbind('Allowed wait time range', paste(input$waitTime, collapse=" to ")),
                           cbind('Absolute wait times', input$absWait),
                           cbind('Absolute consult times', input$absCons)
          )
          colnames(filters) <- c('Filter', 'Values')
          createSheet(wb, 'Dynamic')
          XLgeneric(wb, 'Dynamic', title)
          XLgeneric(wb, 'Dynamic', filters, title = 'Filter Criteria', row1=3)
          
          plotFolder <- 'plots/'
          if (!file.exists("plots") & plotFolder=='plots/') {
            dir.create(file.path("./plots"))
          }
          start <- 15
          jump <- 25
          
          labName <- paste('Top',input$labInclude,"Labs")
          rxName <- paste('Top', input$rxInclude,'Prescriptions')
          d <- list('Sex Distribution' = sexDistribution(), 'Age Distribution' = ageDistribution(),
                    'Wait Time Distribution' = waitDistribution(),'Consultation Time Distribution' = consultDistribution(), 
                     'Top Labs' = labs(), 'Top Prescriptions'  = rx()
          )
          for (i in 1:length(d)) {
            cat(names(d)[i],'\n')
            # initalize png device
            filename <- gsub(" ", "", names(d)[i])
            pimage <- file.path(paste0(plotFolder,filename,".png"))
            png(filename = pimage, width= 600, height=440)
            # print the plot to the device
            print(d[[i]]$plot + ggtitle(names(d)[i]))
            # close the device
            dev.off()
            
            cat('Exporting image to workbook\n')
            createName(wb, filename, formula= paste0('Dynamic!$A$',start), overwrite=TRUE)
            addImage(wb, filename = pimage, name = filename, originalSize = TRUE)
            
            XLgeneric(wb, 'Dynamic', d[[i]]$table, row1=start, col1=6, title=names(d)[i], addRownames=TRUE) # will drop in column I
            start <- start + jump
          }
          
          
          unlink('plots', recursive=TRUE)
          saveWorkbook(wb)          
        })
        file.rename(fname, file)
      }
    )

    # SERVICE COSTING ########################################################################
    output$upload_costs <- renderUI({
      input$clearFile1
      fileInput('costs', 'Select cost weights file:', accept='.xlsx', width="80%")
      
    })
    costing_data <- reactive({
      head(global())
      if (!is.null(input$costs)) {
          costs <- openxlsx::readWorkbook(input$costs$datapath)
          shinyFileSave(input, 'costs', session=session, roots=c(wd='.'))
          file.copy(input$costs$name, 'temp.xlsx', overwrite=T)
          file.copy('temp.xlsx', 'Cross-site cost comparison.xlsx', overwrite = T)
          
#           write(input$costs$datapath, input$costs$name)
# #           file.copy('temp.xlsx', 'Cross-site cost comparison.xlsx', overwrite = T)
# #           file.remove('temp.xlsx')
        

        
      }
      else {
        costs <- openxlsx::readWorkbook('Cross-site cost comparison.xlsx')

      }
        
        # fill in site name
        for (i in 1:nrow(costs)) {
          if (is.na(costs$Site[i])) {
            costs$Site[i] <- costs$Site[i-1]
          }
        }
        
        costs[,3:12] <- apply(costs[,3:12], 2, function(x) ifelse(is.na(x), 0, x))
        cleaned <- global()
#         cleaned$facility <- toupper(cleaned$facility)
        
        
        # find patients per site, per service line, label and then apply costs
        
        results <- data.frame()
        
        for (f in unique(costs$Site)) {
          site_sub <- cleaned[cleaned$facility == f,]
          for (sl in unique(costs$Service.Line[costs$Site == f])) {
            service_costs <- costs[costs$Site == f & costs$Service.Line == sl,, drop=F]
            
            if (!is.na(service_costs$Restrictions)) {
              # parse restrictions if there are any
              restrict <- unlist(strsplit(service_costs$Restrictions, "; "))
              
              # build the criteria
              restrict <- paste0('site_sub$', restrict, collapse=" & ")
              print(restrict)
              
              site_sub <- site_sub[eval(parse(text=restrict)),] # filters based on restrictions
            }
            
            # find the diagnoses that match
            visits <- site_sub[site_sub$diagnosis %in% unlist(strsplit(service_costs$Identifiers,", ")),]
            
            if (nrow(visits) > 0 ) {
              
              visit_costs <- visits[,c('facility', 'pt_code', 'age', 'sex', 'date','diagnosis')]
              visit_costs$Service.Line <- sl
              
              for (i in c('Registration/Check.in/Payment', 'Education', 'Pre-consult.(+.counseling)', 'Tax')) {
                visit_costs[,i] <- service_costs[,i]
              }
              
              visit_costs$Consultation.Time <- visits$consultation_time
              visit_costs$Consultation.Cost.Minute <- service_costs$Consultation
              visit_costs$Consultation.Cost <- visits$consultation_time * service_costs$Consultation
              
              visit_costs$Lab.Fixed.Cost <- ifelse(visits$num_tests > 0, service_costs$`Lab.(fixed.costs)`, 0)
              visit_costs$Lab.Unit.Cost <- ifelse(visits$num_tests > 0, service_costs$`Lab.(cost/lab)`, 0)
              visit_costs$Lab.Units <- visits$num_tests
              visit_costs$Lab.Variable.Cost <- ifelse(visits$num_tests > 0, service_costs$`Lab.(cost/lab)` * visits$num_tests, 0)
              
              visit_costs$Pharmacy.Fixed.Cost <- ifelse(visits$num_rx > 0, service_costs$Pharmacy.cost, 0)
              visit_costs$Pharmacy.Unit.Cost <- ifelse(visits$num_rx > 0, service_costs$`Med.cost.(cost/Rx)`,0)
              visit_costs$Pharmacy.Units <- visits$num_rx
              visit_costs$Pharmacy.Variable.Cost <- ifelse(visits$num_rx > 0, service_costs$`Med.cost.(cost/Rx)` * visits$num_rx, 0)
              
              
              
              results <- rbind.fill(results, visit_costs)
            }
          }
        }
        
        results$Total.Visit.Cost <- rowSums(results[,c('Registration/Check.in/Payment', 'Education', 'Pre-consult.(+.counseling)', 'Tax', 
                                                       'Consultation.Cost', 'Lab.Fixed.Cost', 'Lab.Variable.Cost', 'Pharmacy.Fixed.Cost', 
                                                       'Pharmacy.Variable.Cost')])

      
        sites <- unique(results$facility)
        sites <- sites[!is.na(sites)]
        
        for (s in sites) {
          results[,s] <- 0
          results[,s][results$facility == s] <- 1
        }
        

      results
      
    })
    
    
    
    observeEvent(costing_data(), {
      sites <- unique(costing_data()$facility)
      lines <- unique(costing_data()$Service.Line)
      sites <- sites[!is.na(sites)]
      lines <- lines[!is.na(lines)]
      print(sites)
      print(lines)
      updateCheckboxGroupInput(session, 'sites', choices=sites, selected= sites)
      updateCheckboxGroupInput(session, 'slines', choices= lines, selected= 'ANC')
   
        

   
    })
    
    costing_box <- reactive({
      costing_boxPlot(costing_data(), input$sites, input$slines, sds=input$sds,
                      scatter = input$show.scatter, scatter.alpha = input$scatter.alpha, jitter.width = input$jitter.width,
                      jitter.height = input$jitter.height, FUN=input$sum_func)
    })
    
    output$cost_boxPlot <- renderPlot({costing_box()$plot})
    output$cost_table <- renderDataTable({costing_box()$table})
    output$cost_model <- renderPrint({summary(costing_box()$model)})
    
    output$scatter_controls <- renderUI({
      if (input$show.scatter == T) {
        list(
        column(2, numericInput('scatter.alpha', 'Transparency:', min=.1, max=1, step=.1, value=.2)),
        column(2, numericInput('jitter.width', 'Jitter Width', min=0, max=1, step=.1, value=.3)),
        column(2, numericInput('jitter.height', 'Jitter Height', min=0, max=1, step=.1, value=0))
        )
      }
    })
    
    output$download_costs <- downloadHandler(

      filename = paste0('Costing Weights applied ',Sys.Date(),'.csv'),
      content= function(file) {
#         fname <- 'temp.xlsx' # we'll set the workbook here
#         wb <- XLConnect::loadWorkbook(fname)
#         print('load temp file')
#         local({
#           XLConnect::createSheet(wb, 'data')
#           XLConnect::writeWorksheet(wb, costing_data(), 'data')
# #           XLConnect::createSheet(wb, 'filtered raw data')
# #           XLConnect::writeWorksheet(wb, global(), 'filtered raw data')
#           XLConnect::saveWorkbook(wb)
#         })
#         file.rename(fname, file)
        write.csv(costing_data(), file)
      },
      contentType = 'text/csv'
    )
    
    output$download_weights <- downloadHandler(
      filename= 'Cross-site cost comparison.xlsx',
      content= file('Cross-site cost comparison.xlsx')
    )
    
    }

)

