
tab_manageData <- tabPanel('Raw data management',
         helpText('Import new raw data or definition tables or choose tables to export. *Still in development*'),
         h4('Import - All files must be CSV format'),
         hr(),
         column(6,
                fileInput('raw','Select raw data file',
                          accept = c('text/csv','.csv')
                ),
                downloadButton('rawDown', 'Raw data'),
                h4('Definition Files'),
                hr(),
                fileInput('columns','Select columns definition data file',
                          accept = c('text/csv','.csv')
                ),
                downloadButton('columnsDown', 'Columns'),
                fileInput('prescription','Select prescriptions definition data file',
                          accept = c('text/csv','.csv')
                ),
                downloadButton('rxDown', 'Prescriptions'),
                fileInput('diagnosis','Select diagnosis definition data file',
                          accept = c('text/csv','.csv')
                ),
                downloadButton('diagnosisDown', 'Diagnoses'),
                fileInput('test','Select test definition data file',
                          accept = c('text/csv','.csv')
                ),
                downloadButton('testDown', 'Tests')
         ),
         column(6,
                verbatimTextOutput('rawPrint')
         )
) # END RAW DATA MANAGEMENT