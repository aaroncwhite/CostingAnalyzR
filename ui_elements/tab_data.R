# data viewer
tab_data <- tabPanel('Data', 
         tabsetPanel(type='pills',
                     tabPanel('Cleaned dataset',
                              downloadButton('downloadCleaned', 'Download dataset'),
                              dataTableOutput('viewTable')
                     ),
                     
                     tabPanel('Diagnosis filtered subset',
                              DT::dataTableOutput('fullDiagsData')
                     )
         )
)