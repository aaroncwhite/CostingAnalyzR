## THIS IS THE SERVICE COSTING PANE --------------------------------------
tab_serviceCosting <- tabPanel('Service Costing',
         column(3,
                checkboxGroupInput('sites', 'Facilities', choices="", selected=""),
                checkboxGroupInput('slines', 'Service Lines:', choices="", selected=""),
                numericInput('sds', 'Standard Deviations:', 2, min=1),
                h5('Update Weights'),
                uiOutput('upload_costs'),
                HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
                
                downloadButton('download_costs', 'Download costing data'),
                downloadButton('download_weights', 'Download current weights file.'),
            helpText('If a new weights file is not provided, the current file will be used.'),
            helpText('Weights are applied to the dataset with the "Global Filters" applied.')
         ),
         column(9,
                plotOutput('cost_boxPlot'),
                fluidRow(column(3, checkboxInput('show.scatter', 'Show points', T)), uiOutput('scatter_controls')),
                verbatimTextOutput('cost_model'),
                column(4, h4('Summary Output'), selectInput('sum_func', 'Show:', choices=c("Mean" = 'mean', 
                                                                                           'Median' = 'median', 
                                                                                           'Max' = 'max',
                                                                                           'Min' = 'min'))),
                dataTableOutput('cost_table')

         )
)
## END OF SERVICE COSTING PANE --------------------------------------