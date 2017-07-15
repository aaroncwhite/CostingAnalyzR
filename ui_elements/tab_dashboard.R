## THIS IS THE DASHBOARD PANE ------------------------------------------
tab_dashboard <- tabPanel("Dashboard",
         tableOutput('test'),
         fluidRow(
           column(10, offset=1,
                  tags$b(h1(textOutput('dashboardTitle'))),
                  helpText('   Dynamically filter data using the controls on the Filters tab.
                           Charts and tables will automatically update as controls are changed. *Still in development*'),
                  downloadButton('downloadDynamic','Download to Excel'),
                  # sex distribution
                  tags$b(h2('Patient Demographics', align='center')),
                  tags$b(h4('Sex distribution')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('sexDist'),
                           tableOutput('sexDistTable')
                    ),
                    column(2, # controls and tables will go here
                           checkboxInput('sexPercent', 'Percentages', value= TRUE),
                           checkboxInput('sexShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ), # end fluidRow
                  # patient age density -------------
                  hr(),
                  tags$b(h4('Patient age distribution')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('ageDensity'),
                           tableOutput('ageDensityTable')
                    ),
                    column(2, # controls and tables will go here
                           selectInput('ageStatistic',
                                       "Line Display:",
                                       choices= c('1st Quartile', 'Median', "Mean", "3rd Quartile"),
                                       selected= "Median"
                           ),
                           
                           checkboxInput('ageShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ), # end fluidRow
                  hr(),
                  tags$b(h2('Visit characteristics', align='center')),
                  tags$b(h4('Patient visits')),
                  # patient visits information ------
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('visitsPlot'),
                           tableOutput('visitsTable')
                    ),
                    column(2, # controls will go here
                           selectInput('visitsAggregation',
                                       'Aggregation:',
                                       choices=c('Weekly' = 'week', "Monthly" = 'month'),
                                       selected='week'
                           ),
                           checkboxInput('visitsSmooth', 'Line Smoothing', value=TRUE),
                           checkboxInput('visitsTable', 'Show table', value=FALSE),
                           selectInput('visitsDetail',
                                       'Table View:',
                                       choices=c('Totals', 'Details')
                           )
                           
                           
                    )
                  ), # end fluidRow
                  # patient wait time density -------------
                  hr(),
                  tags$b(h4('Patient wait time distribution')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('waitDensity'),
                           tableOutput('waitDensityTable')
                    ),
                    column(2, # controls and tables will go here
                           selectInput('waitStatistic',
                                       "Line Display:",
                                       choices= c('1st Quartile', 'Median', "Mean", "3rd Quartile"),
                                       selected= "Median"
                           ),
                           
                           checkboxInput('waitShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ), # end fluidRow
                  hr(),
                  tags$b(h4('Consultation time distribution')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('consultDensity'),
                           tableOutput('consultDensityTable')
                    ),
                    column(2, # controls and tables will go here
                           selectInput('consultStatistic',
                                       "Line Display:",
                                       choices= c('1st Quartile', 'Median', "Mean", "3rd Quartile"),
                                       selected= "Median"
                           ),
                           
                           checkboxInput('consultShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ), # end fluidRow
                  hr(),
                  tags$b(h4('Average Prescriptions and Labs per Patient')),
                  fluidRow(
                    tableOutput('avg_rx_lab')
                  ),
                  hr(),
                  tags$b(h4('Top Labs Ordered')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('labBars'),
                           tableOutput('labTable')
                    ),
                    column(2, # controls and tables will go here
                           sliderInput('labInclude', 'Include top N of labs:',
                                       min=1, max=20, value= 5
                           ),
                           checkboxInput('labPerc',
                                         "Percentages",
                                         value = TRUE
                           ),
                           
                           checkboxInput('labShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ), # end fluidRow
                  hr(),
                  tags$b(h4('Top Prescriptions')),
                  fluidRow(
                    column(10, # plot will go here
                           plotOutput('rxBars'),
                           tableOutput('rxTable')
                    ),
                    column(2, # controls and tables will go here
                           sliderInput('rxInclude', 'Include top N of prescriptions:',
                                       min=1, max=20, value= 5
                           ),
                           checkboxInput('rxPerc',
                                         "Percentages",
                                         value = TRUE
                           ),
                           
                           checkboxInput('rxShowTable', 'Show Table', value= FALSE)
                           
                    )
                  ) # end row
                  )
         )
)
## END OF DASHBOARD PANE ------------------------------------------