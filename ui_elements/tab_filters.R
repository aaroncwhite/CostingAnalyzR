## THIS IS THE GLOBAL FILTERS -----------------------------------------------
tab_filters <- tabPanel('Filters',
         helpText("Set filters that will apply to entire dataset for resulting analysis.  All criteria must be met in order to be included in analysis."),
         column(4,
                h2('Global dataset filters'),
                h5(textOutput('availableRecords')),
                hr(),
                helpText('These filters will apply to data reported on the Summary and Dashboard tabs.'),
                tags$b(checkboxInput('strip', 'Strip accented characters', value=TRUE)),
                dateRangeInput('dateRange', label= "Allowed date range:", 
                               min='2015-03-01', start='2015-03-01', startview='month'),
                
                helpText('Default is enabled because of different encoding issues with the raw data.'),
                
                sliderInput('ageRange', 'Allowed age range:',
                            min = 0, max = 110, value = c(0, 100)
                ),
                sliderInput('waitTime', 'Max wait time allowed:',
                            min = 0, max = 500, value = c(0,480)),
                checkboxInput('absWait', 'Take absolute value of wait times',
                              value= TRUE),
                helpText('Default is 8 hour max wait time'),
                sliderInput('consTime', 'Max consultation time allowed:',
                            min = 0, max = 180, value = c(0,120)),
                helpText('Default is 2 hour max consult time'),
                checkboxInput('absCons', 'Take absolute value of consultation times',
                              value= TRUE)
         ),
         column(7,
                h2('Diagnosis dashboard filters'),
                h5(textOutput('diagsSelected')),
                hr(),
                helpText('These filters will only apply to data on the dashboard page. These are applied after the global filters.'),
                
                column(4,
                       
                       ## THIS IS THE START OF THE CONTROL WIDGETS -----------------------------
                       h4('Filter sites:'),
                       checkboxGroupInput("location", label = "Site:",
                                          choices = c("Belladere", "Boucan Carre", "HSN", "Lacolline", "Sante Fanm", "Unknown"),# imported and proccessed from import()
                                          selected= c("Belladere", "Boucan Carre", "HSN", "Lacolline", "Sante Fanm")
                       )
                       
                       # selectInput('diagnosis', 'Include diagnoses:', choices=NA, multiple=TRUE, selectize=TRUE)
                       
                       ## THIS IS THE END OF THE CONTROL WIDGETS ------------------------------                
                ),
                column(8,
                       h4('Filter diagnoses:'),
                       checkboxInput('allDiags','Include all diagnoses', value=TRUE),
                       uiOutput('diags')
                )
         )
)
# ## END OF GLOBAL FILTERS -----------------------------------------------