## THIS IS THE OVERALL SUMMARY PANE --------------------------------------
tab_summary <- tabPanel('Summary',
         column(3, 
                selectInput('summary', 'Display summary for the following:',
                            choices= NULL,
                            selected= NULL),
                sliderInput('num_diags', "Number of top diagnoses:",
                            min=0, max=50, value=10),
                downloadButton('downloadSummary', 'Download Summary')
         ),
         column(9, 
                plotOutput('summaryPlot', height=500),
                tableOutput('summaryTable')
         )
)
## END OF OVERALL SUMMARY PANE --------------------------------------