library(shiny)

ui_elements <- 'ui_elements/'
ui_elements <- paste0(ui_elements, list.files(ui_elements))
sapply(ui_elements, function(ui_e) source(ui_e))

shinyUI(
 
    fluidPage(
      titlePanel(tags$b(h1("Gates Costing Analyzer v 0.23"))),
      fluidRow(
      column(12,      
        tabsetPanel(type = 'tabs',
            tab_dashboard,
            tab_serviceCosting,
            tab_summary,
            tab_filters,
            tab_data

            # tab_manageData
        ) # END OF THE TABS
      ) # END OF DATA PRESENTATION SIDE
      ) # END OF ROW
    ) # END OF PAGE
) # END OF SHINY
