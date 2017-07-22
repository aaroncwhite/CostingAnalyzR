# Global Variables
library(shiny)
library(shinyFiles)
library(DT)



# Load any helper functionality stored in the modules folder
mods <- list.files('modules/', full.names = T)
sapply(mods, function(x) source(x))

# connect the database.  Library is loaded in helpers.R
db <- dbConnect(SQLite(), 'costing.sqlite')
