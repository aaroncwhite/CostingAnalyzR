# Global Variables

# Load any helper functionality stored in the modules folder
mods <- paste0('modules/', list.files('modules/'))
sapply(mods, function(x) source(x))

# connect the database.  Library is loaded in helpers.R
db <- dbConnect(SQLite(), 'costing.sqlite')
