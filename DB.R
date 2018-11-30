library(RSQLite)

#Directory where the database is located

#setwd("college-scorecard")

# You can read in the SQLite datbase like this
db <- dbConnect(dbDriver("SQLite"), "college-scorecard/database.sqlite")