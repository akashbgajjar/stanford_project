source("DB.R")

InstitutionNames <- dbGetQuery(db, "select distinct INSTNM from Scorecard order by INSTNM asc")

Cities <- dbGetQuery(db, "select distinct CITY from Scorecard order by CITY asc")

Control <- dbGetQuery(db, "select distinct CONTROL from Scorecard where CONTROL != 'NA' order by CONTROL asc")
