library(DBI)

source('./ffdbclient.R')

con <- dbConnect(RPostgres::Postgres(), dbname = 'ffdb_db')
# con <- 'ffdb.farfish.eu' 

str(ffdb_list('dlmtool', con))

json_dfs <- ffdb_fetch('dlmtool', 'demo-cobia', instance = con, convert = FALSE)
json_df <- json_dfs$cal
doc <- ffdb_fetch('dlmtool', 'demo-cobia', instance = con, convert = TRUE)
str(doc$cal)

d <- ffdb_to_dlmtool('demo-cobia', instance = con)
str(d@CAA[1,,])
summary(d, wait=FALSE, plots=c('PD'))

