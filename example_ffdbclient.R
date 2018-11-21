library(DBI)

source('./ffdbclient.R')

# You can connect either by using...
# ... a direct DB connection: con <- dbConnect(RPostgres::Postgres(), dbname = 'ffdb_db')
# ... to a named remote server: con <- 'ffdb.farfish.eu'
con <- 'ffdb.farfish.eu'

cat("\n\n# List all DLMtool documents in the database\n")
str(ffdb_list('dlmtool', con))

cat("\n\n# Get a list of data.frames for any database object\n")
doc <- ffdb_fetch('dlmtool', 'demo-cobia', instance = con, convert = TRUE)
str(names(doc))
str(doc$metadata)

cat("\n\n# Fetch and return a DLMtool Data object, which we can then use\n")
d <- ffdb_to_dlmtool('demo-cobia', instance = con)
d@CAA[1,,]
