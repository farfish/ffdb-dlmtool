library(callr)

source('ffdbclient.R')
source('spicttools.R')

# Models and functions to generate input from FFDB doc and output from input
model_fns <- list(
    spict_fit = list(
        model_input = function (doc) ffdbdoc_to_spictstock(doc),
        model_output = function (st) {
            library(spict)
            fit.spict(st)
        }),
    dlm_mp = list(
        model_input = function (doc) ffdbdoc_to_dlmtool(doc),
        model_output = function (d) {
            library(DLMtool)
            runMP(d, reps=1000, silent = TRUE)
        }))

log <- function (...) {
    writeLines(paste(...), con = stderr())
}

# Convert arbitary object to blob::blob, for insertion into a BYTEA field
encode_bytea <- function (object) {
    tf <- tempfile(fileext = ".rds")
    on.exit(unlink(tf))

    saveRDS(object, file = tf)
    blob::blob(readBin(tf, "raw", n = file.info(tf)$size))
}

# Convert blob::blob containing encode_bytea data back into it's object
decode_bytea <- function (raw) {
    tf <- tempfile(fileext = ".rds")
    on.exit(unlink(tf))

    # blob::blob is a list of raw, rs[1,1] will return a blob of one item
    # Extract that one item to a raw
    raw <- blob::as_blob(raw)[[1]]
    if (is.null(raw)) return(NULL)
    writeBin(raw, tf)
    readRDS(tf)
}

# Assume query only returns one row, fetch and return it as a list
fetch_one <- function (conn, sql, params = NULL) {
    rs <- dbSendQuery(conn, sql, params = params)
    on.exit(dbClearResult(rs))
    rows <- dbFetch(rs)
    return(if (nrow(rows) > 0) as.list(rows[1,]) else NA)
}

# Fetch & decode model output object
get_model_output <- function (conn, template_name, document_name, model_name) {
    row <- fetch_one(conn, "
         SELECT output_rdata
           FROM model_output
          WHERE model_name = $3
            AND input_hash = (
                SELECT (input_hashes #>> ARRAY[$3]) -- NB: Get JSON value as text, not JSON object
                  FROM document
                 WHERE template_name = $1
                   AND document_name = $2
              ORDER BY version DESC
                 LIMIT 1
                )
    ", params = list(template_name, document_name, model_name))
    return(if (identical(row, NA)) NA else decode_bytea(row[1]))
}

# Generate Digest of DLMTool objects by CSVing first
sha1.Data <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
    tf <- tempfile(fileext = '.csv')
    on.exit(unlink(tf))
    Data2csv(x, file = tf)
    x <- read.csv(tf, header = FALSE, stringsAsFactors = FALSE)

    digest::sha1(x, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}

listen_fns <- list(
    document = function (conn, payload) {
        # Select from document for update
        row <- fetch_one(conn, "
            SELECT template_name, document_name, version, input_hashes
              FROM document
             WHERE template_name = $1 AND document_name = $2 AND version = $3
               AND input_hashes IS NULL
               FOR UPDATE SKIP LOCKED
        ", params = as.list(strsplit(payload, '/')[[1]]))
        if (identical(row, NA)) return()

        # Generate key-value of each model's hash
        doc <- ffdb_fetch(row$template_name, row$document_name, row$version, instance = conn)
        model_inputs <- lapply(model_fns, function (fns) tryCatch(fns[['model_input']](doc), error = function (e) e))
        row$input_hashes <- lapply(model_inputs, digest::sha1)

        # Insert missing hashes into model output
        for (m in names(model_inputs)) {
            dbExecute(conn, "
                INSERT INTO model_output (model_name, input_hash, input_rdata, output_rdata)
                     VALUES ($1, $2, $3, NULL)
                ON CONFLICT DO NOTHING
            ", params = list(
                m,
                row$input_hashes[[m]],
                encode_bytea(model_inputs[[m]])))
        }

        dbExecute(conn, "
            UPDATE document
               SET input_hashes = $4
             WHERE template_name = $1 AND document_name = $2 AND version = $3
        ", params = list(
            row$template_name,
            row$document_name,
            row$version,
            jsonlite::toJSON(row$input_hashes, auto_unbox = TRUE)))
    },
    model_output = function (conn, payload) {
        row <- fetch_one(conn, "
            SELECT model_name, input_hash, input_rdata, output_rdata
              FROM model_output
             WHERE model_name = $1 AND input_hash = $2
               AND output_rdata IS NULL
               FOR UPDATE SKIP LOCKED
        ", params = as.list(strsplit(payload, '/')[[1]]))
        if (identical(row, NA)) return()

        row$output_rdata <- tryCatch(callr::r(
            model_fns[[row$model_name]][['model_output']],
            args = list(decode_bytea(row$input_rdata)),
            timeout = 600), error = function (e) e)

        x <- dbExecute(conn, "
            UPDATE model_output
               SET output_rdata = $3
             WHERE model_name = $1 AND input_hash = $2
        ", params = list(row$model_name, row$input_hash, encode_bytea(row$output_rdata)))
    })

worker <- function () {
    conn <- dbConnect(RPostgres::Postgres(), dbname = 'ffdb_db')
    on.exit(dbDisconnect(conn))

    for (n in names(listen_fns)) {
        dbExecute(conn, paste("LISTEN", n))
    }

    while(TRUE) {
        n <- RPostgres::postgresWaitForNotify(conn, 60)
        if (is.null(n)) next

        log(n$channel, n$payload)
        tryCatch({
            dbWithTransaction(conn, listen_fns[[n$channel]](conn, n$payload))
        }, error = function (e) {
            # Something went wrong. Report error and carry on
            log("failed:", e$message)
        })
        log("==================")
    }
}
if (!interactive()) worker()
