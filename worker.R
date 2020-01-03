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

# Assume query only returns one row, fetch and return it as a list
fetch_one <- function (conn, sql, params = NULL) {
    rs <- dbSendQuery(conn, sql, params = params)
    on.exit(dbClearResult(rs))
    rows <- dbFetch(rs)
    return(if (nrow(rows) > 0) as.list(rows[1,]) else NA)
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
    document = list(stale = function (conn, payload) {
        dbExecute(conn, "
            SELECT pg_notify('document', template_name ||'/'|| document_name ||'/'|| version)
              FROM document
             WHERE input_hashes IS NULL
        ")
    }, event = function (model_dir, conn, payload) {
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
        model_inputs <- lapply(model_fns, function (fns) tryCatch(fns[['model_input']](doc), error = function (e) { log(e) ; e }))
        row$input_hashes <- lapply(model_inputs, digest::sha1)

        # Insert missing hashes into model output
        for (m in names(model_inputs)) {
            dbExecute(conn, "
                INSERT INTO model_output (model_name, input_hash, input_rdata, output_path)
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
    }),
    model_output = list(stale = function (conn, payload) {
        dbExecute(conn, "
            SELECT pg_notify('model_output', model_name ||'/'|| input_hash)
              FROM model_output
             WHERE output_path IS NULL
        ")
    }, event = function (model_dir, conn, payload) {
        row <- fetch_one(conn, "
            SELECT model_name, input_hash, input_rdata
              FROM model_output
             WHERE model_name = $1 AND input_hash = $2
               AND output_path IS NULL
               FOR UPDATE SKIP LOCKED
        ", params = as.list(strsplit(payload, '/')[[1]]))
        if (identical(row, NA)) return()

        output_path <- normalizePath(file.path(model_dir, paste(
            'output',
            row$model_name,
            row$input_hash,
            'rds',
            sep = ".")), mustWork = FALSE)
        if (!startsWith(output_path, model_dir)) stop("Attempt to escape model_dir")

        output_obj <- tryCatch(callr::r(
            model_fns[[row$model_name]][['model_output']],
            args = list(decode_bytea(row$input_rdata)),
            timeout = 240), error = function (e) e)
        saveRDS(output_obj, output_path, compress = FALSE)  # NB: Sacrifice disk space for responsiveness (~1.4s to decompress vs ~4.9s)
        Sys.chmod(output_path, mode = "0644", use_umask = FALSE)  # Make sure anyone can read output
        x <- dbExecute(conn, "
            UPDATE model_output
               SET output_path = $3
             WHERE model_name = $1 AND input_hash = $2
        ", params = list(row$model_name, row$input_hash, output_path))
    }))

worker <- function (model_dir) {
    conn <- dbConnect(RPostgres::Postgres(), dbname = 'ffdb_db')
    on.exit(dbDisconnect(conn))

    for (n in names(listen_fns)) {
        dbExecute(conn, paste("LISTEN", n))
    }

    while(TRUE) {
        n <- RPostgres::postgresWaitForNotify(conn, 60)
        if (is.null(n)) {
            for (listen_name in names(listen_fns)) {
                listen_fns[[listen_name]][['stale']](conn)
            }
        } else {
            log(n$channel, n$payload)
            dbWithTransaction(conn, listen_fns[[n$channel]][['event']](model_dir, conn, n$payload))
            log("==================")
        }
    }
}
if (!interactive()) worker()
