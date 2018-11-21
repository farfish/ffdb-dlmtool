# Import from the FFDB database
# (c) 2018 Jamie Lentin - jamie.lentin@shuttlethread.com
library(httr)
library(jsonlite)
library(DLMtool)
library(DBI)


# List all templates for a given template name
# example: ffdb_list('dlmtool')
ffdb_list <- function (template_name, instance = 'ffdb.farfish.eu') {
    if (class(instance) == "PqConnection") {
        # instance is a database connection, fetch directly
        res <- dbSendQuery(instance, paste0(
            "SELECT DISTINCT document_name",
            " FROM document",
            " WHERE template_name = '", template_name, "'",
            " ORDER BY document_name",
            ""))
        out <- dbFetch(res)
        dbClearResult(res)
        out <- out[,1]
    } else {
        # Fetch via. HTTP
        req <- GET(paste0('https://', instance, '/api/doc/', template_name),
            add_headers('Accept' = "application/json"))
        if (http_error(req)) {
            stop(sprintf("Request failed: status %s - URL '%s'", status_code(req), uri))
        }
        resp <- fromJSON(content(req, as = "text", encoding = "UTF-8"))
        out <- resp$documents$document_name
    }
    return (out)
}


# Fetch an FFDB document, convert it into a list of data frames
# example: ffdb_fetch('dlmtool', 'demo-cobia')
ffdb_fetch <- function (template_name, document_name, convert = TRUE, instance = 'ffdb.farfish.eu') {
    if (class(instance) == "PqConnection") {
        # instance is a database connection, fetch directly
        res <- dbSendQuery(instance, paste0(
            "SELECT content",
            " FROM document",
            " WHERE template_name = 'dlmtool'",
            " AND document_name = '", document_name, "'",
            " ORDER BY version DESC",
            " LIMIT 1",
            ""))
        out <- dbFetch(res)
        dbClearResult(res)
        out <- fromJSON(out$content)
    } else {
        # Instance is a hostname, fetch via. HTTP
        req <- GET(paste0('https://', instance, '/api/doc/', template_name, '/', document_name),
            add_headers('Accept' = "application/json"))
        if (http_error(req)) {
            stop(sprintf("Request failed: status %s - URL '%s'", status_code(req), uri))
        }
        resp <- fromJSON(content(req, as = "text", encoding = "UTF-8"))
        out <- resp$content
    }

    if (convert) {
        return(lapply(out, json_df_to_ffdbdoc))
    }
    return(out)
}


# Fetch a document and convert directly to a dlmtool Data object
# example: ffdb_to_dlmtool('demo-cobia')
ffdb_to_dlmtool <- function (document_name, instance = 'ffdb.farfish.eu') {
    doc <- ffdb_fetch('dlmtool', document_name, instance = instance)

    f <- tempfile(fileext = ".csv")
    ffdbdoc_to_dlmtool_csv(doc, output = f)
    d <- DLMtool::XL2Data(f)
    unlink(f)
    return(d)
}


# (internal use only) Convert a raw JSON FFDB document into proper data.frame's
json_df_to_ffdbdoc <- function (json_df) {
    to_numeric_or_char <- function (l) {
        withCallingHandlers((function (m) {
            withRestarts(
                as.numeric(m),
                as_char_restart = as.character)
        })(l), warning = function (w) { 
            invokeRestart('as_char_restart', l)
        })
    }

    # Take a FFDB data.frame structure and convert it into R
    do.call(data.frame, c(list(
            row.names = json_df$`_headings`$values,
            stringsAsFactors = FALSE
        ), lapply(json_df[json_df$`_headings`$fields], to_numeric_or_char)))
}


# (internal use only) Given (doc) from ffdb_fetch(), write a DLMtool-compatible csv to the (output) file handle
ffdbdoc_to_dlmtool_csv <- function (doc, output = stdout()) {
    null_to_na <- function (x) { ifelse(is.null(x), NA, ifelse(identical(x, "NA"), NA, x)) }

    first_line <- TRUE
    write_line <- function(line_name, x) {
        cat(line_name, x, "\n", file = output, sep=",", append=!(first_line))
        first_line <<- FALSE
    }

    write_line('Name', as.character(doc$metadata[1, "species"]))
    write_line('Year', as.numeric(rownames(doc$catch)))
    for (i in seq_len(ncol(doc$catch))) {
        write_line(ifelse(i == 1, 'Catch', 'Abundance index'), as.numeric(doc$catch[,i]))
    }
    write_line('Duration t', length(rownames(doc$catch)))
    write_line('Average catch over time t', mean(doc$catch$catch))
    write_line('Depletion over time t', doc$constants[1, "depletion_over_time"])
    write_line('M', doc$constants[1, "M"])
    write_line('FMSY/M', as.numeric(null_to_na(doc$constants[1, "FMSY.M"])))
    write_line('BMSY/B0', as.numeric(null_to_na(doc$constants[1, "BMSY.B0"])))
    write_line('MSY', as.numeric(null_to_na(doc$constants[1, "MSY"])))
    write_line('BMSY', as.numeric(null_to_na(doc$constants[1, "BMSY"])))
    write_line('Length at 50% maturity', as.numeric(null_to_na(doc$constants[1, "length_at_50pc_maturity"])))
    write_line('Length at 95% maturity', as.numeric(null_to_na(doc$constants[1, "length_at_95pc_maturity"])))
    write_line('Length at first capture', as.numeric(null_to_na(doc$constants[1, "length_at_first_capture"])))
    write_line('Length at full selection', as.numeric(null_to_na(doc$constants[1, "length_at_full_selection"])))
    if (!all(is.na(doc$caa))) {
        for (r in rownames(doc$caa)) {
            write_line(paste('CAA', r), as.numeric(doc$caa[r,]))
        }
    }
    if (!all(is.na(doc$cal))) {
        for (r in rownames(doc$cal)) {
            write_line(ifelse(r == "Min Length", "CAL_bins",paste('CAL', r)), as.numeric(doc$cal[r,]))
        }
    } else {
        write_line("CAL_bins", "")
    }
    write_line('Current stock depletion', as.numeric(null_to_na(doc$constants[1, "current_stock_depletion"])))
    write_line('Current stock abundance', as.numeric(null_to_na(doc$constants[1, "current_stock_abundance"])))
    write_line('Von Bertalanffy K parameter', as.numeric(null_to_na(doc$constants[1, "Von_Bertalanffy_K"])))
    write_line('Von Bertalanffy Linf parameter', as.numeric(null_to_na(doc$constants[1, "Von_Bertalanffy_Linf"])))
    write_line('Von Bertalanffy t0 parameter', as.numeric(null_to_na(doc$constants[1, "Von_Bertalanffy_t0"])))
    write_line('Length-weight parameter a', as.numeric(null_to_na(doc$constants[1, "Length.weight_parameter_a"])))
    write_line('Length-weight parameter b', as.numeric(null_to_na(doc$constants[1, "Length.weight_parameter_b"])))
    write_line('Steepness', as.numeric(NA))
    write_line('Maximum age', as.numeric(null_to_na(doc$constants[1, "maximum_age"])))
    write_line('CV Catch', as.numeric(null_to_na(doc$cv[1, "catch"])))
    write_line('CV Depletion over time t', as.numeric(null_to_na(doc$cv[1, "depletion_over_time"])))
    write_line('CV Average catch over time t', as.numeric(null_to_na(doc$cv[1, "avg_catch_over_time"])))
    write_line('CV Abundance index', as.numeric(null_to_na(doc$cv[1, "abundance_index"])))
    write_line('CV M', as.numeric(null_to_na(doc$cv[1, "M"])))
    write_line('CV FMSY/M', as.numeric(null_to_na(doc$cv[1, "FMSY/M"])))
    write_line('CV BMSY/B0', as.numeric(null_to_na(doc$cv[1, "BMSY/B0"])))
    write_line('CV current stock depletion', as.numeric(null_to_na(doc$cv[1, "current_stock_depletion"])))
    write_line('CV current stock abundance', as.numeric(null_to_na(doc$cv[1, "current_stock_abundance"])))
    write_line('CV von B. K parameter', as.numeric(null_to_na(doc$cv[1, "Von_Bertalanffy_K"])))
    write_line('CV von B. Linf parameter', as.numeric(null_to_na(doc$cv[1, "Von_Bertalanffy_Linf"])))
    write_line('CV von B. t0 parameter', as.numeric(null_to_na(doc$cv[1, "Von_Bertalanffy_t0"])))
    write_line('CV Length at 50% maturity', as.numeric(null_to_na(doc$cv[1, "length_at_50pc_maturity"])))
    write_line('CV Length at first capture', as.numeric(null_to_na(doc$cv[1, "length_at_first_capture"])))
    write_line('CV Length at full selection', as.numeric(null_to_na(doc$cv[1, "length_at_full_selection"])))
    write_line('CV Length-weight parameter a', as.numeric(null_to_na(doc$cv[1, "Length-weight_parameter_a"])))
    write_line('CV Length-weight parameter b', as.numeric(null_to_na(doc$cv[1, "Length-weight_parameter_b"])))
    write_line('CV Steepness', as.numeric(NA))
    write_line('Sigma length composition', as.numeric(null_to_na(doc$cv[1, "length_composition"])))
    write_line('Units', 'metric tonnes')
    write_line('Reference OFL', as.numeric(NA))
    write_line('Reference OFL type', as.numeric(NA))
    write_line('MPrec', as.numeric(NA))
    write_line('LHYear', as.numeric(null_to_na(rownames(doc$catch)[[length(rownames(doc$catch))]])))
}
