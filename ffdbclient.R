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
ffdb_fetch <- function (template_name, document_name, convert = TRUE, instance = 'https://ffdb.farfish.eu') {
    out <- NULL
    if (class(instance) == "PqConnection") {
        # instance is a database connection, fetch directly
        res <- dbSendQuery(instance, paste0(
            "SELECT content",
            " FROM document",
            " WHERE template_name = '", template_name, "'",
            " AND document_name = '", document_name, "'",
            " ORDER BY version DESC",
            " LIMIT 1",
            ""))
        out <- dbFetch(res)
        dbClearResult(res)
        if ('content' %in% names(out)) {
            # Output might be data.frame() of a pq_jsonb type, depending on DB versions
            out <- as.character(out$content)
        }
        if (length(out) > 0) {
            out <- fromJSON(out)
        }
    } else {
        # Instance is a hostname, fetch via. HTTP
        uri <- paste0(instance, '/api/doc/', template_name, '/', document_name)
        req <- GET(uri,
            add_headers('Accept' = "application/json"))
        if (http_error(req)) {
            stop(sprintf("Request failed: status %s - URL '%s'", status_code(req), uri))
        }
        resp <- fromJSON(content(req, as = "text", encoding = "UTF-8"))
        out <- resp$content
    }

    if (convert) {
        out <- lapply(out, json_df_to_ffdbdoc)

        if (template_name == 'dlmtool') {
            out <- dlmtool_fixup(out)
        }
    }
    return(out)
}


# Fetch & decode model input object
ffdb_model_input <- function (template_name, document_name, model_name, instance = 'https://ffdb.farfish.eu') {
    if (class(instance) == "PqConnection") {
        # instance is a database connection, fetch directly
        res <- dbSendQuery(instance, "
             SELECT input_rdata
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
        on.exit(dbClearResult(res))
        row <- dbFetch(res)
        if (nrow(row) == 0) stop("Model input for ", paste(template_name, document_name, model_name, sep = "/"), " not generated yet")
        return(decode_bytea(row[1,1]))
    }
    stop("Cannot fetch model input via. HTTP")
}


# Fetch & decode model output object
ffdb_model_output <- function (template_name, document_name, model_name, instance = 'https://ffdb.farfish.eu') {
    if (class(instance) == "PqConnection") {
        # instance is a database connection, fetch directly
        res <- dbSendQuery(instance, "
             SELECT output_path
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
        on.exit(dbClearResult(res))
        row <- dbFetch(res)
        if (nrow(row) == 0) stop("Model output for ", paste(template_name, document_name, model_name, sep = "/"), " not generated yet")
        out <- readRDS(row[1,1])
        if (is.null(out)) stop("Model output not generated yet, please try again later.")
        return(out)
    }
    stop("Cannot fetch model output via. HTTP")
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

dlmtool_fixup <- function (doc) {
    coalesce <- function (x, def) { ifelse(is.na(x), def, x) }

    # v1 --> v2: Move catch$abundance_index into it's own table
    if ('abundance_index_1' %in% names(doc$catch)) {
        doc$abundance_index_1 <- doc$catch[,c('abundance_index_1'), drop = FALSE]
        names(doc$abundance_index_1) <- c('index')
    }

    # v2 --> v3: Move abundance_index columns into their own tables
    if ('abundance_index' %in% names(doc)) {
        for (n in colnames(doc$abundance_index)) {
            doc[[n]] <- doc$abundance_index[,c(n), drop = FALSE]
            names(doc[[n]]) <- c('index')
        }
    }

    for (n in grep('^catch$|^abundance_index_', names(doc), value = TRUE)) {
        if ('month' %in% names(doc[[n]])) {
            custom_month <- suppressWarnings(as.numeric(doc[[n]]$month))
        } else {
            custom_month <- rep(NA, nrow(doc[[n]]))
        }
        # Default to 1, same as dimension_timeseries:update_init
        month <- vapply(strsplit(rownames(doc[[n]]), "_"), function (x) {
            as.integer(ifelse(length(x) > 1, x[[2]], 1))
        }, integer(1))
        doc[[n]]$year <- vapply(strsplit(rownames(doc[[n]]), "_"), function (x) { as.integer(x[[1]]) }, integer(1))
        doc[[n]]$month <- ifelse(is.na(custom_month), month, custom_month)
        rownames(doc[[n]]) <- paste(doc[[n]]$year, doc[[n]]$month, sep = "_")

        # Make sure values are numeric, converting "NA" --> NA
        if ('catch' %in% names(doc[[n]])) {
            doc[[n]]$catch <- suppressWarnings(as.numeric(doc[[n]]$catch))
        } else if ('index' %in% names(doc[[n]])) {
            doc[[n]]$index <- suppressWarnings(as.numeric(doc[[n]]$index))
        } else if ('abundance_index_1' %in% names(doc[[n]])) {
            doc[[n]]$index <- suppressWarnings(as.numeric(doc[[n]]$abundance_index_1))
        }
    }

    return(doc)
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


ffdbdoc_to_dlmtool <- function (ffdbdoc) {
    f <- tempfile(fileext = ".csv")
    ffdbdoc_to_dlmtool_csv(ffdbdoc, output = f)
    d <- DLMtool::XL2Data(f)
    unlink(f)
    return(d)
}


dlmtool_csv_to_ffdbdoc <- function (in_file) {
    d <- XL2Data(in_file)

    # TODO: Parse this directly? Complain about bug?
    caa <- as.data.frame(d@CAA[1,,])
    # NB: We don't know the CAA absolute years, this gets thrown away in parsing, assume it's towards the end
    rownames(caa) <- seq(d@LHYear - dim(caa)[1] + 1, d@LHYear)
    colnames(caa) <- seq(1,dim(caa)[2])

    cal <- rbind(as.data.frame(t(d@CAL_bins)), as.data.frame(cbind(d@CAL[1,,], rep(NA, length(d@CAL_bins) - dim(d@CAL)[3]))))
    # NB: We don't know the CAL absolute years, this gets thrown away in parsing, assume it's towards the end
    rownames(cal) <- c("Min Length", seq(d@LHYear - dim(cal)[1] + 2, d@LHYear))
    colnames(cal) <- seq(1,dim(cal)[2])

    list(
        metadata = data.frame(
            species = d@Name,
            location = ifelse(length(d@Region), d@Region, as.character(NA)),
            case_study = '',
            row.names = c('value'),
            stringsAsFactors = FALSE),
        catch = data.frame(
            catch = as.vector(d@Cat),
            row.names = d@Year),
        abundance_index_1 = data.frame(
            index = as.vector(d@Ind),
            row.names = d@Year),
        caa = caa,
        cal = cal,
        constants = data.frame(
            "avg_catch_over_time" = c(d@AvC, ""),
            "depletion_over_time" = c(d@Dt, ""),
            "M" = c(d@Mort, ""),
            "FMSY/M" = c(d@FMSY_M, ""),
            "BMSY/B0" = c(d@BMSY_B0, ""),
            # "MSY" = c(d@, ""),  # TODO: These values in CSV are ignored, would need to fetch directly
            # "BMSY" = c(d@, ""),  # TODO: These values in CSV are ignored, would need to fetch directly
            "length_at_50pc_maturity" = c(d@L50, ""),
            "length_at_95pc_maturity" = c(d@L95, ""),
            "length_at_first_capture" = c(d@LFC, ""),
            "length_at_full_selection" = c(d@LFS, ""),
            "current_stock_depletion" = c(d@Dep, ""),
            "current_stock_abundance" = c(d@Abun, ""),
            "Von_Bertalanffy_K" = c(d@vbK, ""),
            "Von_Bertalanffy_Linf" = c(d@vbLinf, ""),
            "Von_Bertalanffy_t0" = c(d@vbt0, ""),
            "Length-weight_parameter_a" = c(d@wla, ""),
            "Length-weight_parameter_b" = c(d@wlb, ""),
            "maximum_age" = c(d@MaxAge, ""),
            "ref_ofl_limit" = c(d@Ref, ""),  # TODO: Right?
            row.names = c('value', 'source'),
            stringsAsFactors = FALSE),
        cv = data.frame(
            row.names = c('value', 'source'),
            # "catch" = c(d@CV_Cat, ""),  # TODO:
            "depletion_over_time" = c(d@CV_Dt, ""),
            "avg_catch_over_time" = c(d@CV_AvC, ""),
            # "abundance_index" = c(d@CV_Ind, ""),  # TODO:
            "M" = c(d@CV_Mort, ""),
            "FMSY/M" = c(d@CV_FMSY_M, ""),
            "BMSY/B0" = c(d@CV_BMSY_B0, ""),
            "current_stock_depletion" = c(d@CV_Dep, ""),
            "current_stock_abundance" = c(d@CV_Abun, ""),
            "Von_Bertalanffy_K" = c(d@CV_vbK, ""),
            "Von_Bertalanffy_Linf" = c(d@CV_vbLinf, ""),
            "Von_Bertalanffy_t0" = c(d@CV_vbt0, ""),
            "length_at_50pc_maturity" = c(d@CV_L50, ""),
            "length_at_first_capture" = c(d@CV_LFC, ""),
            "length_at_full_selection" = c(d@CV_LFS, ""),
            "Length-weight_parameter_a" = c(d@CV_wla, ""),
            "Length-weight_parameter_b" = c(d@CV_wlb, ""),
            # "length_composition" = c(d@sigmaR, ""),  # TODO:
            stringsAsFactors = FALSE))
}

# (internal use only) Given (doc) from ffdb_fetch(), write a DLMtool-compatible csv to the (output) file handle
ffdbdoc_to_dlmtool_csv <- function (doc, output = stdout()) {
    null_to_na <- function (x) { ifelse(is.null(x), NA, ifelse(identical(x, "NA"), NA, x)) }

    first_line <- TRUE
    write_line <- function(line_name, x) {
        cat(paste0(c(line_name, x), collapse = ","),
            "\r\n",
            sep="",
            file = output,
            append=!(first_line))
        first_line <<- FALSE
    }

    write_line('Name', as.character(doc$metadata[1, "species"]))

    # Merge catch and abundance index data
    combined <- merge(
        doc$catch[,c('catch'),drop = FALSE],  # i.e. don't pick up any old abundance_index
        doc[[grep('^abundance_index_', names(doc), value = TRUE)[[1]]]],
        by = "row.names", all.x = TRUE, all.y = TRUE)
    combined$year <- gsub('_[0-9]+$', '', combined$Row.names)
    # Combine years together
    combined <- aggregate(combined, list(year = combined$year), function (x) {
        out <- mean(suppressWarnings(as.numeric(x)), na.rm = TRUE)
        out <- ifelse(is.nan(out), NA, out)
        out
    })
    combined <- combined[order(as.numeric(combined$year)),]

    write_line('Year', combined$year)
    write_line('Catch', combined$catch)
    write_line("Abundance index", combined$index)
    write_line('Duration t', length(combined$year))
    write_line('Average catch over time t', doc$constants[1, "avg_catch_over_time"])
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
    write_line('Units', 'metric tonnes')  # TODO: Needs to be fetched directly
    write_line('Reference OFL', as.numeric(null_to_na(doc$cv[1, "ref_ofl_limit"])))
    write_line('Reference OFL type', as.numeric(NA))
    write_line('MPrec', as.numeric(NA))
    write_line('LHYear', as.numeric(null_to_na(combined$year[[length(combined$year)]])))
}

# Convert arbitary object to blob::blob, for insertion into a BYTEA field
encode_bytea <- function (object) {
    tf <- tempfile(fileext = ".rds")
    on.exit(unlink(tf))

    saveRDS(object, file = tf, compress = FALSE)
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
