library(spict)


ffdbdoc_to_spictstock <- function (doc, seaprod = FALSE, timevaryinggrowth = FALSE) {
    rownames_to_num <- function (rnames) {
        coalesce <- function (x, def) { ifelse(is.na(x), def, x) }

        vapply(strsplit(rnames, "_"), function (c) {
            strtoi(c[[1]]) + (coalesce(strtoi(c[2]), 1) - 1) / 12
        }, 0)
    }

    count_seasons <- function (rnames) {
        years <- floor(rnames)
        # Maximum unique value count (i.e. number of values in a year)
        max(table(floor(years)))
    }

    col_to_vec <- function (tbl, col_name) {
        # Extract column, applying rownames
        out <- structure(
            suppressWarnings(as.numeric(tbl[, col_name])),
            names=rownames(tbl))

        out[!is.na(out)]
    }

    samplestock<-list(
        seasontype = 1,  # use the spline-based representation of seasonality
        splineorder = 3,
        seaprod = ifelse(seaprod, 3, 0),
        timevaryinggrowth = timevaryinggrowth,
        dteuler = 1/16)

    samplestock$obsC <- col_to_vec(doc$catch, 'catch')
    samplestock$obsC <- ifelse(samplestock$obsC < 0.001, 0.001, samplestock$obsC)
    # NB: We're re-parsing rownames so we don't include times for NA data points
    samplestock$timeC <- rownames_to_num(names(samplestock$obsC))
    samplestock$nseasons <- count_seasons(samplestock$timeC)

    indices <- grep('abundance_index_', names(doc), value = TRUE)
    samplestock$obsI <- lapply(structure(indices, names = indices), function (ai_name) {
        return(col_to_vec(doc[[ai_name]], 'index'))
    })
    samplestock$timeI <- lapply(samplestock$obsI, function (obsI) {
        return(rownames_to_num(names(obsI)))
    })

    return(check.inp(samplestock))
}
