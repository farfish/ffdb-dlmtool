library(spict)


ffdbdoc_to_spictstock <- function (doc, seaprod = FALSE, timevaryinggrowth = FALSE) {
    # Non-null, non-empty string (seaprod/timevaryinggrowth might be missing for old documents)
    is_truthy <- function (x) nchar(paste0(x[!is.na(x)], collapse = "")) > 0

    samplestock<-list(
        seasontype = 1,  # use the spline-based representation of seasonality
        splineorder = 3,
        seaprod = ifelse(is_truthy(doc$constants[1, 'seaprod']), 3, 0),
        timevaryinggrowth = is_truthy(doc$constants[1, 'timevaryinggrowth']),
        dteuler = 1/16)

    # Turn tables into obs/time lists
    indices <- grep('^catch$|^abundance_index_', names(doc), value = TRUE)
    data <- lapply(structure(indices, names = indices), function (tbl_name) {
        value_col <- ifelse(tbl_name == 'catch', 'catch', 'index')
        tbl <- doc[[tbl_name]][!is.na(doc[[tbl_name]][[value_col]]), ]

        list(
            obs = tbl[[value_col]],
            time = tbl$year + ((tbl$month - 1) %/% 3) / 4  # Floor to nearest quarter
        )
    })

    # Add catch to obsC/timeC
    samplestock$obsC <- ifelse(data$catch$obs < 0.001, 0.001, data$catch$obs)
    samplestock$timeC <- data$catch$time
    data$catch <- NULL

    # Add everything else to obsI/timeI
    samplestock$obsI <- lapply(data, function (d) d$obs)
    samplestock$timeI <- lapply(data, function (d) d$time)

    # Count seasons in catch data
    samplestock$nseasons <- max(spict::annual(samplestock$timeC, samplestock$timeC, type = length)$annvec)

    return(check.inp(samplestock))
}
