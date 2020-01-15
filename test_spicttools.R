library(unittest)

source('./spicttools.R')

ok_group("ffdbdoc_to_spictstock", {
    st <- ffdbdoc_to_spictstock(dlmtool_fixup(list(  # NB: dlmtool_fixup will do some processing for us
        catch = data.frame(
            row.names = c('2000_1', '2000_4', '2000_7', '2001_1'),
            catch = c(NA, 100, 110, 120),
            stringsAsFactors = FALSE),
        abundance_index_frank = data.frame(
            row.names = c('2000_1', '2001_1', '2002_1'),
            index = c(1, NA, 2),
            stringsAsFactors = FALSE),
        abundance_index_gelda = data.frame(
            row.names = c('2000_1', '2001_1', '2002_1'),
            index = c(3, 4, NA),
            stringsAsFactors = FALSE))))

    ok(ut_cmp_identical(st$obsC, c(100, 110, 120)),
        "Generated obsC, skipping NA entries")

    ok(ut_cmp_identical(st$timeC, c(2000 + (3/12), 2000 + (6/12), 2001)),
        "Generated timeC, fractional months")

    ok(ut_cmp_identical(st$obsI, list(
        abundance_index_frank = c(1, 2),
        abundance_index_gelda = c(3, 4))), "Generated obsI")

    ok(ut_cmp_identical(st$timeI, list(
        abundance_index_frank = c(2000, 2002),
        abundance_index_gelda = c(2000, 2001))), "Generated timeI")

    doc <- dlmtool_fixup(list(  # NB: dlmtool_fixup will do some processing for us
        catch = data.frame(
            row.names = c(paste('2000', seq(2, 11, by = 3), sep = "_"), paste('2001', seq(3, 12, by = 3), sep = "_")),
            catch = rep(5, 8),
            stringsAsFactors = FALSE)))
    st <- ffdbdoc_to_spictstock(doc)
    ok(ut_cmp_identical(st$timeC, seq(2000, 2001.75, by = 0.25)),
        "TimeC rounded down to quarters")
})

ok_group("ffdbdoc_to_spictstock:nseasons", {
    cs <- function(times) {
        times <- strsplit(times, " ")[[1]]
        st <- ffdbdoc_to_spictstock(dlmtool_fixup(list(
            catch = data.frame(
                row.names = times,
                catch = rep(1, length(times)),
                stringsAsFactors = FALSE))))
        return(as.numeric(st$nseasons))
    }

    ok(ut_cmp_identical(cs('2000_1 2000_4 2000_7 2000_10 2001_1'), 4),
        "Data has 4 seasons")

    ok(ut_cmp_identical(cs('2000_1 2000_6 2001_1 2001_6'), 2),
        "Data has 2 seasons")

    ok(ut_cmp_identical(cs('2000_1 2001_1 2002_1 2003_1'), 1),
        "Data has 1 season (annual)")

    ok(ut_cmp_identical(cs('2000_1 2000_4 2000_7 2001_1 2001_4 2001_7 2001_10 2002_1'), 4),
        "Data has 4 seasons, even though there's a gap")

})

ok_group("ffdbdoc_to_spictstock:options", {
    cs <- function (constants) {
        ffdbdoc_to_spictstock(dlmtool_fixup(list(
            constants = constants,
        catch = data.frame(
            row.names = c('2000_1', '2000_4', '2000_7', '2001_1'),
            catch = c(NA, 100, 110, 120),
            stringsAsFactors = FALSE))))
    }

    st <- cs(data.frame(
        stringsAsFactors = FALSE))
    ok(ut_cmp_identical(st$seaprod, 0), "seaprod always 0")
    ok(ut_cmp_identical(st$timevaryinggrowth, FALSE), "timevaryinggrowth always FALSE")
})
