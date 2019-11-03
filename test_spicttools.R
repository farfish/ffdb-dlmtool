library(unittest)

source('./spicttools.R')

ok_group("ffdbdoc_to_spictstock", {
    st <- ffdbdoc_to_spictstock(dlmtool_fixup(list(  # NB: dlmtool_fixup will do some processing for us
        catch = data.frame(
            row.names = c('2000_1', '2000_3', '2000_7', '2001_1'),
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

    ok(cmp_identical(st$obsC, c("2000_3" = 100,"2000_7" = 110, "2001_1" = 120)),
        "Generated obsC, skipping NA entries")

    ok(cmp_identical(st$timeC, c(2000 + (2/12), 2000 + (6/12), 2001)),
        "Generated timeC, fractional months")

    ok(cmp_identical(st$obsI, list(
        abundance_index_frank = c("2000_1" = 1, "2002_1" = 2),
        abundance_index_gelda = c("2000_1" = 3, "2001_1" = 4))), "Generated obsI")

    ok(cmp_identical(st$timeI, list(
        abundance_index_frank = c(2000, 2002),
        abundance_index_gelda = c(2000, 2001))), "Generated timeI")
})
