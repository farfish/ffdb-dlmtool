library(unittest)

source('./ffdbclient.R')

ok_group("json_df_to_ffdbdoc", {
  json_df <- list(
      catch = c(NA, "200", "104.5"),
      abundance_index = c("", "100", "104.5"),
      some_string = c("cuthbert", "dibble", "grub"),
      "_headings" = list(
          fields = c('catch', 'abundance_index', 'some_string'),
          values = c('2000', '2001', '2002')
      )
  )
  ok(cmp_identical(json_df_to_ffdbdoc(json_df), data.frame(
      catch = c(NA, 200.0, 104.5),  # "NA" converted to NA
      abundance_index = c(NA, 100.0, 104.5),  # "" converted to NA
      some_string = c("cuthbert", "dibble", "grub"),  # Haven't converted string vectors into NA
  row.names = c('2000', '2001', '2002'), stringsAsFactors = FALSE)), "NA's converted correctly")
})

ok_group("dlmtool_fixup", {
    out <- dlmtool_fixup(list(
        catch = data.frame(
            catch = c(100, 200, 300, 400),
            abundance_index_1 = c(1, 2, 3, 4),
            row.names = c('2001', '2002', '2003', '2004'),
            stringsAsFactors = FALSE)
    ))
    ok(cmp_identical(out, list(
        catch = data.frame(
            catch = c(100, 200, 300, 400),
            abundance_index_1 = c(1, 2, 3, 4),
            year = as.integer(c(2001, 2002, 2003, 2004)),
            month = as.integer(c(1, 1, 1, 1)),
            row.names = c('2001_1', '2002_1', '2003_1', '2004_1'),
            stringsAsFactors = FALSE),
        abundance_index_1 = data.frame(
            index = c(1, 2, 3, 4),
            year = as.integer(c(2001, 2002, 2003, 2004)),
            month = as.integer(c(1, 1, 1, 1)),
            row.names = c('2001_1', '2002_1', '2003_1', '2004_1'),
            stringsAsFactors = FALSE))
    ), "Moved abundance index to it's own table")

    out <- dlmtool_fixup(list(
        catch = data.frame(
            catch = c(100, 200, 300, 400),
            row.names = c('2001', '2002', '2003', '2004'),
            stringsAsFactors = FALSE),
        abundance_index = data.frame(
            abundance_index_1 = c(1, 2, 3, 4),
            abundance_index_2 = c(21, 22, 23, 24),
            row.names = c('2001', '2002', '2003', '2004'),
            stringsAsFactors = FALSE)
    ))
    ok(cmp_identical(out, list(
        catch = data.frame(
            catch = c(100, 200, 300, 400),
            year = as.integer(c(2001, 2002, 2003, 2004)),
            month = as.integer(c(1, 1, 1, 1)),
            row.names = c('2001_1', '2002_1', '2003_1', '2004_1'),
            stringsAsFactors = FALSE),
        abundance_index = data.frame(
            abundance_index_1 = c(1, 2, 3, 4),
            abundance_index_2 = c(21, 22, 23, 24),
            row.names = c('2001', '2002', '2003', '2004'),
            stringsAsFactors = FALSE),
        abundance_index_1 = data.frame(
            index = c(1, 2, 3, 4),
            year = as.integer(c(2001, 2002, 2003, 2004)),
            month = as.integer(c(1, 1, 1, 1)),
            row.names = c('2001_1', '2002_1', '2003_1', '2004_1'),
            stringsAsFactors = FALSE),
        abundance_index_2 = data.frame(
            index = c(21, 22, 23, 24),
            year = as.integer(c(2001, 2002, 2003, 2004)),
            month = as.integer(c(1, 1, 1, 1)),
            row.names = c('2001_1', '2002_1', '2003_1', '2004_1'),
            stringsAsFactors = FALSE))
    ), "Moved abundance index table into separate tables")

    out <- dlmtool_fixup(list(
        abundance_index_pelagio = data.frame(
            month = c("2", "NA", "NA", "7"),
            index = c(1, 2, 3, 4),
            row.names = c('2001_1', '2001_6', '2002_1', '2002_6'),
            stringsAsFactors = FALSE)
    ))
    ok(cmp_identical(out$abundance_index_pelagio, data.frame(
            month = c(2, 6, 1, 7),
            index = c(1, 2, 3, 4),
            year = as.integer(c(2001, 2001, 2002, 2002)),
            row.names = c('2001_2', '2001_6', '2002_1', '2002_7'),
            stringsAsFactors = FALSE)), "Convert custom months")
})
