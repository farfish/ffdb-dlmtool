library(unittest)

cmp <- function(a, b) {
    if(identical(all.equal(a,b), TRUE)) return(TRUE)
    if (file.exists(Sys.which('git'))) {
        totmp <- function(x) {
            f <- tempfile(pattern = "str.")
            capture.output(str(x,
                vec.len = 1000,
                digits.d = 5,
                nchar.max = 1000), file = f)
            return(f)
        }
        return(suppressWarnings(system2(
            Sys.which('git'),
            c("diff", "--no-index", "--color-words", totmp(a), totmp(b)),
            input = "",
            stdout = TRUE, stderr = TRUE)))
    }
    return(c(
        capture.output(str(a)),
        "... does not equal...",
        capture.output(str(b))
    ))
}

source('dlmtool_glossary.R')

ok_group('dlmtool_help_link', {
    ok(cmp(dlmtool_help_link(c('CC1', 'CC5', 'CCnonexistant')), structure(c(
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC1</a>',
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC5</a>',
        'CCnonexistant'
    ), names=c('CC1', 'CC5', 'CCnonexistant'))), "Resolve function -> help topic, nonexistant items don't have a link")

    ok(cmp(dlmtool_help_link(as.factor(c('CC1', 'CC5', 'CCnonexistant'))), structure(c(
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC1</a>',
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC5</a>',
        'CCnonexistant'
    ), names=c('CC1', 'CC5', 'CCnonexistant'))), "Works with factors too")
})
