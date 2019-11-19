library(unittest)

source('dlmtool_glossary.R')

ok_group('dlmtool_help_link', {
    ok(ut_cmp_identical(dlmtool_help_link(c('CC1', 'CC5', 'CCnonexistant')), structure(c(
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC1</a>',
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC5</a>',
        'CCnonexistant'
    ), names=c('CC1', 'CC5', 'CCnonexistant'))), "Resolve function -> help topic, nonexistant items don't have a link")

    ok(ut_cmp_identical(dlmtool_help_link(as.factor(c('CC1', 'CC5', 'CCnonexistant'))), structure(c(
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC1</a>',
        '<a href="https://dlmtool.github.io/DLMtool/reference/CC1.html" target="_blank">CC5</a>',
        'CCnonexistant'
    ), names=c('CC1', 'CC5', 'CCnonexistant'))), "Works with factors too")
})
