library(xml2)

dlmtool_method_info <- function (method_code) {
    help_file <- tryCatch(utils:::.getHelpFile(help(method_code, 'DLMtool')), error = function (e) character())
    if (length(help_file) == 0) {
        return(c(code = method_code, name = '', description = ''))
    }

    doc <- xml2::read_html(paste(capture.output(tools:::Rd2HTML(help_file)), collapse = "\n"))
    
    return(c(
        code = method_code,
        name = xml2::xml_text(xml2::xml_find_first(doc, '/html/body/h2')),
        description = xml2::xml_text(xml2::xml_find_first(doc, "/html/body/h3[text() = 'Description']/following-sibling::p"))))
}
