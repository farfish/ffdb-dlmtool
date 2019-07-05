library(xml2)
library(DLMtool)

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

# Decorate character vector of DLMtool (method_codes) with HTML links to their documentation
dlmtool_help_link <- function (method_codes) {
    dlmtool_help_url <- function (method_codes) {
        vapply(method_codes, function (x) {
            out <- as.character(help(x, 'DLMtool', try.all.packages = FALSE, help_type = 'text'))
            if (length(out) > 0) return(paste0(gsub('.*/', 'https://dlmtool.github.io/DLMtool/reference/', out), '.html'))
            return("")
        }, "")
    }

    method_codes <- as.character(method_codes)  # NB: Resolve any factors first
    urls <- dlmtool_help_url(method_codes)
    ifelse(urls == "", method_codes, paste0('<a href="', urls, '" target="_blank">', method_codes, '</a>'))
}
