library(shinycssloaders)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  tags$head(
      tags$link(rel="stylesheet", type="text/css", href="//fonts.googleapis.com/css?family=Open+Sans:400,600"),
      includeCSS("header.css"),
      includeCSS("footer.css"),
      includeScript("local-config.js"),
      includeScript("tracking.js")),
  includeHTML("header.html"),
  a(icon("github", lib = "font-awesome"),
                   href="https://github.com/farfish/ffdb-dlmtool",
                   class="navbar-brand",
                   style="position: absolute; top: 0; right: 0"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,

      selectInput(
          "document_name",
          "Document name",
          list(),
          ''),

      a("Upload/edit data in FFDB", href="/upload?template=dlmtool"),
      br(),
      br()
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(id = "main_tabs",
          tabPanel(
              "Catch / Abundance Index",
              withSpinner(plotOutput("catchPlot", height=600)),
              downloadButton("catchPlotDownload", label = "Download plot")),
          tabPanel("CAA",
              withSpinner(plotOutput("caaPlot", height=600)),
              downloadButton("caaPlotDownload", label = "Download plot")),
          tabPanel("CAL",
              withSpinner(plotOutput("calPlot", height=600)),
              downloadButton("calPlotDownload", label = "Download plot")),
          tabPanel("DLMTool",
              a(id = "download_dlmcsv", class = "btn btn-link shiny-download-link", icon("download"), "Download DLMtool CSV"),
              tabsetPanel(id = "dlmtool_tabs",
                  tabPanel("Parameter Distributions",
                      withSpinner(plotOutput("parameterDistributionsPlot", height=600)),
                      downloadButton("parameterDistributionsPlotDownload", label = "Download plot")),
                  tabPanel("DLMTool Diagnostics",
                      h2("Enough data to produce"),
                      tableOutput("canTable"),
                      h2("Cannot produce"),
                      tableOutput("cantTable")),
                  tabPanel("TAC Plot",
                      p(strong("Please Note:"), "This tool cannot be used for management purposes, it requires previous knowledge or training on stock assessment theory"),
                      withSpinner(plotOutput("mpBoxPlot")),
                      downloadButton("mpBoxPlotDownload", label = "Download plot"),
                      downloadButton("mpResultDownload", label = "Download result table"),
                      checkboxGroupInput("mpLegend", "MPs to show:", inline = TRUE)
                      ))),
          tabPanel("SPiCt",
              a(id = "download_spict", class = "btn btn-link shiny-download-link", icon("download"), "Download SPiCt data object"),
              p(
                  "For more information on SPiCt, please see the",
                  a("SPiCt guideline document", href = "https://github.com/DTUAqua/spict/blob/master/spict/vignettes/spict_guidelines.pdf"),
                  "or the",
                  a("SPiCt manual", href = "https://github.com/DTUAqua/spict/raw/master/spict/inst/doc/spict_manual.pdf"),
                  ""),
              tabsetPanel(id = "spict_tabs",
                  tabPanel("SPiCt visualization",
                      withSpinner(plotOutput("spictDataPlot", height = "700px"))
                      ),
                  tabPanel("SPiCt results summary",
                      withSpinner(plotOutput("spictFitPlot", height = "700px")),
                      withSpinner(verbatimTextOutput("spictFitMessage"))
                      ),
                  tabPanel("SPiCt diagnostics plot",
                      withSpinner(plotOutput("spictDiagnosticsPlot", height = "700px"))
                      ))))
    )
  ),
  includeHTML("footer.html")
)
