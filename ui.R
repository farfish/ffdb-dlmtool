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
      br(),
      downloadButton("download_csv", "Download DLMtool CSV")
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
                      tableOutput("mpTable")))),
          tabPanel("SPiCt",
              checkboxInput("spict_seaprod", "Seasonal Productivity", value = FALSE),
              checkboxInput("spict_timevaryinggrowth", "Time-varying growth", value = FALSE),
              tabsetPanel(id = "spict_tabs",
                  tabPanel("SPiCt visualization",
                      withSpinner(plotOutput("spictDataPlot"))
                      ),
                  tabPanel("SPiCt results summary",
                      withSpinner(plotOutput("spictFitPlot")),
                      withSpinner(verbatimTextOutput("spictFitMessage"))
                      ),
                  tabPanel("SPiCt diagnostics plot",
                      withSpinner(plotOutput("spictDiagnosticsPlot"))
                      ))))
    )
  ),
  includeHTML("footer.html")
)
