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
    sidebarPanel(

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

      tabsetPanel(type = "tabs", id = "main_tabs",
                  tabPanel("Catch / Abundance Index Plot",
                      withSpinner(plotOutput("catchPlot", height=600)),
                      downloadButton("catchPlotDownload", label = "Download plot")),
                  tabPanel("CAA",
                      withSpinner(plotOutput("caaPlot", height=600)),
                      downloadButton("caaPlotDownload", label = "Download plot")),
                  tabPanel("CAL",
                      withSpinner(plotOutput("calPlot", height=600)),
                      downloadButton("calPlotDownload", label = "Download plot")),
                  tabPanel("Parameter Distributions",
                      withSpinner(plotOutput("parameterDistributionsPlot", height=600)),
                      downloadButton("parameterDistributionsPlotDownload", label = "Download plot")),
                  tabPanel("Diagnostics",
                      h2("Enough data to produce"),
                      tableOutput("canTable"),
                      h2("Cannot produce"),
                      tableOutput("cantTable")),
                  tabPanel("TAC Plot",
                      withSpinner(plotOutput("mpBoxPlot")),
                      downloadButton("mpBoxPlotDownload", label = "Download plot"),
                      tableOutput("mpTable"))
                  )
    )
  ),
  includeHTML("footer.html")
)
