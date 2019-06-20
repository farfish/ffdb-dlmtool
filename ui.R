# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(
    fluidRow(
      column(1, img(
        width = 89, height = 83,
        src = "https://www.farfish.eu/wp-content/uploads/2017/08/FarFish-300x281.png"
      )),
      column(9, "Farfish DLMtool")
    )
  , windowTitle = "Farfish DLMtool"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput(
          "document_name",
          "Document name",
          list('demo-cobia'),
          'demo-cobia'),

      a("Upload/edit data in FFDB", href="/upload"),
      br(),
      br(),
      downloadButton("download_csv", "Download DLMtool CSV")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Catch / Abundance Index Plot",
                      plotOutput("catchPlot", height=600),
                      downloadButton("catchPlotDownload", label = "Download plot")),
                  tabPanel("CAA",
                      plotOutput("caaPlot", height=600),
                      downloadButton("caaPlotDownload", label = "Download plot")),
                  tabPanel("CAL",
                      plotOutput("calPlot", height=600),
                      downloadButton("calPlotDownload", label = "Download plot")),
                  tabPanel("Parameter Distributions",
                      plotOutput("parameterDistributionsPlot", height=600),
                      downloadButton("parameterDistributionsPlotDownload", label = "Download plot")),
                  tabPanel("Diagnostics",
                      h2("Enough data to produce"),
                      tableOutput("canTable"),
                      h2("Cannot produce"),
                      tableOutput("cantTable")),
                  tabPanel("TAC Plot",
                      plotOutput("mpBoxPlot"),
                      downloadButton("mpBoxPlotDownload", label = "Download plot"),
                      tableOutput("mpTable"))
                  )
    )
  )
)
