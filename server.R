library(DBI)
library(jsonlite)
library(pool)
library(ggplot2)
library(memoise)
library(patchwork)
library(DLMtool)
library(data.table)
options(shiny.sanitize.errors = FALSE)

source('ffdbclient.R')
source('spicttools.R')
source('dlmtool_glossary.R')
# Cache DLMtool data objects as we create them
ffdb_to_dlmtool <- memoise(ffdb_to_dlmtool)

dlmtool_methods <- read.csv('dlmtool-methods.csv')

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Open DB pool
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = 'ffdb_db'
  )
  onStop(function() {
    poolClose(pool)
  })

  conn <- poolCheckout(pool)
  updateSelectInput(session, "document_name", choices = ffdb_list('dlmtool', instance = conn))
  poolReturn(conn)

  dlm_doc <- reactive({
    if (nchar(input$document_name) > 0) {
        return(ffdb_to_dlmtool(input$document_name, instance = conn))
    } else {
        return(NULL)
    }
  })

  spict_doc <- reactive({
    if (nchar(input$document_name) > 0) {
        return(ffdbdoc_to_spictstock(
            ffdb_fetch('dlmtool', input$document_name, instance = conn)))
    } else {
        return(NULL)
    }
  })
  spict_fit <- reactive({
    fit.spict(spict_doc())
  })

  plotPlusDownload <- function (fn_name, fn) {
      output[[fn_name]] <- renderPlot({ fn() })
      output[[paste0(fn_name, 'Download')]] <- downloadHandler(
        filename = function() { paste(input$document_name, ".", fn_name, ".png", sep="") },
        content = function(file) { png(file) ; print(fn()) ; dev.off() }
      )
  }

  plotPlusDownload('catchPlot', function () {
    doc <- ffdb_fetch('dlmtool', input$document_name, instance = conn)

    month_pallete <- c(
        "1"  = "#666666",
        "2"  = "#333333",
        "3"  = "#000000",
        "4"  = "#00FF00",
        "5"  = "#00CC00",
        "6"  = "#009900",
        "7"  = "#FF0000",
        "8"  = "#CC0000",
        "9"  = "#990000",
        "10" = "#0000FF",
        "11" = "#0000CC",
        "12" = "#000099"
    )
    theme_set(theme_bw())
    wrap_plots(lapply(grep('^catch$|^abundance_index_', names(doc), value = TRUE), function(n) {
        doc[[n]]$time <- as.integer(doc[[n]]$year) + (as.integer(doc[[n]]$month) - 1) / 12
        doc[[n]]$month <- as.factor(doc[[n]]$month)  # So we can use it as a discrete scale
        ggplot(doc[[n]], aes_string("time", ifelse(n == "catch", "catch", "index"), fill = "month")) +
            geom_bar(stat="identity") +
            ylab(n) +
            theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            theme(axis.title.x = element_blank()) +
            scale_fill_manual(values = month_pallete) +
            scale_x_continuous(breaks = doc[[n]]$year)
    }), ncol = 1)
  })

  plotPlusDownload('caaPlot', function () {
    d <- dlm_doc()
    caa <- d@CAA[1,,]
    dimnames(caa) <- list(
        year=tail(d@Year, dim(d@CAA)[2]),
        age=1:ncol(caa))
    
    ggplot(as.data.frame(as.table(caa)), aes(age,Freq)) +
        geom_bar(stat="identity")+facet_wrap(~year)
  })

  plotPlusDownload('calPlot', function () {
    d <- dlm_doc()
    cal <- d@CAL[1,,]
    dimnames(cal)<-list(
        year=tail(d@Year, dim(d@CAL)[2]),
        length=tail(d@CAL_bins, dim(d@CAL)[3])/10)  # divided to be in cms

    # Only show 6 labels on the x axis
    label_positions <- seq_len(length(colnames(cal))) %% (length(colnames(cal)) %/% 6) == 1
    label_positions <- vapply(seq_len(length(label_positions)), function (i) ifelse(label_positions[i], colnames(cal)[i], ""), "")

    ggplot(as.data.frame(as.table(cal)), aes(length,Freq)) +
        geom_bar(stat="identity") + 
        scale_x_discrete(labels = label_positions) +
        facet_wrap(~year)
  })

  plotPlusDownload('parameterDistributionsPlot', function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('PD'))
  })

  output$download_dlmcsv <- downloadHandler(
      filename = function() {
          paste(input$document_name, ".csv", sep="")
      },
      content = function(file) {
          d <- dlm_doc()
          doc <- ffdb_fetch('dlmtool', input$document_name, instance = conn)
          ffdbdoc_to_dlmtool_csv(doc, output = file)
      }
  )

  output$canTable <- renderTable({
    d <- dlm_doc()
    out <- merge(data.frame(Code = Can(d), stringsAsFactors = FALSE), dlmtool_methods, by = "Code", all.x = TRUE)
    out$Code <- dlmtool_help_link(out$Code)
    out[with(out, order(Direction, Code)), colnames(dlmtool_methods)]
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links

  output$cantTable <- renderTable({
    d <- dlm_doc()
    out <- as.data.frame(Cant(d), stringsAsFactors = FALSE)
    colnames(out) <- c("Code", "Reason")
    out <- merge(out, dlmtool_methods, by = "Code", all.x = TRUE)
    out$Code <- dlmtool_help_link(out$Code)
    out[with(out, order(Direction, Code)), c('Direction', 'Code', 'Name', 'Type', 'Reason')]
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links

  document_mps <- observeEvent(input$document_name, {
    # Document changed, so clear legend checkboxes
    updateCheckboxGroupInput(session, "mpLegend", choices = c(''), selected = c())
  })
  dlm_tac <- reactive({
    d <- dlm_doc()

    # Restrict to selected MPs if any selected
    MPs <- input$mpLegend
    if (length(MPs) == 0) MPs <- NA

    return(runMP(d, MPs = MPs, reps=1000, silent = TRUE))
  })
  output$mpBoxPlot <- renderPlot({
    d_tac <- dlm_tac()
    results <- boxplot(d_tac, col = "#237aa5")

    # Update legend with available MPs
    if (length(input$mpLegend) == 0) {
        MPs <- rev(as.character(results$MP))
        updateCheckboxGroupInput(session, "mpLegend",
            choiceValues = MPs,
            choiceNames = lapply(unname(MPs), function (m) {
                span(HTML(dlmtool_help_link(m)),
                    span(dlmtool_method_info(m)['description'], style="float:right;width:calc(100% - 10rem);padding-bottom:1rem"))
            }))
    }
  })
  output$mpBoxPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".mpBoxPlot.png", sep="") },
      content = function(file) {
          d_tac <- dlm_tac()

          png(file)
          boxplot(d_tac, col = "#237aa5")
          dev.off()
      }
  )
  output$mpResultDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".mpResult.csv", sep="") },
      content = function(file) {
          d_tac <- dlm_tac()

          pdf(file = NULL)  # NB: Using boxplot for it's result table, not it's plot
          write.csv(boxplot(d_tac), file = file)
          dev.off()
      }
  )

  output$download_spict <- downloadHandler(
      filename = function() {
          paste(input$document_name, ".RData", sep="")
      },
      content = function(file) {
          st <- spict_doc()
          save(st, file = file)
      }
  )

  plotPlusDownload('spictDataPlot', function () {
    st <- spict_doc()
    plotspict.data(st)
  })

  output$spictFitMessage <- renderPrint({
    spict_fit()
  })
  plotPlusDownload('spictFitPlot', function () {
    fit <- spict_fit()
    if (fit$opt$convergence == 0) {
      plot(fit)
    }
  })

  plotPlusDownload('spictDiagnosticsPlot', function () {
    fit <- spict_fit()

    plotspict.diagnostic(calc.osa.resid(fit))
  })
}

# conn <- poolCheckout(pool)
# getDocumentNames(conn)
# doc <- getDLMToolData(conn, getDocumentNames(conn)[[1]])
# catch <- ffdbToDataFrame(doc$catch)
# poolClose(pool)
