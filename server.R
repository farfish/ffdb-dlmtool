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
            dlmtool_fixup(ffdb_fetch('dlmtool', input$document_name, instance = conn)),
            seaprod = input$spict_seaprod,
            timevaryinggrowth = input$spict_timevaryinggrowth))
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
    d <- dlm_doc()
    catch <- data.frame(year=as.character(d@Year), catch=d@Cat[1,], ind=d@Ind[1,])

    theme_set(theme_bw())
    p1<-ggplot(catch, aes(year,catch)) +
        geom_bar(stat="identity") +
        ylab("Catch (in tonnes)") +
        theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(catch$year)
    p2<-ggplot(catch, aes(year,ind)) +
        geom_bar(stat="identity") +
        ylab("Relative abundance") +
        theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(catch$year)
    p1 + p2 + plot_layout(ncol = 1)
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

  output$download_csv <- downloadHandler(
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

  plotPlusDownload('mpBoxPlot', function () {
    d <- dlm_doc()
    d_tac <- runMP(d, reps=1000)
    boxplot(d_tac)
  })

  output$mpTable <- renderTable({
    d <- dlm_doc()
    # Find all possible output methods
    out <- merge(data.frame(Code = Can(d), stringsAsFactors = FALSE), dlmtool_methods, by = "Code")
    out$Code <- dlmtool_help_link(out$Code)
    out[which(out$Direction == 'output'), colnames(out) != 'Direction']
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links

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
