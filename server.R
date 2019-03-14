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
  updateSelectInput(session, "document_name", choices = ffdb_list('dlmtool', instance = conn), selected = "demo-cobia")
  poolReturn(conn)

  output$catchPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)

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

  output$caaPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)

    caa <- d@CAA[1,,]
    dimnames(caa) <- list(
        year=tail(d@Year, dim(d@CAA)[2]),
        age=1:ncol(caa))
    
    ggplot(as.data.frame(as.table(caa)), aes(age,Freq)) +
        geom_bar(stat="identity")+facet_wrap(~year)
  })

  output$calPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)
      
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

  output$parameterDistributionsPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)

    summary(d, wait=FALSE, plots=c('PD'))
  })

  output$download_csv <- downloadHandler(
      filename = function() {
          paste(input$document_name, ".csv", sep="")
      },
      content = function(file) {
          doc <- ffdb_fetch('dlmtool', input$document_name, instance = conn)
          ffdbdoc_to_dlmtool_csv(doc, output = file)
      }
  )

  output$canTable <- renderTable({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)
    out <- merge(data.frame(Code = Can(d)), dlmtool_methods, by = "Code", all.x = TRUE)
    out[with(out, order(Direction, Code)), colnames(dlmtool_methods)]
  })

  output$cantTable <- renderTable({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)
    out <- as.data.frame(Cant(d))
    colnames(out) <- c("Code", "Reason")
    out <- merge(out, dlmtool_methods, by = "Code", all.x = TRUE)
    out[with(out, order(Direction, Code)), c('Direction', 'Code', 'Name', 'Type', 'Reason')]
  })

  output$mpBoxPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)
    d_tac <- runMP(d, reps=1000)
    boxplot(d_tac)
  })

  output$mpTable <- renderTable({
    if (nchar(input$document_name) == 0) {
        return()
    }
    d <- ffdb_to_dlmtool(input$document_name, instance = conn)
    # Find all possible output methods
    out <- merge(data.frame(Code = Can(d)), dlmtool_methods, by = "Code")
    out[which(out$Direction == 'output'), colnames(out) != 'Direction']
  })
}

# conn <- poolCheckout(pool)
# getDocumentNames(conn)
# doc <- getDLMToolData(conn, getDocumentNames(conn)[[1]])
# catch <- ffdbToDataFrame(doc$catch)
# poolClose(pool)
