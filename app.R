rm(list = ls())
## Install/Load Packages
source(file = "Auxiliary-Functions.R")
ipak(c("shiny", "shinythemes", "dygraphs", "zoom", "ggplot2", "zoo"))
Sys.setenv(TZ = "GMT")
table_name <- "clean_requests" #input
table_name <- input$date
dt <- read_rds(table_name, "dt")
anomalies <- read_rds(table_name, "anomalies")
clusters <- read_rds(table_name, "clusters")
tuples_dt <- read_rds(table_name, "tuples_dt", date)
time_windows <- unique(dt$time_window)
anomaly_times <- data.table(time_window = time_windows[anomalies$index])
anomaly_times[, anomaly_num := sequence(.N), by = as.Date(time_window)]
ts <- get_ts_from_dt(dt, win_size = 5, type = "xts")
ts_dt <- as.data.table(time(ts))
setnames(ts_dt, c("time_window"))

date <- as.Date(max(dt$TimeStamp))
# 
# test <- dygraph(ts)
# test$x$events
# test$x$annotations

ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Lobster');
                      "))
      ),
    fluidRow(
      h1("Anomaly Lead Detector", align = "center", style = "font-family: 'Lobster'")
    ),
    fluidRow(
      column(6,
        dateInput('date',
                  label = 'Date input: yyyy-mm-dd',
                  value = as.Date(max(dt$TimeStamp))
        )
      ), column(6,
          selectInput("select", label = h3("Select box"), 
          choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
          selected = 1))
    ),
    fluidRow(
      column(8, 
        plotOutput(outputId = "ts", click = "plot_click", width = "100%")),
      column(4, 
              DT::dataTableOutput("anomaly_selector"))
    ),
    fluidRow(
      DT::dataTableOutput("scoreboard")
    )
  )
)

server <- function(input, output) {
  output$ts <- renderPlot({
    plot(ts)
    lines(ts[anomalies$index] ~ time(ts)[anomalies$index]
          ,type = "p"
          ,col = as.character(clusters))
    if (length(row_selected()))
    {
      points(ts[anomalies$index[row_selected()]], pch = 20, cex = 3, 
                          col = as.character(clusters[row_selected()]))
    }
  })
  
  output$anomaly_selector <- DT::renderDataTable(anomaly_times,
                                            selection = "single",  
                                           options = list(pageLength = 6))
  
  observeEvent(input$anomaly_selector_row_last_clicked, {
    if (is.null(input$anomaly_selector_row_last_clicked))
    {
      input$anomaly_selector_row_last_clicked <- 1
    }
  })

  row_selected <- eventReactive(input$anomaly_selector_row_last_clicked, {
    as.numeric(input$anomaly_selector_row_last_clicked)
  })
  
  # point <- eventReactive(input$anomaly_selector_row_last_clicked, {
  #   row_selected <- as.numeric(input$anomaly_selector_last_row_clicked)
  #   ts_index <- anomalies$index[row_selected]
  #   ts_index
  # })
  
  output$scoreboard <- DT::renderDataTable({
    anomaly_time_window <- as.character(anomaly_times[row_selected(), 
                                                      time_window])
    anomaly_tuples_dt <- tuples_dt[[anomaly_time_window]]
    anomaly_tuples_dt <- anomaly_tuples_dt[order(cluster_value,
                                                 overall_anomalies),
                                           .(tuple, overall_anomalies)]
    anomaly_tuples_dt},
                                           selection = "single", 
                                           options = list(pageLength = 5))
  
}

shinyApp(ui, server)