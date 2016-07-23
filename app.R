rm(list = ls())
source(file = "Auxiliary-Functions.R")

## Make sure latest version of DT
ipackages <- installed.packages()
if (ipackages[which(names(ipackages[, 1]) == "DT"), ][3] != "0.1.60")
{
  devtools::install_github("Rstudio/DT")
} else {
  library("DT")
}

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
      selectInput("select", label = h3("Select box"), 
      choices = list("Requests" = "clean_requests", 
                     "Synthetic Dataset" = "synthetic_data"), 
      selected = "clean_requests")
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
  observeEvent(input$select, {
    print(as.character(input$select))
  })
  
  table_values <- eventReactive(input$select, {
    table_name <- as.character(input$select)
    dt <- read_rds(table_name, "dt")
    anomalies <- read_rds(table_name, "anomalies")
    clusters <- read_rds(table_name, "clusters")
    tuples_dt <- read_rds(table_name, "tuples_dt")
    time_windows <- unique(dt$time_window)
    anomaly_times <- data.table(time_window = time_windows[anomalies$index])
    anomaly_times <- anomaly_times[1:(nrow(anomaly_times) - 6)]
    anomaly_times[, anomaly_num := sequence(.N), by = as.Date(time_window)]
    if (table_name == "synthetic_data")
    {
      win_size = 8
    } else {
      win_size = 5
    }
    ts <- get_ts_from_dt(dt, win_size, type = "xts")
    ts_dt <- as.data.table(time(ts))
    setnames(ts_dt, c("time_window"))
    table_values <- list(dt = dt, 
                         anomalies = anomalies, 
                         clusters = clusters,
                         tuples_dt = tuples_dt,
                         anomaly_times = anomaly_times,
                         ts = ts,
                         ts_dt = ts_dt)
    table_values
  })
  
  observeEvent(input$anomaly_selector_rows_selected, {
    print(as.numeric(input$anomaly_selector_rows_selected))
  })
  
  row_selected <- eventReactive(input$anomaly_selector_rows_selected, {
    as.numeric(tail(input$anomaly_selector_rows_selected, 1))
  })
  
  output$ts <- renderPlot({
    vals <- table_values()
    ts <- vals$ts
    anomalies <- vals$anomalies
    clusters <- vals$clusters
    table_name <- vals$table_name
    plot(ts, main = table_name)
    lines(ts[anomalies$index] ~ time(ts)[anomalies$index]
          ,type = "p"
          ,col = as.character(clusters))
    points(ts[anomalies$index[row_selected()]], pch = 20, cex = 2,
                        col = as.character(clusters[row_selected()]))
  })
  
  output$anomaly_selector <- DT::renderDataTable({
    table_values()$anomaly_times}, 
    selection = list(mode = 'single', selected = c(1)),
    options = list(pageLength = 6)
  )
    
  output$scoreboard <- DT::renderDataTable({
    vals <- table_values()
    anomaly_times <- vals$anomaly_times
    tuples_dt <- vals$tuples_dt
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