source(file = "Auxiliary-Functions.R")

## Install Packages

ipak(c("shiny", "shinythemes"))


ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Lobster');
                      "))
      ),
    fluidRow(
      h1("Local Anomaly Detector", align = "center", style = "font-family: 'Lobster'")
    ),
    fluidRow(
      column(7,
             plotOutput(outputId = "ts")
      ),
      
      column(5,
             DT::dataTableOutput("scoreboard")
      )
    )
  )
)

server <- function(input, output) {
  output$scoreboard <- DT::renderDataTable(tuples_distances, 
                                          selection = "single",  
                                          options = list(pageLength = 5))
  output$ts <- renderPlot({
    first = TRUE
    lines(original_ts[indices] ~ time(original_ts)[indices]
          ,type = "p"
          ,col = as.character(clusters))
    #lines(ref_ts, col = 3)
    # legend("bottomright", c("expected", "actual", "filtered"), col = c(1, 3, 4)
    #        ,lty = c(1, 1, 1))
    
    if (first == TRUE)
    {
      row_selected <- as.numeric(input$scoreboard_row_last_clicked)
      tuple_name <- tuples_distances[row_selected]$tuple
      tuple <- tuples_tables$tuple_name[[1]]
      filtered_dt <- get_dt_from_tuple(dt, tuple)
      #tuples_tables[[tuple_name]][[2]]
      filtered_ts <- ts(filtered_dt$ReqDuration, 
                        frequency = frequency(original_ts))
      lines(filtered_ts, col = 3)
      first = FALSE
    } else {
      row_selected <- as.numeric(input$scoreboard_row_last_clicked)
      tuple_name <- tuples_distances[row_selected]$tuple
      tuple <- tuples_tables$tuple_name[[1]]
      filtered_dt <- get_dt_from_tuple(dt, tuple)
      filtered_ts <- ts(filtered_dt$ReqDuration, 
                        frequency = frequency(original_ts))
      lines(filtered_ts, col = 3)
    }
  })
}

shinyApp(ui, server)
