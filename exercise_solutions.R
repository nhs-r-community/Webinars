
# Exercise solutions ------------------------------------------------------

# Answer 1 - without `add_trace`

airquality %>%
  plot_ly(x = ~Temp, y = ~Ozone, type = "scatter") %>% 
  layout(title = "Temperature vs Ozone",
         xaxis = list(title = "Temperature"),
         yaxis = list(title = "Ozone"))

# Answer 2 - with `add_trace`

airquality %>%
  plot_ly(x = ~Temp) %>%
  add_trace(y = ~Ozone, type = "scatter") %>%
  layout(title = "Temperature vs Ozone",
         xaxis = list(title = "Temperature"),
         yaxis = list(title = "Ozone"))

# shiny app

library(shiny)
ui <- fluidPage(
  textOutput("text"),
  plotlyOutput("plot")
)
server <- function(input, output){
  output$text <- renderText({
    "This is an interative plot within a Shiny app:"
  })
  output$plot <- renderPlotly({
    mtcars %>% 
      plot_ly(x = ~factor(cyl), y = ~mpg, type = "scatter", name = "Scatter") %>% 
      add_boxplot(name = "Boxplot")
  })
}
shinyApp(ui = ui, server = server)
