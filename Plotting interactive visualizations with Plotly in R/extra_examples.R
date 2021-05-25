
# Using ggplotly with other plotly functions ------------------------------

# create ggplot2 object p
car_copy <- select(mtcars, -cyl)
p <- ggplot() +
  geom_point(data = car_copy, aes(x = mpg, y = disp),
             color = "lightgrey") +
  geom_point(data = mtcars, aes(x = mpg, y = disp)) +
  facet_grid(~cyl) +
  ggtitle("MPG vs Displacement by Number of Cylinders") +
  xlab("Miles per Gallon") +
  ylab("Displacement")
p

# Assign the interactive plot using `ggplotly` to
# a variable
fig <- ggplotly(p)
fig

# Now we can pipe this object to set other customizations, e.g.
fig %>% 
  layout(
    yaxis = list(range = c(100, 400))
  )
fig


# Using plotly in Shiny app -----------------------------------------------

library(shiny)

ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "This is an interative plot within a Shiny app:"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    mtcars %>% 
      plot_ly(x = ~factor(cyl), y = ~mpg, type = "scatter", name = "Scatter") %>% 
      add_boxplot(name = "Boxplot")
  })
}

shinyApp(ui = ui, server = server)
