
library(shiny)
library(tidyverse)
library(DT)
library(leaflet)

load("ae_attendances.RData")

ui <- fluidPage(
    
    # Application title
    titlePanel("A and E data"),
    
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("date", "Date range", 
                           as.Date("2016-04-01"), 
                           as.Date("2019-03-01"),
                           startview = "year"),
            conditionalPanel(
                condition = "input.tabset == 'graph'",
                uiOutput("trustControl")
            )),
        
        mainPanel(
            tabsetPanel(id = "tabset",
                        tabPanel("Graph", value = "graph", plotOutput("graph")),
                        tabPanel("Map", value = "map", fluidRow(
                            column(width = 6,
                                   leafletOutput("trustMap", height = 600, width = 500)
                            ),
                            column(width = 6, 
                                   plotOutput("oneGraph", hover = "plot_hover"),
                                   textOutput("hoverDetails")
                            )
                        ))
            )
        )
    )
)

server <- function(input, output) {
    
    filter_data <- reactive({
        
        sample_trusts <- ae_attendances %>% 
            filter(period >= input$date[1], period <= input$date[2]) %>% 
            arrange(Name)
    })
    
    output$trustControl <- renderUI({
        
        selectInput("trust",
                    "Select Trust",
                    choices = unique(filter_data()$Name),
                    multiple = TRUE)
    })
    
    output$trustMap <- renderLeaflet({
        
        filter_data() %>%
            leaflet() %>%
            addTiles() %>%
            setView(lng = -2, lat = 53, zoom = 7) %>%
            addCircleMarkers(lng = ~ lon, lat = ~ lat, weight = 1,
                             radius = ~ attendances/ 1000,
                             layerId = ~org_code)
    })
    
    observeEvent(input$trustMap_marker_click, { 
        
        loc <- input$trustMap_marker_click
        
        trust_id <- input$trustMap_marker_click$id
        
        trust_details <- filter_data() %>% 
            filter(org_code == trust_id) %>% 
            arrange(desc(period)) %>% 
            slice(1)        
        
        leafletProxy("trustMap") %>% 
            addPopups(loc$lng, loc$lat, 
                      paste0(trust_details$Name, "<br>",
                             "Attendances: ", trust_details$attendances, "<br>",
                             "Breaches: ", trust_details$breaches)
            )
    })
    
    output$graph <- renderPlot({
        
        validate(need(input$trust, "Please select a Trust"))
        
        graph_data <- filter_data() %>% 
            filter(Name %in% input$trust)
        
        graph_data %>% 
            group_by(period, Name) %>% 
            summarise(mean_attendance = mean(attendances)) %>% 
            ggplot(aes(x = period, y = mean_attendance)) +
            geom_line() + facet_wrap(~ Name, scales = "free")
    })
    
    output$oneGraph <- renderPlot({
        
        req(input$trustMap_marker_click)
        
        trust_id <- input$trustMap_marker_click$id
        
        oneTrust <- filter_data() %>% 
            filter(org_code == trust_id)
        
        oneTrust %>% 
            group_by(period) %>% 
            summarise(mean_attendance = mean(attendances)) %>% 
            ggplot(aes(x = period, y = mean_attendance)) +
            geom_line()
    })
    
    output$hoverDetails <- renderText({
        
        req(input$trustMap_marker_click)
        
        trust_id <- input$trustMap_marker_click$id
        
        oneTrust <- filter_data() %>% 
            filter(org_code == trust_id) %>% 
            group_by(period) %>% 
            summarise(mean_attendance = mean(attendances))
        
        attendance <- nearPoints(oneTrust, input$plot_hover, 
                                 "period", "mean_attendance", 
                                 threshold = 10) %>% 
            slice(1) %>% 
            pull(mean_attendance)
        
        if(is.null(input$plot_hover)){
            
            "Please hover over a point for more information"
        } else {
            
            paste0("Attendances: ", round(attendance, 0))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
