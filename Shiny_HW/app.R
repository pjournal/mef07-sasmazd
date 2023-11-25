library(shiny)
library(tidyverse)

#setwd("/Users/deryasasmaz/Documents/GitHub/mef07-sasmazd/Shiny_HW")
my_data = read.csv("netflix_titles.csv")
print(my_data)

ui <- fluidPage(
  titlePanel("Netflix Veri Analizi"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Tür Seç:", choices = unique(my_data$type)),
      selectInput("country", "Ülke Seç:", choices = unique(my_data$country)),
      sliderInput("release_year", "Yıl Seç:", 
                  min = min(my_data$release_year), 
                  max = max(my_data$release_year), 
                  value = c(min(my_data$release_year), max(my_data$release_year)),
                  step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tablo", tableOutput("netflix_table")),
        tabPanel("Histogram", plotOutput("histogram"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    my_data %>%
      filter(
        type == input$type |
          is.null(input$type),
        release_year >= input$release_year[1] & release_year <= input$release_year[2],
        country == input$country |
          is.null(input$country)
      )
  })
  
  output$netflix_table <- renderTable({
    filtered_data()
  })
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = release_year)) +
      geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
      labs(title = "Histogram: Release Year Distribution")
  })
}

shinyApp(ui, server)