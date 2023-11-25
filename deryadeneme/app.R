pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}
​
##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)
​
# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
  movies %>% 
  filter(year >= 2000) %>%
  select(title,year,length,rating,votes,Action:Short) %>% 
  gather(genre,value,Action:Short) %>% 
  filter(value == 1) %>% 
  select(-value)
​
# Get genre list
genres <- 
  shiny_movie_set %>% 
  distinct(genre) %>% 
  unlist(.)
​
names(genres) <- NULL
​
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("year","Year Filter",min=min(shiny_movie_set$year),max=max(shiny_movie_set$year),value=c(2002,2004),step=1,sep=""),
      selectInput("genre","Genre",choices = c("All",genres),selected="All",multiple = TRUE),
      sliderInput("votes","At least X votes",min=0,max=max(shiny_movie_set$votes),value=0)
    ),
    mainPanel(plotOutput("movie_plot"))
  )
)
​
server <- function(input,output){
  
  output$movie_plot <- renderPlot({
    my_df <- shiny_movie_set %>% 
      filter(year >= input$year[1] & year <= input$year[2] & votes >=input$votes) 
    print(input$genre)
    if (!("All" %in% input$genre)){
      my_df <- my_df %>% 
        filter(genre %in% input$genre)
      
    }
    
    ggplot(my_df,aes(x=length,y=rating,color=genre)) + 
      geom_point() + 
      labs(title="IMDB Movies")
  })
}
​
shiny::shinyApp(ui,server)