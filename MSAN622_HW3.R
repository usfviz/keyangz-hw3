mydata <- read.csv("dataset_Facebook.csv", sep = ';')
library(shiny)
library(ggplot2)
library(plotly)
library(GGally)

ui <- fluidPage(
  headerPanel("Homework3"),
  sidebarPanel(
    sliderInput(inputId = 'Post.Month', label = 'Select Month', value = 1, min = 1, 
                max = 12, step = 1)
  ),
  mainPanel(tabsetPanel(
    tabPanel("BubblePlot", plotlyOutput("BubblePlot")),
    tabPanel("ScatterPlot Matrix", plotOutput("ScatterPlot_Matrix")),
    tabPanel("Parallel", plotlyOutput("Parallel"))
  ))
)

server <- function(input, output) {

  output$BubblePlot <- renderPlotly(

    { sliderMonth <- reactive({input$Post.Month})
      ggplot() + geom_point(data = subset(mydata, Post.Month == sliderMonth()), 
                           aes(x = Lifetime.Post.Total.Reach, y = Lifetime.Post.Total.Impressions, colour = Type, size = like)) + 
        xlim(1000, 50000) + ylim(2000, 100000) + ggtitle("Life Time Post Total Reach VS Total Impressions") + scale_size(guide = 'none')}
  )
  
  output$ScatterPlot_Matrix <- renderPlot(
    { sliderMonth <- reactive({input$Post.Month})
      my_fn <- function(data, mapping, ...){
      p <- ggplot(data = data, mapping = mapping) + 
        geom_point() +
        geom_smooth(method=lm, fill="red", color="red", ...)
      p
    }
      ggpairs(subset(mydata, Post.Month== sliderMonth()), columns = c(16:18), lower = list(continuous = my_fn))}
  )
  
  output$Parallel <- renderPlotly(
    { sliderMonth <- reactive({input$Post.Month})
      ggparcoord(data = subset(mydata, Post.Month == sliderMonth()), columns = 16:18, groupColumn = 'Type', scale = 'center')}
  )
}

shinyApp(ui = ui, server = server)

