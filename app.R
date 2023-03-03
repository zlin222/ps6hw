
library(shiny)
library(tidyverse)

cancerI <- read_delim("Cancer Incidence by Sex.csv")


ui <- fluidPage(
      titlePanel("Cancer Incidence by Sex"),
        
        
        mainPanel(
          tabsetPanel(
            tabPanel("General information",
              p("This data is from dataplanet and focuses on the number of people who have developed cancer in", strong("four"), 
                "parts of the human body from 2009 to 2018 by female and male in central geological part of the United State. 
                The four parts are ", em('breast,'),
                em('larynx,'), em('liver, '), "and", em('myeloma.'), "There are ", nrow(cancerI), "rows and",
                ncol(cancerI), "columns. For this data I will try to figure out", strong("the trend of these cancer in these years."),
                "and", strong("the maximum average number of people who get these cancers from different years range."), 
                "Hope these information can help peope who see these data have better understanding of cancer trends,
                and amount of people who suffer from that.")
              ),
            tabPanel("Plot",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("time_range", label = "Choose the time range",
                              min = min(cancerI$time),
                              max = max(cancerI$time),
                              value = c(2014, 2016)),
                  
                  selectInput("sex", label = "Select box",
                              choices = list("Female" = "Female", "Male" = "Male"),
                              selected = "Female"),
                  
                  radioButtons("plotchoice", label = "Different choice of plot", 
                               choices = list("Trend_plot", "Scartter_plot"),
                               selected = "Trend_plot"),
                  
                  ),
                mainPanel(plotOutput("cancerIplot"),
                          textOutput("linedata"))
              )),
            
            tabPanel("Table",
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput("time_range1", label = "Choose the time range",
                                     min = min(cancerI$time),
                                     max = max(cancerI$time),
                                     value = c(2011, 2016)),
                       ),
                       mainPanel(dataTableOutput("cancerITable"),
                                 textOutput("popdata"))
                     )
                     ),
        )))
    

server <- function(input, output) {

  output$cancerITable <- renderDataTable({
    cancerI %>% 
      filter(time >= input$time_range1[1],
             time <= input$time_range1[2]) %>%
      group_by(position) %>%
      summarise("Mean population" = mean(population))
  })
  
  output$popdata <- renderText({
    cancertable <- cancerI %>%
      filter(time >= input$time_range1[1],
             time <= input$time_range1[2]) %>%
      group_by(position) %>%
      summarise("Mean population" = mean(population)) %>% 
      pull("Mean population") %>%
      max() %>%
      paste("The max population from these years are brest cancer, which is ", .,
            ". For people who get the cancer among both female and male,
            breast cancer always have the maximum value among all of them. 
            And the larynx cancer is the least possible cancer people might have since it always get smallest number.
            The average population for breast cancer always around 20000 over these years,
            which is a very large number among the whole population in central geological part of the United States.")
  })
  
  
  output$cancerIplot <- renderPlot({
    
    if (input$plotchoice == "Trend_plot"){
      cancerI %>%
      filter(sex %in% input$sex) %>%
      filter(time >= input$time_range[1],
             time <= input$time_range[2]) %>%
      group_by(position) %>%
      ggplot(aes(time, population, group = position, color = factor(position))) +
      labs(x = "Time", y = "Population of cancer", color = "Different body position") +
      geom_line() +
      geom_point()
      }
    else {
    cancerI %>% 
        filter(sex %in% input$sex) %>%
        filter(time >= input$time_range[1],
               time <= input$time_range[2]) %>%
        group_by(position) %>%
        ggplot(aes(time, population, group = position, color = factor(position))) +
        labs(x = "Time", y = "Population of cancer", color = "Different body position") +
        geom_point()}
  })
  
  output$linedata <- renderText({
    cancerI %>%
      filter(sex %in% input$sex) %>%
      filter(time >= input$time_range[1],
             time <= input$time_range[2]) %>%
      pull("population") %>%
      sum() %>%
      paste("The total population from these years among these cancers are", .,
            ". Also, according to graph, we can see that female are most likely to get breast cancer.
            And for male, the breast are the lowest chance of position they might get the cancer.
            The trend of the breast cancer for women from 2009 to 2018 shows a increasing rate. And other three parts are smooth.
            For male, liver cancer and larynax cancer shows decrease start from 2013. Breast cancer trend is smooth,
            and myeloma show relatively increase trend.")
  })
  
}

 
shinyApp(ui = ui, server = server)
