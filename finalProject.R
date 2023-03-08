library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
happiness <- read_delim("worldhappinessreport2021.csv.xls")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("World happiness in 2021"),
  tabsetPanel(
    tabPanel("About",
             img(alt = "Report Image",
                 src = "world happiness.png", height = '400px', width = '700px'),
             h2("Project Overview"),
             p("The world happiness report provides a brife summary of how does different ",
               "factors affect a country's happiness level in 2021. With the results, the report will ",
               "clearly show the relationship between happiness and logged GDP per capita ",
               ", the relationship between happiness and healthy life expectancy and ",
               "the average world happiness in 2021. We hope the report could help reader have ",
               "a more comprehensive understanding towards world happiness ",
               "and factors that influencing world happiness. Enjoy the report!"),
             h2("Data Source"),
             p("We will be working with the world happiness report data set made by the ",
               "Sustainable Development Solutions Network published on kaggle."),
             h2("Questions we are working with?"),
             p("Comparing internationally?"),
             p("What's the relationship between happiness and logged GDP per capita?"),
             p("What's the relationship between happiness and healthy life expectancy?")
    ),
    
    tabPanel("Comparing internationally",
             sidebarLayout(
               sidebarPanel(
                 # select the region
                 checkboxGroupInput("regional", "Select the region:", 
                              choices = unique(happiness$`Regional indicator`),
                              selected = "Western Europe")
               ),
               mainPanel(
                 verbatimTextOutput("text3"),
                 # the scatter plot output
                 plotlyOutput("international"))
             )),
    
    
    tabPanel("Happiness and logged GDP Per Capita", 
             sidebarLayout(
               sidebarPanel(
                 # select the region
                 selectInput("region", "Select the region:", 
                             choices = unique(happiness$`Regional indicator`),
                             selected = "Western Europe"),
                 # add a trend line or not
                 checkboxInput("trend", "Trend Line")
                 
               ),
                 mainPanel(
                   # the scatter plot output
                 plotlyOutput("p2"),
                 verbatimTextOutput("text1"))
             )),
  
    tabPanel("Happiness and Healthy life expectancy",
        sidebarLayout(
          sidebarPanel(
            # select the  region
            selectInput("region2", "Select the region:", 
                        choices = unique(happiness$`Regional indicator`), 
                        selected = "Western Europe")

          ),
          mainPanel(
            tableOutput("table"),  # table output
            verbatimTextOutput("text"),  # render dynamic text
            h4("Region with the highest average happiness score"),
            tableOutput("table1"),  # table output
            h4("Region with the lowest average happiness score"),
            tableOutput("table2"),  # table output
            verbatimTextOutput("text2")
          )
        )     
    ),
  
    tabPanel("Conclusion",
             checkboxInput("trend2", "Add Trend Line"),
             plotlyOutput("lastP"),
             p("---According to the chart above and our data, ninety percent of ",
               "regions clearly show that logged GDP per capita indeed have a positive relationship ",
               "with happiness score. The rest of ten percent, however, have a negative relationship with ",
               "happiness score. The reason behind it still need further research and analysis. ",
               "Because of this, we should also realize that there is no single factor that ",
               "could have same influence to everyone's even region's happiness score!"),
             p("---On the one side, I believe my dataset of reasonable quality since it uses ",
               "ladder score and logged GDP per capita instead of just happiness score and ",
               "GDP per capita. Comparing to normal happiness score and GDP per capita, ",
               "ladder score and logged GDP per capita are more reasonable and clearer. ",
               "On the other side, the way that the questions are phrased on the poll makes ",
               "data show a cultural bias as it can emphasize western preferences and values. ",
               "This explains a bit about why does certain countries have a lower happiness ",
               "score than they expected."),
             p("---With additional research and analysis, the project could be improved by finding ",
               "the reason behind certain region's negative relationship between ladder score and ",
               "logged GDP per capita, which will make this report become more comprehensive."))
  ),
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$p2 <- renderPlotly({
    # draw a scatter plot
    p <- happiness %>%
      filter(`Regional indicator`==input$region) %>%
      ggplot(aes(x=`Logged GDP per capita`, y=`Ladder score`)) + geom_point()
    if(input$trend) {
      # add a trend line
      p <- p + geom_smooth(method="lm", se=F)
    }
    p
  })
  
  output$text1 <- renderText({
    paste0("The second One is scatter charts.
From this scatter plot we can clearly see the correlation between ladder score and Logged GDP per capita.
If Trend line is an upward trend, then the two variables are positively correlated.
It proves that the higher the GDP is, the higher the Ladder score is.
If the Trend Line is trending down, then the two variables are negatively correlated.
It indicates that the higher the GDP, the lower the Ladder Score.")
  })
  
  output$text3 <- renderText({
    paste0("This chart could help user have a better understanding of different region's
ladder score. User could select their interested region to see or compare.")
  })
  
  output$international <- renderPlotly({
    inter <- happiness %>%
      filter(`Regional indicator` %in% input$regional) %>%
      group_by(`Regional indicator`) %>%
      summarise(mean=mean(`Ladder score`)) %>%
      ggplot(aes(x=`Regional indicator`, y=`mean`,fill=`Regional indicator`)) + geom_col()
    inter
  })
  
  output$lastP <- renderPlotly({
    p3 <- happiness %>%
      group_by(`Regional indicator`) %>%
      ggplot(aes(x=`Logged GDP per capita`, y=`Ladder score`, col=`Regional indicator`)) +
      geom_point()
    if(input$trend2) {
      # add a trend line
      p3 <- p3 + geom_smooth(method="lm", se=F)
    }
    ggplotly(p3)  # convert to plotly object
  })
  
  output$table <- renderTable({
    # wide to long
    happiness %>%
      filter(`Regional indicator`==input$region2) %>%
      select(`Country name`, `Ladder score`, `Healthy life expectancy`) %>%
      gather(key=Metric, value=value, -`Country name`) %>%
      # calcualte the statistic 
      group_by(Metric) %>%
      summarise(mean=mean(value),
                sd=sd(value), min=min(value), max=max(value), median=median(value)) %>%
      mutate(Metric=ifelse(Metric=="Ladder score", "Ladder score(unit)", "Healthy life expectancy/year"))
  })
  
  output$table1 <- renderTable({
    region <- happiness %>%
      group_by(`Regional indicator`) %>%
      summarise(value=mean(`Ladder score`)) %>%
      arrange(desc(value)) %>%
      head(1) %>%
      pull(`Regional indicator`)
    happiness %>%
      filter(`Regional indicator`==region) %>%
      select(`Regional indicator`,`Country name`, `Ladder score`, `Healthy life expectancy`)
  })
  
  output$table2 <- renderTable({
    region <- happiness %>%
      group_by(`Regional indicator`) %>%
      summarise(value=mean(`Ladder score`)) %>%
      arrange(value) %>%
      head(1) %>%
      pull(`Regional indicator`)
    happiness %>%
      filter(`Regional indicator`==region) %>%
      select(`Regional indicator`, `Country name`, `Ladder score`, `Healthy life expectancy`)
  })
  
  output$text <- renderText({
    # filter data
    df <- happiness %>%
      filter(`Regional indicator`==input$region) 
    # calculate the correlation coefficient
    paste0("The selected region is ", input$region, 
           "\nThe correlation coefficient between happiness and Healthy life expectancy is ", 
           round(cor(df$`Ladder score`, df$`Healthy life expectancy`), 2))
  })
  
  output$text2 <- renderText({
    paste0("The third picture is a table. There are two tables that are fixed.
They record the countries with the highest and the lowest Ladder Score, respectively.
Users can change the region according to their needs by using the option bar on the right.
Thus, the situation in different regions can be viewed.
The chart includes the average, standard deviation, minimum, maximum and median of Healthy life expectancy and Ladder score.")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
