library(tidyverse)
library(shiny)
library(ggplot2)

data_football <- read_csv('data/football.csv')
ui <- navbarPage(
  title = "football",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "The Football History of Illinois Team"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "wl",
          label = "W/L",
          choices = unique(data_football$Result),
          selected = "L"),
      
        selectInput(
          inputId = "football_season", 
          label = "Year:", 
          choices = sort(unique(data_football$Season)), 
          selected = 2002),
        selectInput(
          inputId = "loc", 
          label = "home/away game:", 
          choices = unique(data_football$Location),
          selected = "@")
        
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  tabPanel(title = 'Table', dataTableOutput('table')),
  tabPanel(title = "About", includeMarkdown("About.Rmd"))
)
server <- function(input, output) {
  rct = reactive({
    data_football %>% 
      filter(Season == input$football_season)
  })
  observeEvent(
    eventExpr = input$football_season,
    handlerExpr = {
      updateSelectInput(inputId = "loc",
                        choices = unique(rct()$Location),
                        selected = "@")
    }
  )
  output$plot <- renderPlot({
    data_football %>% 
      filter(Result == input$wl) %>% 
      filter(Season == input$football_season) %>% 
      filter(Location == input$loc) %>% 
      select(Opponent, OpponentScore) %>% 
      ggplot() +
      aes(x = Opponent, y = OpponentScore, fill='#A4A4A4', color="darkred") %>%
      geom_bar(stat = "identity") +
      theme_bw() +
      ggtitle("Opponent Score") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$table <- renderDataTable({
    data_football %>% 
      filter(Season == input$football_season) %>% 
      filter(Location == input$loc) %>% 
      filter(Result == input$wl) %>% 
      select(Opponent, OpponentScore)
  })
}
shinyApp(ui = ui, server = server)
