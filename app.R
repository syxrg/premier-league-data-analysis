# Load the necessary libraries
library(shiny)
library(tidyverse)

# Load data
soccer <- read_csv("data/soccer.csv")

# Define UI
ui <- navbarPage("Soccer Data Analysis", 
                 tabPanel("Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("year", "Year:", choices = unique(soccer$Year)),
                              uiOutput("homeTeam_ui"),
                              uiOutput("awayTeam_ui")
                            ),
                            mainPanel(
                              plotOutput("barPlot"),
                              tableOutput("soccerTable")
                            )
                          )
                 ),
                 tabPanel("About", includeMarkdown("about.Rmd"))
)

# Define server logic 
server <- function(input, output, session) {
  
  output$homeTeam_ui <- renderUI({
    req(input$year)
    df <- soccer %>%
      filter(Year == input$year)
    selectInput("homeTeam", "Home Team:", choices = unique(df$HomeTeam))
  })
  
  observeEvent(input$homeTeam, {
    req(input$year, input$homeTeam)
    df <- soccer %>%
      filter(Year == input$year, HomeTeam == input$homeTeam)
    updateSelectInput(session, "awayTeam", "Away Team:", choices = unique(df$AwayTeam))
  }, ignoreNULL = TRUE)
  
  output$awayTeam_ui <- renderUI({
    selectInput("awayTeam", "Away Team:", choices = NULL)
  })
  
  filtered_data <- reactive({
    req(input$year, input$homeTeam, input$awayTeam)
    soccer %>%
      filter(Year == input$year, HomeTeam == input$homeTeam, AwayTeam == input$awayTeam)
  })
  
  output$barPlot <- renderPlot({
    df <- filtered_data()
    
    df_long <- df %>%
      select(HTHG, HTAG, FTHG, FTAG) %>%
      pivot_longer(everything(), names_to = "Type", values_to = "Goals") %>%
      mutate(Time = if_else(str_detect(Type, "HT"), "Half Time", "Full Time"),
             Team = if_else(str_detect(Type, "HG"), "Home Goals", "Away Goals"))
    
    df_long$Time <- factor(df_long$Time, levels = c("Half Time", "Full Time"))
    df_long$Team <- factor(df_long$Team, levels = c("Home Goals", "Away Goals"))
    
    ggplot(df_long, aes(x = Time, y = Goals, fill = Team)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Goals", 
           x = "", 
           title = "Goals by Home Team and Away Team",
           fill = "Team") +
      theme_minimal()
  })
  
  output$soccerTable <- renderTable({
    df <- filtered_data()
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


