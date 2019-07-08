#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(C3)
library(googleVis)
library(radarchart)
library(gridExtra)
library(grid)


# Define UI for application that draws a histogram

ui <- navbarPage(theme = shinytheme("spacelab"),
                 "FIFA 2019 Explorer",

    # Application title
    tabPanel("Individual Stats"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("player_photo", align = 'center'),
            selectInput("select_player", label = h4("Player"),
                        choices = data$Name, selected = data[1]),
            br(),
            htmlOutput("player_general"),
            
            hr(),
                                  htmlOutput("similar_players")),

        
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(class = 'text-center',
                   
              column(10,
                     htmlOutput("name"),
                    htmlOutput("rating"), 
                    align = 'center'),
              
              column(2,
              uiOutput("club_logo"))),

          tabsetPanel(
              tabPanel("Player Stats",
                       fluidRow(
                       column(4,
                              chartJSRadarOutput("passing"), align = 'left'),
                       column(4,
                              chartJSRadarOutput("physical")),
                       column(4, 
                              chartJSRadarOutput("shooting"))),
                       
                      hr(),
                      
                       fluidRow(column(4,
                                       chartJSRadarOutput("defending")),
                                column(4,
                                       chartJSRadarOutput("dribbling")))),
              tabPanel("Player Bio")
          ))))
          



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$player_photo <- renderUI({
        
        # Display Player Photo
        
        tags$img(src = data$Photo[data$Name == input$select_player] , alt = 'photo', height = '200px', width = '200px')
        
        
    })
    
    output$player_general <- renderUI({
        
        # Display player general info
       
       country <- paste(tags$img(src = data$Flag[data$Name == input$select_player], alt = 'photo'),data$Nationality[data$Name == input$select_player], sep = " ") 
       height <- paste(tags$strong("Height:"), data$Height[data$Name == input$select_player],"cm", sep = " ")
       weight <- paste(tags$strong("Weight:"), data$Weight[data$Name == input$select_player], "lbs", sep = " ")
       age <- paste(tags$strong("Age:"), data$Age[data$Name == input$select_player], sep = " ")
       position <- paste(tags$strong("Position:"), data$Position[data$Name == input$select_player], sep = " ")
       
       HTML(paste(country, height, weight, age, position, sep = '<p/>'))
    })
    
    output$name <- renderUI({
        
        h3(tags$strong(data$Name[data$Name == input$select_player]))
    })
    
    output$club_logo <- renderUI({
        
        # Display Flag
        
        tags$img(src = data$Club.Logo[data$Name == input$select_player], alt = 'photo', height = '125px', width = '125px')
    })
    
    output$similar_players <- renderUI({
        
        sim1 <- tags$img(src = data$Photo[data$Name == closest.labels[fifa[,1] == input$select_player][2]], alt = 'photo', height = '100px', width = '100px', hspace = '25')
        sim2 <- tags$img(src = data$Photo[data$Name == closest.labels[fifa[,1] == input$select_player][3]], alt = 'photo', height = '100px', width = '100px', hspace = '35')
        sim3 <- tags$img(src = data$Photo[data$Name == closest.labels[fifa[,1] == input$select_player][4]], alt = 'photo', height = '100px', width = '100px', hspace = '25')
        
        HTML(paste(sim1, sim2, sim3, sep = ""))
    })
    
    output$rating <- renderGvis({

       gvisGauge(data.frame(Item='Overall',Value=data$Overall[data$Name == input$select_player]),options=list(min=46, max=94, greenFrom=84,
                                                                                    greenTo=100, yellowFrom=62, yellowTo=84,
                                                                                    redFrom=46, redTo=62, width=150, height=150))
     
    })
    
    output$passing <- renderChartJSRadar({
        
       
        scores <- data.frame("Label"=c("Crossing", "Long Pass", "Short Pass", "FK Accuracy", "Vision"),
                             "PASSING"= as.numeric(data[,c("Crossing", "LongPassing", "ShortPassing", "FKAccuracy", "Vision")][data$Name == input$select_player,]))
        
        
        
        chartJSRadar(scores, maxScale = 100, showToolTipLabel=TRUE, height = '250', width = '250')
        
        
        
    })
    
    output$defending <- renderChartJSRadar({
        
        
        scores2 <- data.frame("Label"=c("Marking", "StandingTackle", "SlidingTackle", "Interceptions", "Aggression"),
                              "DEFENDING" = as.numeric(data[,c("Marking", "StandingTackle", "SlidingTackle", "Interceptions", "Aggression")][data$Name == input$select_player,]))
        
        
        
        chartJSRadar(scores2, maxScale = 100, showToolTipLabel=TRUE, width = '250', colMatrix = grDevices::col2rgb("blue"))
    })
    
    output$physical <- renderChartJSRadar({
        
        scores3 <- data.frame("Label"=c("Acceleration", "Stamina", "Strength", "Sprint Speed", "Jumping"),
                              "PHYSICAL" = as.numeric(data[,c("Acceleration", "Stamina", "Strength" ,"SprintSpeed", "Jumping")][data$Name == input$select_player,]))
        
        chartJSRadar(scores3, maxScale = 100, showToolTipLabel=TRUE, colMatrix = grDevices::col2rgb("green"), width ='100')
        
    })
    
    output$shooting <- renderChartJSRadar({
        
        scores4 <- data.frame("Label"=c("Shot Power", "Finishing", "Heading", "Long Shots", "Curve", "FK Accuracy", "Penalties", "Volleys"),
                              "SHOOTING" = as.numeric(data[,c("ShotPower", "Finishing", "HeadingAccuracy" ,"LongShots","Curve", "FKAccuracy", "Penalties","Volleys")][data$Name == input$select_player,]))
        
        chartJSRadar(scores4, maxScale = 100, showToolTipLabel=TRUE, colMatrix = grDevices::col2rgb("purple"), width ='100')
    })
    
    output$dribbling <- renderChartJSRadar({
        
        scores5 <- data.frame("Label"=c("Ball Control", "Dribbling", "Composure", "Reactions", "Balance", "Agility"),
                              "DRIBBLING" = as.numeric(data[,c("BallControl", "Dribbling", "Composure" ,"Reactions","Balance", "Agility")][data$Name == input$select_player,]))
        
        chartJSRadar(scores5, maxScale = 100, showToolTipLabel=TRUE, colMatrix = grDevices::col2rgb("orange"), width ='100')
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


