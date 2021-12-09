# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(tidybayes)
library(forcats)
library(bayesplot)
library(lubridate)
library(nflfastR)
library(shinydashboard)

model3rd <- readRDS("quarter_model.rds")


header<-dashboardHeader(title = "Win Probability Predictions")
sidebar <-dashboardSidebar(
    sidebarMenu(
        menuItem("dashboard" , tabName = "dashboard"),
        menuItem("Beginning of Game", tabName = "prior", icon = icon("th")),
        menuItem("End of First Quarter", tabName = "first"), 
        menuItem("End of Second Quarter", tabName = "second"), 
        menuItem("End of Third Quarter", tabName = "third")
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "Win Probability Predictions"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("dashboard" , tabName = "dashboard"),
            menuItem("Predictions", tabName = "StartofGame", icon = icon("th")),
            menuItem('FirstQuarter', tabName = 'FirstQuarter'),
            menuItem('SecondQuarter', tabName = 'SecondQuarter'),
            menuItem('ThirdQuarter', tabName = 'ThirdQuarter'))),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    box(title = 'Welcome Page')),
            # Second tab content
            tabItem(tabName = "StartofGame",
                    box(title = 'Prediction Start of Game')),
            # Third tab content
            tabItem(tabName = "FirstQuarter",
                    box(title = 'Prediction After First Quarter'),
                    sliderInput(inputId = "",
                                label = "Score Differential 1st",
                                min = -30,
                                max = 30,
                                value = 0)),
            # Fourth tab content
            tabItem(tabName = "SecondQuarter",
                    box(title = 'Prediction After Second Quarter'),
                    sliderInput(inputId = "",
                                label = "Score Differential 1st",
                                min = -30,
                                max = 30,
                                value = 0),
                    sliderInput(inputId = "",
                                label = "Score Differential 2nd",
                                min = -30,
                                max = 30,
                                value = 0)),
            # Fifth tab content
            tabItem(tabName = "ThirdQuarter",
                    box(title = 'Prediction After Third Quarter'),
                    box(tableOutput('binary_prediction')),
                    sliderInput(inputId = "score_differential.x",
                                label = "Score Differential 1st",
                                min = -30,
                                max = 30,
                                value = 0),
                    sliderInput(inputId = "score_differential.y",
                                label = "Score Differential 2nd",
                                min = -30,
                                max = 30,
                                value = 0),
                    sliderInput(inputId = "score_differential.x.x",
                                label = "Score Differential 3rd",
                                min = -30,
                                max = 30,
                                value = 0))
    )))
    
server <- function(input, output) {
    output$binary_prediction <- renderTable(posterior_predict(model3rd, newdata = data.frame(score_differential.x = input$score_differential.x,
                                                                                          score_differential.y = input$score_differential.y,
                                                                                          score_differential.x.x = input$score_differential.x.x)) %>%
                                                as.data.frame(.) %>%
                                               summarise(Prob = mean(`1`)))
}
# Run the application
shinyApp(ui = ui, server = server)

