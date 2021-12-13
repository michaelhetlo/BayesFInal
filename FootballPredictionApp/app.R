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

modelpre <- readRDS('pregame_model.rds')
model1st <- readRDS('first_quarter.rds')
model2nd <- readRDS('second_quarter.rds')
model3rd <- readRDS("third_quarter.rds")

ui <- dashboardPage(
    dashboardHeader(title = "Win Probability Predictions"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Welcome Page" , tabName = "dashboard"),
            menuItem("Motivation and Background", tabName = "background"),
            menuItem("Start of Game", tabName = "StartofGame"),
            menuItem('FirstQuarter', tabName = 'FirstQuarter'),
            menuItem('SecondQuarter', tabName = 'SecondQuarter'),
            menuItem('ThirdQuarter', tabName = 'ThirdQuarter'))),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    box(title = 'NFL Win Probability Calculator',
                        p("By: Micheal Helton, Jacob Bulling and Ty Bruckner")
                        )), 
            # Second tab content
            tabItem(tabName = "StartofGame",
                    box(title = 'Prediction Start of Game'),
                    box(tableOutput('pregame_prediction')),
                    sliderInput(inputId = "spread_line_pre",
                                label = "Home Team Spread",
                                min = -21,
                                max = 21,
                                value = 0)),
            # Third tab content
            tabItem(tabName = "FirstQuarter",
                    box(title = 'Prediction After First Quarter'),
                    box(tableOutput('firstquarter_prediction')),
                    sliderInput(inputId = "spread_line_1",
                                label = "Home Team Spread",
                                min = -21,
                                max = 21,
                                value = 0),
                    sliderInput(inputId = "scoredif1.1st",
                                label = "Score Differential 1st",
                                min = -30,
                                max = 30,
                                value = 0)),
            # Fourth tab content
            tabItem(tabName = "SecondQuarter",
                    box(title = 'Prediction After Second Quarter'),
                    box(tableOutput('secondquarter_prediction')),
                    sliderInput(inputId = "spread_line_2",
                                label = "Home Team Spread",
                                min = -21,
                                max = 21,
                                value = 0),
                    sliderInput(inputId = "scoredif1.2nd",
                                label = "Score Differential 1st",
                                min = -30,
                                max = 30,
                                value = 0),
                    sliderInput(inputId = "scoredif2.2nd",
                                label = "Score Differential 2nd",
                                min = -30,
                                max = 30,
                                value = 0)),
            # Fifth tab content
            tabItem(tabName = "ThirdQuarter",
                    box(title = 'Prediction After Third Quarter'),
                    box(tableOutput('binary_prediction')),
                    sliderInput(inputId = "spread_line_3",
                                label = "Home Team Spread",
                                min = -21,
                                max = 21,
                                value = 0),
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
                                                                                          score_differential.x.x = input$score_differential.x.x,
                                                                                          spread_line.x = -(input$spread_line_3))) %>%
                                                as.data.frame(.) %>%
                                               summarise(Prob = mean(`1`)))
    output$pregame_prediction <- renderTable(posterior_predict(modelpre, newdata = data.frame(spread_line.x = -(input$spread_line_pre))) %>%
                                                 as.data.frame(.) %>%
                                                 summarise(Prob = mean(`1`)))
    output$firstquarter_prediction <- renderTable(posterior_predict(model1st, newdata = data.frame(spread_line.x = -(input$spread_line_1),
                                                                                                   score_differential.x = input$scoredif1.1st)) %>%
                                                      as.data.frame(.) %>%
                                                      summarise(Prob = mean(`1`)))
    output$secondquarter_prediction <- renderTable(posterior_predict(model1st, newdata = data.frame(spread_line.x = -(input$spread_line_2),
                                                                                                   score_differential.x = input$scoredif1.2nd,
                                                                                                   score_differential.y = input$scoredif2.2nd)) %>%
                                                      as.data.frame(.) %>%
                                                      summarise(Prob = mean(`1`)))
}
# Run the application
shinyApp(ui = ui, server = server)

