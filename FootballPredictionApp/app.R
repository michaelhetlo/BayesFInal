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
library(ggplot2)

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
            menuItem('First Quarter', tabName = 'FirstQuarter'),
            menuItem('Second Quarter', tabName = 'SecondQuarter'),
            menuItem('Third Quarter', tabName = 'ThirdQuarter'),
            menuItem('Plot', tabName = 'Plot'), 
            menuItem("Modeling Methodology", tabName = "methodology"),
            menuItem("NextSteps",tabName = "nextst"))),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    box(title = 'NFL Win Probability Calculator',
                        p("By: Michael Helton, Jacob Bulling and Ty Bruckner")
                        )), 
            # Second tab content
            tabItem(tabName = "background", 
                    box(title = "Motivation and Background"), 
                    p("Welcome to our NFL win probability predictor! 
                    This project was inspired by the ESPN FPI win probability calculator, they have an exhaustive model that does a very good job predicting win probability, fivethirtyeight also has a win probability model that they use to predict every game of the season and is widely considered one of the best football win probability models around."), 
                    p("To use this app, click the tab to choose which model you would like to use based on what point in the game you would like to predict from. 
                      Based on what quarter you decide to use, you can use the sliders to enter the information on what the score differential was from the home team’s perspective at the end of the first, second, or third quarter and the pregame spread line. 
                      The pregame spread is a gambling term used to reference how much the oddsmakers think one team is supposed to win or lose by. When the spread is a negative number say -6, this means that the home team is predicted to win by about 6 points.
                      The opposite is true with positive values, this means that the home team is predicted to lose by about 6 points.
                      We input the spread to have a predictor that served as a proxy variable for intrinsic team quality.
                      Once all the information is input the app will give you a win probability for the home team and a chart to show you the percentages for each team.")),
            # Third tab content
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
                                value = 0)),
            
            # Sixith tab content
            tabItem(tabName = "Plot",
                    box(title = 'Prediction After Third Quarter'),
                    box(plotOutput('dataoutput')),
                    sliderInput(inputId = "n1",
                                label = "Pregame Probability",
                                min = 0,
                                max = 1,
                                value = 0),
                    sliderInput(inputId = "n2",
                                label = "Quarter 1 Probability",
                                min = 0,
                                max = 1,
                                value = 0),
                    sliderInput(inputId = "n3",
                                label = "Quarter 2 Probability",
                                min = 0,
                                max = 1,
                                value = 0),
                    sliderInput(inputId = "n4",
                                label = "Quarter 3 Probability",
                                min = 0,
                                max = 1,
                                value = 0)), 
            
            # Seventh tab content
            tabItem(tabName = "methodology",
                    p("To start our analysis we searched for data to be able to create a play by play analysis of win probability. 
                      We found such play by play data with nflFastR package which holds play by play data for years as well as seasonal data that we thought we would be able to use as a prior. 
                      The data we ended up using was a filtered down version of the large play by play data, where each row was a score, at the end of every quarter. 
                      In our model we used a binary response that measured whether or not the home team won the game. Our predictors included the point differential at the end of the first quarter, point differential at the end of the second quarter, point differential at the end of the third quarter, and the pregame point spread."),
                    p("Our model structure was a logistic regression model on a binary variable that measured if the home team won the game.
                      The priors we used are weakly informative priors that did not provide us with very meaningful outputs.
                      Once we ran our posterior regressions we gained our model betas which provided us with an idea of the win probability based on the different scores that we had in our model."), 
                    p("Model 0 - Spread line - Every 1 point increase in the spread line increases our odds of winning by 16 percent"),
                    p("Accuracy - 67%"),
                    p("According to the website covers.com favorites win 66.5 percent of the time. In 2020, favorites won 67.1% of games. This matches our model 0."),
                    p("Pregame probability between teams of 0 spread -𝛃0 - The logged odds of winning when the game is a pick’em. 𝛃0 = -.28 which equates to an odds of .76. The odds of winning are less than 1 due to the fact that our outcome accounts for regulations wins."), 
                    p("Vegas Spreadline Variable -𝛃1 - The increase in logged odds when the spread increases by 1. 𝛃1 = .15 . Every 1 point increase in the spread line increases our odds of winning by 16 percent"), 
                    p("Model 1 - 1st Quarter"),
                    p("Q1 - Every 1 point increase in score differential at the end of the first quarter increases our odds of winning by 10 percent"),
                    p("Accuracy - 71.3%"),
                    p("End of 1st quarter probability when the score is tied between teams with 0 spread -𝛃0 - The logged odds of winning when the game is a pick’em and the score is tied after the 1st quarter. 𝛃0 = -.27 which equates to an odds of .76. The odds of winning are less than 1 due to the fact that our outcome accounts for regulations wins."),
                    p("Vegas Spreadline Variable - 𝛃1 - The increase in logged odds when the spread increases by 1. 𝛃1 = .15 . Every 1 point increase in the spread line increases our odds of winning by 16 percent."),
                    p("1st Quarter Score Differential - 𝛃2 - The increase in logged odds when the score differential increases by 1 at the end of the 1st quarter. 𝛃2  = .09. Every 1 point increase in score differential at the end of the first quarter increases our odds of winning by 10 percent."),
                    p("Model 2 - 2nd Quarter"),
                    p("Q2 - Every 1 point increase in score differential at halftime increases our odds of winning by 17 percent"),
                    p("End of 2nd quarter probability when the score is tied between teams with 0 spread -𝛃0 - The logged odds of winning when the game is a pick’em and the score is tied after the 2nd quarter. 𝛃0 = -.33 which equates to an odds of .72. The odds of winning are less than 1 due to the fact that our outcome accounts for regulations wins."),
                    p("Vegas Spreadline Variable - 𝛃1 - The increase in logged odds when the spread increases by 1. 𝛃1 = .13 . Every 1 point increase in the spread line increases our odds of winning by 14 percent."),
                    p("2nd Quarter Score Differential - 𝛃2 - The increase in logged odds when the score differential increases by 1 at the end of the 2nd quarter. 𝛃2  = .16 Every 1 point increase in score differential at the end of the second quarter increases our odds of winning by 17 percent."),
                    p("Model 3 - 3rd Quarter"),
                    p("Q3 - Every 1 point increase in score differential at the end of the 3rd quarter increases our odds of winning by 21 percent  while holding other variables constant"),
                    p("Accuracy - 85%"),
                    p("End of 3rd quarter probability when the score is tied between teams with 0 spread -𝛃0 - The logged odds of winning when the game is a pick’em and the score is tied after the 3rd quarter. 𝛃0  = -.35 which equates to an odds of .70. The odds of winning are less than 1 due to the fact that our outcome accounts for regulations wins."),
                    p("Vegas Spreadline Variable -𝛃1 - The increase in logged odds when the spread increases by 1. 𝛃1  = .11 . Every 1 point increase in the spread line increases our odds of winning by 11 percent."),
                    p("3rd Quarter Score Differential - 𝛃2 - The increase in logged odds when the score differential increases by 1 at the end of the 3rd quarter. 𝛃2  = .21. Every 1 point increase in score differential at the end of the third quarter increases our odds of winning by 23 percent."),
                    p("Thoughout our 4 models the accuracy increased. This makes sense because each model has information that occurs later in the game, so they should be more accurate."),
                    p("Each quarter the score differential exponentiated coefficient - the number that the odds increase by - is larger than previous models. For example the score differential in the 3rd quarter is more informative than the second quarter.
                      The 3rd quarter is the closest our model gets to the end of the game, so it should be the most informative.")),
        
        # Eighth tab content
        tabItem(tabName = "nextst",
                p("Our goal when starting the project was to create a play by play win probability model that would update after every play based on many different factors including the intrinsic skill of the team, the score differential and the time left in the game. 
                  This operation proved to be quite daunting given the time that we had to finish the project. 
                  We then shifted our focus into creating a win probability model after each scoring play, which for some reason while we ran that model it was far overfit to the data and when applied to outside data on our predictions it did not work at all, given the time we had remaining we decided to move on with a model that evaluates the win probability after the first, second, and third quarters."),
                p("We have a few ideas to improve on our model in the future. 
                would like to be able to incorporate stronger priors into our model. 
                We talked about using win percentage and a combination of yardage statistics as simple, but informative priors. 
                In the future we also would like to include home field advantage into our plot. 
                Home teams have historically won around 56% of the time in the NFL. This would be another informative prior.")))
    
        ))
    
server <- function(input, output) {
    output$binary_prediction <- renderTable(posterior_predict(model3rd, newdata = data.frame(score_differential.x = input$score_differential.x,
                                                                                                      score_differential.y = input$score_differential.y,
                                                                                                      score_differential.x.x = input$score_differential.x.x,
                                                                                                      spread_line.x = -(input$spread_line_3))) %>%
                                                         as.data.frame(.) %>%
                                                         summarise(Probability = mean(`1`)))
    output$pregame_prediction <- renderTable(posterior_predict(modelpre, newdata = data.frame(spread_line.x = -(input$spread_line_pre))) %>%
                                                 as.data.frame(.) %>%
                                                 summarise(Probability = mean(`1`)))
    output$firstquarter_prediction <- renderTable(posterior_predict(model1st, newdata = data.frame(spread_line.x = -(input$spread_line_1),
                                                                                                   score_differential.x = input$scoredif1.1st)) %>%
                                                      as.data.frame(.) %>%
                                                      summarise(Probability = mean(`1`)))
    output$secondquarter_prediction <- renderTable(posterior_predict(model2nd, newdata = data.frame(spread_line.x = -(input$spread_line_2),
                                                                                                   score_differential.x = input$scoredif1.2nd,
                                                                                                   score_differential.y = input$scoredif2.2nd)) %>%
                                                      as.data.frame(.) %>%
                                                      summarise(Probability = mean(`1`)))
    output$dataoutput <- renderPlot(ggplot(data = data.frame(Preds = c(input$n1, input$n2, input$n3, input$n4), names = c('Pregame', 'Quarter 1', 'Quarter 2', 'Quarter 3'), game = c('game', 'game', 'game', 'game')), aes(x = names, y = Preds, group = game)) + 
                                        geom_line() + 
                                        geom_point() + 
                                        ylim(0, 1))
}
# Run the application
shinyApp(ui = ui, server = server)

