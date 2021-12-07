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

model <- readRDS("quarter_model.rds")


server <- function(input, output) {
    output$binary_prediction <- renderTable(posterior_predict(model, newdata = data.frame(score_differential.x = input$score_differential.x,
                                                                                          score_differential.y = input$score_differential.y,
                                                                                          score_differential.x.x = input$score_differential.x.x)) %>%
                                                as.data.frame(.) %>%
                                                summarise(Prob = mean(`1`)))
    }

ui <- dashboardPage(
    dashboardHeader(title = "Win Probability Predictions"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("dashboard" , tabName = "dashboard"),
            menuItem("Start of Game", tabName = "Start of Game", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    box(tableOutput('binary_prediction')),
                    box(title = 'inputs',
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
                                                 value = 0),
                                 )),
            
            # Second tab content
            tabItem(tabName = "Start of Game",
                    box(title = 'Priors Soon'))
    ))
)


# Run the application
shinyApp(ui = ui, server = server)

