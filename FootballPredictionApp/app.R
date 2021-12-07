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

body <-dashboardBody(
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                h2("We should put an introduction section here and explain our methodology and shit.")
        ),
        
        
        # Second tab content
        tabItem(tabName = "prior",
                h2('Priors Soon')
        ),
        
        tabItem(tabName = "first", 
                h2("Model coming soon")
        ), 
        
        tabItem(tabName = "second", 
                h2("Hopefully this works")
        ), 
        tabItem(tabName = "third", 
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
                                value = 0)
                )
        )
    )
)

ui <-dashboardPage(header, sidebar, body)
    

server <- function(input, output) {
    output$binary_prediction <- renderTable(posterior_predict(model, newdata = data.frame(score_differential.x = input$score_differential.x,
                                                                                          score_differential.y = input$score_differential.y,
                                                                                          score_differential.x.x = input$score_differential.x.x)) %>%
                                                as.data.frame(.) %>%
                                               summarise(Prob = mean(`1`)))
}
# Run the application
shinyApp(ui = ui, server = server)

