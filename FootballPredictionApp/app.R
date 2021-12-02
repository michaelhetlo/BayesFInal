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

model <- readRDS("quarter_model.rds")

server <- function(input, output) {
    # reactive(
    # output$binary_prediction <- posterior_predict(
    #     model,
    #     newdata = data.frame(score_differential.x = input$score_differential.x, 
    #                          score_differential.y = input$score_differential.y, 
    #                          score_differential.x.x = input$score_differential.x.x)) %>%
    #     as.data.frame(.) %>%
    #     group_by(`1`))
    }

ui <- fluidPage(
    
    # Application title
    titlePanel("Quarter Model"),
    
    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "score_differential.x", 
                        label = "Score Differential 1st",
                        min = 0, 
                        max = 30, 
                        value = 15),
            sliderInput(inputId = "score_differential.y", 
                        label = "Score Differential 2nd",
                        min = 0, 
                        max = 30, 
                        value = 15),
            sliderInput(inputId = "score_differential.x.x", 
                        label = "Score Differential 3rd",
                        min = 0, 
                        max = 30, 
                        value = 15),
        ),
        
        
        # Show output
        mainPanel(
            dataTableOutput("binary_prediction")
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)
