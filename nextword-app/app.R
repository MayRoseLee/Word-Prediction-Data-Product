# Load packages ----

library(dplyr)
library(stringr)

# Load data ----


source("helpers.R")
dat <- readRDS("data/dictionary.rds")
library(stringr) 
library(dplyr)
library(shiny)
ui <- fluidPage(headerPanel("What Will They Say Next?"),
                textInput(inputId="predictor_words", label = "type in four words"),
               textOutput(outputId="predicted_word")
)

server <- function(input, output){
  output$predicted_word <- renderText({
    paste("the next word is", whatsnexta(phrase = input$predictor_words))
  })
  
  
}
shinyApp (ui=ui, server=server)