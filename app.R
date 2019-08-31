# Load data ----


source("helpers.R")
dat <- readRDS("data/dictionary.rds")
library(stringr) 
library(dplyr)
library(shiny)
ui <- fluidPage(
  headerPanel("What Will They Say Next?"),
  textInput(inputId="predictor_words", label = "type in four words(comma separated)"),
  actionButton ("predictButton","predict"),          
  textOutput(outputId="predicted_word")
)

server <- function(input, output){
  
  myWords = eventReactive(input$predictButton,{
    unlist(strsplit(input$predictor_words, "\\s*,\\s*"))
  })

  output$predicted_word <- renderText({
    paste("", whatsnexta(phrase = myWords()))
  })
  
  
}
shinyApp (ui=ui, server=server)
