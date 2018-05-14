library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

source("kbo.R")

shinyServer(
  
  function(input, output) {

    load("ngram10.rds")
    
    output$predict  <- renderText({ 
      predict(input$first, ngram$uni, ngram$bi, ngram$tri)
    })
  }
  
)
