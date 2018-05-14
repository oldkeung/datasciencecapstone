library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

source("kbo.R")

shinyServer(
  
  function(input, output) {

    load("ngram5.rds")
    
    output$predict  <- renderText({ 
      qbo_trigrams <- predict(input$first, ngram$uni, ngram$bi, ngram$tri)
      
      output$predictTable <- qbo_trigrams
      
      qbo_trigrams$prob[1]
      
    })
  }
  
)
