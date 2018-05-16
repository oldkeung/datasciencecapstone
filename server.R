library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

source("kbo.R")

shinyServer(
  
  function(input, output, session) {
    
    load("ngram5.rds")
    
    observeEvent(input$submit, {
      
      withProgress(message = "Predicting Text", value = 0, {
        
        qbo_trigrams <- predict(input$first, ngram$uni, ngram$bi, ngram$tri)
        
        predictResult <- head(qbo_trigrams, 10) %>% 
          mutate(last_term = str_split_fixed(ngram, "_", 3)[, 3]) %>% 
          select(last_term, prob)
        
        output$predict <- renderText(predictResult$last_term[1])
        
        output$predictTable <- renderDataTable(
          predictResult,
          options = list(searching = FALSE, paging = FALSE)
        )
        
      })
      
    })
    
  }
  
)
