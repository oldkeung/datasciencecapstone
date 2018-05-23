library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

source("kbo_alt.R")

shinyServer(
  
  function(input, output, session) {
    
    #load("ngram10.rds")
    loadData()
    
    observeEvent(input$submit, {
      
      withProgress(message = "Predicting Text", value = 0, {
        
        #qbo_trigrams <- predict(input$first, ngram$uni, ngram$bi, ngram$tri)
        qbo_trigrams <- predict(input$first)
        
        #incProgress(1/n, detail = paste("Doing part", i))
        
        qbo_trigrams$ID <- seq.int(nrow(qbo_trigrams))
        predictResult <- qbo_trigrams %>%
          select(ID, Word = predict, Probability = prob)

        output$predict <- renderText(predictResult$predict[1])
        
        output$predictTable01to10 <- renderDataTable(
          predictResult[1:10,],
          options = list(
            searching = FALSE, 
            paging = FALSE,
            pageLength = -1)
        )
        
      })
      
    })
    
  }
  
)
