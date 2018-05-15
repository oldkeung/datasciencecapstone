library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

source("kbo.R")

shinyServer(
  
  function(input, output) {
    
    load("ngram5.rds")
    init <- TRUE
    
    output$predict  <- renderText({
      
      if (!init)
      {
        
        qbo_trigrams <- predict(input$first, ngram$uni, ngram$bi, ngram$tri)
        
        a <- head(qbo_trigrams, 10) %>% mutate(last_term = str_split(ngram, "_"))[3] %>% select(last_term, prob)
        
        output$predictTable <- renderDataTable(
          a,
          options = list(searching = FALSE, paging = FALSE)
          )
    
        qbo_trigrams$prob[1]
        
      }
      
    })
    
    init <- FALSE
  }
  
)
