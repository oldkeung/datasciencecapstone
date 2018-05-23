library(shiny)
library(shinythemes)
library(plotly)
library(colourpicker)

shinyUI(navbarPage(
  
  "Text Prediction",
  theme = shinytheme("united"),
  
  tabPanel(
    "Home",
    
    fluidRow(
      
      column(8,
             textInput("first", "Text input", placeholder = "Enter text to predict..."),
             actionButton("submit", "Predict")
      ),
      
      column(4,
             textOutput("predict")
      )
      
    ),
    
    fluidRow(
      hr()
    ),
    
    fluidRow(
      column(4, dataTableOutput("predictTable01to10")),
      column(4, dataTableOutput("predictTable11to20")),
      column(4, dataTableOutput("predictTable21to30"))
    )
    
  ),
  tabPanel("Help", fluidRow()
           #includeMarkdown("trees_manual.Rmd")
  )
  
))
