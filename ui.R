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
      textInput("first", h3("Text input"), value = "Enter text..."),
      submitButton(text = "Predict"),
      textOutput("predict")
    )
    
  ),
  tabPanel("Help", fluidRow()
           #includeMarkdown("trees_manual.Rmd")
  )
  
))
