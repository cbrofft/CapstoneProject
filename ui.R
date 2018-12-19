
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Word Prediction Prototype 1.0"),


    # Show a plot of the generated distribution
    mainPanel(
      # Copy the line below to make a text input box
      textInput("sentence", label = h4("Type a partial sentence:"), value = ""),
      submitButton("Submit"),
      
      hr(),

      fluidRow(column(3, h4("Prediction:"),verbatimTextOutput("value")))
    )
  
))
