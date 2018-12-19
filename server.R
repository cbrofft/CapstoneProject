
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#source("global.R")

shinyServer(function(input, output) {
 

  output$value <- renderText({getWords(input$sentence)})


})
