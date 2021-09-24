#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("2010-2018 Arrest Data"),
  
  # Sidebar with a radio buttons for data refinement
  sidebarLayout(
    sidebarPanel(
       sliderInput("year", label="Year", 
                   min=2010,max=2018,value=2010,sep="",animate=animationOptions(interval=500,loop=TRUE)),      
       radioButtons("data",label="Data:",c("Total Arrests","White Arrests","Black Arrests")),
       radioButtons("adj",label="Data Adjustment:",c("None","% of the Population","% of Total Arrests",
                                                     "Standardized Incidence Ratio","Poisson Regression"))),
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table"))
  )))
