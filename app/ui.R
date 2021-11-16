#########################
#  Author-Mark Austin
#
#  Due Date 12/05/21
#
#  Purpose-ST 558 Project 3
#   UI portion 
#  
#########################


##Load needed libraries
library(shiny)


##Define UI using NavbarPage layout

shinyUI(navbarPage("Project 3",

  tabPanel("About"),
  tabPanel("Data"),
  tabPanel("Data Exploration"),
  tabPanel("Modeling")
                   
                                      
))                   


