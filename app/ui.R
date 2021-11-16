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
library(readxl)
library(tidyverse)

##Read Data
fullData<-read_excel("../data/Concrete_Data.xls")



##Define UI using NavbarPage layout

shinyUI(navbarPage("Project 3",
  
  ##Enable shiny theme
  theme = shinytheme("cerulean"),
  
  ##Define Project Pages with tabPanel()
  tabPanel("About"),
  tabPanel("Data"),
  tabPanel("Data Exploration"),
  tabPanel("Modeling")
                   
                                      
))                   


