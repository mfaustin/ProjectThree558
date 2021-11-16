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
library(shinythemes)
library(readxl)
library(DT)
library(tidyverse)

##Read Data
fullData<-read_excel("../data/Concrete_Data.xls")



##Define UI using NavbarPage layout

shinyUI(navbarPage("Project 3",
  
  ##Enable shiny theme
  theme = shinytheme("cerulean"),
  
  ##Define Project Pages with tabPanel()
  
  ##About Page Section
  tabPanel("About"),
  
  ##Data Page Section
  tabPanel("Data",
    fluidPage(
     sidebarLayout(
      sidebarPanel("sidebar panel"),
      mainPanel("main panel",
                dataTableOutput("tableResults"))
     )  
    )           
           ),
  
  ##Data Exploration Page Section
  tabPanel("Data Exploration"),
  
  ##Modeling Page Section
  tabPanel("Modeling",
   mainPanel(
      tabsetPanel(
        tabPanel("Modeling Info"),
        tabPanel("Model Fitting"),
        tabPanel("Prediction")
      )
   )           
  )
                   
                                      
))                   


