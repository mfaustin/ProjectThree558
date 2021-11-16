#########################
#  Author-Mark Austin
#
#  Due Date 12/05/21
#
#  Purpose-ST 558 Project 3
#   Server portion 
#  
#########################

##Load needed libraries
library(shiny)
library(readxl)
library(tidyverse)

##Read Data
fullData<-read_excel("../data/Concrete_Data.xls")

##Define Shiny Server
shinyServer(function(input, output, session){
  
  
  
})
