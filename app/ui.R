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

##Rename Columns to be Easier to Read
fullData<-fullData %>% 
  rename(Cement=`Cement (component 1)(kg in a m^3 mixture)`,
   Blast_Furnace_Slag=`Blast Furnace Slag (component 2)(kg in a m^3 mixture)`,
   Fly_Ash= `Fly Ash (component 3)(kg in a m^3 mixture)`,
   Water= `Water  (component 4)(kg in a m^3 mixture)`,
   Superplasticizer=`Superplasticizer (component 5)(kg in a m^3 mixture)`,
   Coarse_Aggregate=`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`,
   Fine_Aggregate=`Fine Aggregate (component 7)(kg in a m^3 mixture)`,
   Age= `Age (day)`,
   Concrete_Compressive_Strength=`Concrete compressive strength(MPa, megapascals)`
  )


##Define UI using NavbarPage layout

shinyUI(navbarPage("Project 3",
  
  ##Enable shiny theme
  theme = shinytheme("cerulean"),
  
  ##Define Project Pages with tabPanel()
  
  ##About Page Section
  tabPanel("About",
    ##Using fluid page for needed About elements       
    fluidPage(
      h2("App's Purpose"),
      p("some sample text very long foo long foo long foo long"),
      h2("Project Data"),
      p("describe data"),
      h2("App Pages"),
      p("describe each page"),
      p("add an image")
    )
  ),
  
  ##Data Page Section
  tabPanel("Data",
    fluidPage(
     sidebarLayout(
      sidebarPanel("sidebar panel",
        downloadButton("downloadData", "Download Data"),           
        checkboxInput("subdata", h4("Subset Data", style = "color:green;")),          conditionalPanel("input.subdata", 
              checkboxGroupInput("dtcolumns", h4("Select Columns"),
                      choices = colnames(fullData),
                      selected = colnames(fullData)),
              h4("Filter Rows by Using Slider Values"),
              sliderInput("CementSlide","Cement Range",
                    min = min(fullData$Cement),
                    max = max(fullData$Cement),
                    value = c(min(fullData$Cement),max(fullData$Cement))),
              sliderInput("BlastSlide","Blast_Furnace_Slag Range",
                          min = min(fullData$Blast_Furnace_Slag),
                          max = max(fullData$Blast_Furnace_Slag),
                          value = c(min(fullData$Blast_Furnace_Slag),
                                    max(fullData$Blast_Furnace_Slag))),
              sliderInput("FlySlide","Fly_Ash Range",
                          min = min(fullData$Fly_Ash),
                          max = max(fullData$Fly_Ash),
                          value = c(min(fullData$Fly_Ash),
                                    max(fullData$Fly_Ash))),
              sliderInput("WaterSlide","Water Range",
                          min = min(fullData$Water),
                          max = max(fullData$Water),
                          value = c(min(fullData$Water),
                                    max(fullData$Water))),
              sliderInput("SuperSlide","Superplasticizer Range",
                          min = min(fullData$Superplasticizer),
                          max = max(fullData$Superplasticizer),
                          value = c(min(fullData$Superplasticizer),
                                    max(fullData$Superplasticizer))),
              sliderInput("CoarseSlide","Coarse_Aggregate Range",
                          min = min(fullData$Coarse_Aggregate),
                          max = max(fullData$Coarse_Aggregate),
                          value = c(min(fullData$Coarse_Aggregate),
                                    max(fullData$Coarse_Aggregate))),
              sliderInput("FineSlide","Fine_Aggregate Range",
                          min = min(fullData$Fine_Aggregate),
                          max = max(fullData$Fine_Aggregate),
                          value = c(min(fullData$Fine_Aggregate),
                                    max(fullData$Fine_Aggregate))),
              sliderInput("AgeSlide","Age Range",
                          min = min(fullData$Age),
                          max = max(fullData$Age),
                          value = c(min(fullData$Age),
                                    max(fullData$Age))),
          sliderInput("ConcreteSlide","Concrete_Compressive_Strength Range",
                      min = min(fullData$Concrete_Compressive_Strength),
                      max = max(fullData$Concrete_Compressive_Strength),
                      value = c(min(fullData$Concrete_Compressive_Strength),
                            max(fullData$Concrete_Compressive_Strength)))
              )
      ),
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


