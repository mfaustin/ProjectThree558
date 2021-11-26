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
  
######About Page Section###########################################
  tabPanel("About",
    ##Using fluid page for needed About elements       
    fluidPage(
      h2("Purpose"),
      h4("This App allows the user to interactively explore Concrete Compressive Strength data both as raw data and in select statistical models.  ",a(href="https://en.wikipedia.org/wiki/Compressive_strength","Compressive strength"), "measures how much load can be handled by an object being pushed together."),
      h2("Project Data"),
      h4("This project explores the", a(href="https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength", "Concrete Compressive Strength"),"  data set available in the UCI machine learning repository.  The data set was originally published in article I-Cheng Yeh, \"Modeling of strength of high performance concrete using artificial neural networks,\" Cement and Concrete Research, Vol. 28, No. 12, pp. 1797-1808 (1998)."),
      h4("The data are all quantitative.  This data set presents a regression problem with concrete compressive strength as the response variable.  Higher strength values are desirable in this scenario."),
      h4("The data set has 8 quantitative predictor variables, 1 quantitative response variable, and 1030 observations.  A variable list with variable name and units follows:"),
      tags$ul(
       tags$li(code("Cement"),"kg in a cubic meter mixture"),
       tags$li(code("Blast_Furnace_Slag"),"kg in a cubic meter mixture"),
       tags$li(code("Fly_Ash"),"kg in a cubic meter mixture"),
       tags$li(code("Water"),"kg in a cubic meter mixture"),
       tags$li(code("Superplasticizer"),"kg in a cubic meter mixture"),
       tags$li(code("Coarse_Aggregate"),"kg in a cubic meter mixture"),
       tags$li(code("Fine_Aggregate"),"kg in a cubic meter mixture"),
       tags$li(code("Age"),"Days"),
       tags$li(code("Concrete_Compressive_Strength"),"MPa, megapascals"),
      ),
      h2("Page Descriptions"),
      h4("The",strong(" Data Page")," presents a scrollable data view.  Options are available to subset the data view for both columns and rows.  Optionally, the current data view can be downloaded and saved."),
      h4("The",strong(" Data Exploration Page")," displays both graphical and numeric summaries with several different summary choices.  Data for summaries can be subset for certain columns and row values."),
      h4("The",strong(" Modeling Page")," fits mulitple regression, regression tree, and random forest models.  This page is divided into three tabs.  The Info tab explains the models.  The Fitting Tab allows the user to select criteria for fitting models.  The Prediction Tab uses a fitted model to let the user select predictor values and obtain predictions for concrete compression strength."),
      hr(),
      p("An excavator-mounted hydraulic jackhammer being used to break up concrete. Public Domain.  Source:",
        a(href="https://commons.wikimedia.org/wiki/File:Excavator_jackhammer.jpg","Wikimedia Commons")
      ),
      img(src="Excavator_jackhammer.jpg"),
    )
  ),
  
######Data Page Section#######################################
  tabPanel("Data",
    fluidPage(
     uiOutput("DataTitle"),  
     sidebarLayout(
      sidebarPanel(
        downloadButton("downloadData", "Download and Save Data"),
        checkboxInput("subdata", h4("Subset Data", style = "color:green;")),          conditionalPanel("input.subdata", 
              checkboxGroupInput("dtcolumns", 
                      h4("Select Columns to Display"),
                      choices = colnames(fullData),
                      selected = colnames(fullData)),
              h4("Adjust Slider Values to Filter Rows"),
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
      mainPanel(
                dataTableOutput("tableResults"))
     )  
    )           
           ),
  
#####Data Exploration Page Section################################
  tabPanel("Data Exploration",
   fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxInput("filterData", h4("Filter Data Rows (Selections Filter Data for ALL Summaries)")),
        conditionalPanel(
          "input.filterData==1",
#          h4("Selections Filter Data for ALL Summaries"),
          radioButtons("superAdded","Superplastizer Added",
            choices = list("No" = 1,"Yes" = 2, "All Rows" = 3),
            selected = 3
          ),
          radioButtons("blastAdded","Blast_Furnace_Slag Added",
                       choices = list("No" = 1,"Yes" = 2, "All Rows" = 3),
                       selected = 3
          ),
          uiOutput("slideRatio")
        ),
        hr(),
        h4("Graphical Summaries"),
        radioButtons("radioGraph","Select Graph Type",
          choices = list("Scatter Plot" = 1, 
                         "Correlation Plot" = 2,
                         "Histogram" = 3),
          selected = 1
        ),
        conditionalPanel(
          "input.radioGraph == 1",
          h4("Scatter Plot Variable Selection"),
          selectInput("selectY","Select Y Axis Variable",
                  choices = colnames(fullData),
                  selected = "Concrete_Compressive_Strength"),
          selectInput("selectX","Select X Axis Variable",
                      choices = colnames(fullData),
                      selected = "Cement")
        ),
        conditionalPanel(
          "input.radioGraph == 2",
          h4("Correlation Plot Variable Selection"),
          checkboxGroupInput("corSelect", "Correlation Variables",
                             choices = colnames(fullData),
                             selected = colnames(fullData)),
        ),
        conditionalPanel(
          "input.radioGraph == 3",
           h4("Histogram Variable Selection"),
           selectInput("selectHVar","Select Variable",
              choices = colnames(fullData),
              selected = "Concrete_Compressive_Strength")
        ),
        hr(),
        h4("Numerical Summaries"),
        radioButtons("radioNum","Select Numical Summary Type",
              choices = list("Five Number Summary" = 1,
                             "Mean, SD, IQR Summary" = 2,
                             "Variance-Covariance Table" = 3),
              selected = 1
        ),
        conditionalPanel(
          "input.radioNum == 1",
          h4("Five Number Summary Variable Selection"),
          checkboxGroupInput("fiveSelect", "Summary Variables",
                             choices = colnames(fullData),
                             selected = colnames(fullData)),
        ),
        conditionalPanel(
          "input.radioNum == 2",
          h4("Mean, SD, IQR Summary Variable Selection"),
          checkboxGroupInput("threeSelect", "Summary Variables",
                             choices = colnames(fullData),
                             selected = colnames(fullData)),
        ),
       conditionalPanel(
          "input.radioNum == 3",
          h4("Variance-Covariance Table Variable Selection"),
          checkboxGroupInput("contSelect", "Summary Variables",
                     choices = colnames(fullData),
                     selected = colnames(fullData)),
)


      ),
      mainPanel(
        htmlOutput("graphText"),
        plotOutput("PlotOut"),
        br(),
        hr(),
        htmlOutput("numberText"),
        tableOutput("numberOut")
      )
    )   
   )         
          ),
  
######Modeling Page Section#####################################
  tabPanel("Modeling",
   mainPanel(
      tabsetPanel(
        tabPanel("Modeling Info",
          fluidPage(
            h2("Model Explanations"),
            h4("Explain 1"),
            h4("Explain 2"),
            h4("Explain 3")
          )         
        ),
        tabPanel("Model Fitting",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               h3("Holding for Model Controls"),
               actionButton("submit","Fit Models"),
             ),
             mainPanel("Model Output Goes Here",
               verbatimTextOutput("modTest"),
               plotOutput("rfPlot")
             )
           )
          )
        ),
        tabPanel("Prediction",
         fluidPage(
          sidebarLayout(
            sidebarPanel(
              h3("Holding for Prediction Controls")
            ),
            mainPanel("Prediction Output Goes Here")
            )
          )        
        )
      )
   )           
  )
                   
                                      
))                   


