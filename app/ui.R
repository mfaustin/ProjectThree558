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
fullData<-read_excel("./data/Concrete_Data.xls")

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
      h4("The",strong(" Data Exploration Page")," displays both graphical and numerical summaries with several different summary choices.  Data for summaries can be subset for certain columns and row values."),
      h4("The",strong(" Modeling Page")," fits multiple regression, regression tree, and random forest models.  This page is divided into three tabs.  The ",strong("Model Info")," tab explains the models including benefits and drawbacks of each approach.  The ",strong("Model Fitting")," tab allows the user to select criteria for fitting models.  The user can choose which predictor variables to include, common settings such as number of CV folds, and other model settings.  The ",strong("Prediction")," tab allows the user to choose a fitted model and select predictor values to obtain predictions for concrete compressive strength."),
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
   fluidPage(
      tabsetPanel(
        ##Modeling Info Tab
        tabPanel("Model Info",
          fluidPage(
            withMathJax(),
            h4("Multiple Linear Regression"),
            h5("The general multiple linear regression model:"),
            h5('$$Y_i = \\beta_0 + \\beta_1 x_{i1} + \\beta_2 x_{i2} + \\ldots + \\beta_p x_{ip} + E_i  $$'),
            h5("for the ith observation of response Y and p explanatory variables x describes a linear relationship between the response and explanatory variables.  Multiple linear regression models can include polynomial and interaction terms.  Linear regression models are fit with training data by minimizing the sum of squared errors.  In the context of prediction, the desired outcome is to minimize prediction error between predicted and observed values: "),
            h5('$$M S E=\\frac{1}{n} \\sum_{i=1}^{n}\\left(y_{i}-\\hat{y}_{i}\\right)^{2}$$'),
            h5("A drawback to linear regression models is that they can be influenced by outliers and influential observations.  Multicollinearity or correlation between independent variables can also exist.  Methods exist to check or remedy these potential drawbacks but care needs to be taken."),
            h5("Benefits of linear regression models are that they are simple to implement compared to other models and are often highly interpretable."),
            hr(),
            h4("Regression Tree"),
            h5("For tree based methods, the predictor space is split into regions with different predictions for each region.  This data has a continuous response so we are working with regression tree models.  Recursive binary splitting is used to calculate s split values for j total predictors.  Given Regions 1 and 2 denoted here:"),
            h5('$$R_{1}(j, s)=\\left\\{x \\mid x_{j}<s\\right\\}$$ and $$
R_{2}(j, s)=\\left\\{x \\mid x_{j} \\geq s\\right\\}$$'),
            h5("Using mean response in each region, the goal is to find s and j values to minimize the following equation:"),
            h5('$$\\sum_{i: x_{i} \\in R_{1}(j, s)}\\left(y_{i}-\\bar{y}_{R_{1}}\\right)^{2}+\\sum_{i: x_{i} \\in R_{2}(j, s)}\\left(y_{i}-\\bar{y}_{R_{2}}\\right)^{2}$$'),
            h5("This process is continued for each split until a tree with a large number of nodes is produced.  At this point trees usually need to be pruned using cost-complexity."),
            h5("A disadvantage of basic tree based models is they often need the extra step of pruning in order to avoid overfitting.  In addition, small changes to data can result in big changes to the tree.  The splitting algorithm is said to be greedy because it only takes the current split calculation into account which only accounts for the best split for the current split and does not take future splits into account.  In other words, there is no optimal algorithm to determine the splits."),
            h5("An advantage would be that basic tree models are easy to interpret and visualize.  Linear methods with lots of interactions and higher order terms can be much harder to interpret.  Whether basic or ensemble, tree based models have more flexible fits than many linear methods."),  
            hr(),
            h4("Random Forest"),
            h5("Random forest models aggregate results from many sample decision trees. Those sample trees are produced using bootstrap samples created using resampling with replacement. A tree is trained on each bootstrap sample, resulting in a prediction based on that training sample data. Results from all B bootstrap samples are averaged to arrive at a final prediction."),
            h5('$$\\hat{y}(x)=\\frac{1}{B} \\sum_{j=1}^{B} \\hat{y}^{* j}(x)
$$'),
            h5("Both bagging and random forest methods use bootstrap sampling with decision trees. However, bagging includes all predictors which can lead to less reduction in variance when strong predictors exist. Unlike bagging, random forests do not use all predictors but use a random subset of predictors for each bootstrap tree fit.  For the regression random forest case, the m number of p predictors to try is often chosen as \\(m=p/3\\)."),
            h5("One disadvantage of ensemble methods like random forest is that they are harder to interpret compared to a simple tree or many regression models.  Another disadvantage of ensemble methods is that they are particularly computing intensive with much longer runtimes than other methods."),
            h5("The main advantage of the random forest method is that by averaging results across many trees it decreases variance compared to a single tree fit.  Random forest improves over bagging by using the random subset of predictors which reduces variance compared to bagging and usually results in a better fitting model."),
            br(),br(),br()
          )         
        ),
        ##Model Fitting Tab
        tabPanel("Model Fitting",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               h4("Split Data into Training/Test Sets:"),
               sliderInput("splitSlide",
                  "Select Proportion of Data for Training Set
                  (remainder is assigned to test set)",
                  min=.50, max=.80,value = .70),
               hr(),
               h4("Cross Validation Values to Apply to ALL Models"),
               selectInput("numFolds",
                           "Select Number of Folds",
                           choices = list("5"=5,"10"=10),
                           selected = 5),
               selectInput("numRepeats",
                           "Select Number of Repeats",
                           choices = list("1"=1,"2"=2,"3"=3),
                           selected = 3),
               hr(),
               checkboxGroupInput("regVars", 
                                  h4("Select Multiple Linear Regression Model Predictor Variables"),
                                  choices = colnames(fullData)[1:8],
                                  selected = colnames(fullData)[1:8]),
               hr(),
               checkboxGroupInput("treeVars", 
                                  h4("Select Regresion Tree Model Predictor Variables"),
                                  choices = colnames(fullData)[1:8],
                                  selected = colnames(fullData)[1:8]),
               selectInput("cpMax",
                        "Select Max CP for Regression Tree Model",
                        choices = list("0.10"=0.10,"0.15"=0.15,"0.20"=0.20),
                        selected =0.10),
               selectInput("cpIncrement",
                        "Select CP Increment for Regression Tree Model",
                        choices = list("0.01"=0.01,"0.001"=0.001),
                        selected =0.01),
               hr(),
               checkboxGroupInput("rfVars", 
                    h4("Select Random Forest Model Predictor Variables"),
                                  choices = colnames(fullData)[1:8],
                                  selected = colnames(fullData)[1:8]),
               uiOutput("mtrySelect"),
               hr(),
               actionButton("submit","Fit Models"),
             ),
             mainPanel(
               uiOutput("trainHeader"),
               uiOutput("regModHeader"),
               verbatimTextOutput("regTrain"),
               verbatimTextOutput("regTrainFit"),
               uiOutput("treeHeader"),
               verbatimTextOutput("treeTrain"),
               plotOutput("treePlot"),
               uiOutput("rfHeader"),
               verbatimTextOutput("rfTrain"),
               plotOutput("rfPlot"),
               uiOutput("testHeader"),
               tableOutput("testTable"),
               textOutput("testText"),
               br(),br(),br(),br(),br()
             )
           )
          )
        ),
        ##Model Prediction Tab
        tabPanel("Prediction",
         fluidPage(
          sidebarLayout(
            sidebarPanel(
              uiOutput("predModUI"),
              uiOutput("predVarOne"),
              uiOutput("predVarTwo"),
              uiOutput("predVarThree"),
              uiOutput("predVarFour"),
              uiOutput("predVarFive"),
              uiOutput("predVarSix"),
              uiOutput("predVarSeven"),
              uiOutput("predVarEight"),
              uiOutput("predButton")
            ),
            mainPanel(
              br(),br(),br(),br(),br(),br(),
              uiOutput("predHeader"),
              uiOutput("predText")
              )
            )
          )        
        )
      )
   )           
  )
                   
                                      
))                   


