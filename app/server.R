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
library(corrplot)
library(caret)


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


##Define Shiny Server
shinyServer(function(input, output, session){

  
#####Code for Data Page Handling############################
  
  #create title text 
  output$DataTitle <- renderUI({
    if (input$subdata){
      subtext<-"Subsetting"
    }else{
      subtext<-"All"
    }
    text <- paste0("Concrete Compressive Strength ",subtext," Data")
    h1(text)
  })

  dataOut<-reactive({
    ##if subsetting is checked first do filtering
    ##  then do column selection
    if (input$subdata){
     subdata<-fullData %>%
      filter(between(Cement,input$CementSlide[1],input$CementSlide[2])
       & between(Blast_Furnace_Slag,input$BlastSlide[1],input$BlastSlide[2])
       & between(Fly_Ash,input$FlySlide[1],input$FlySlide[2])
       & between(Water,input$WaterSlide[1],input$WaterSlide[2])
       & between(Superplasticizer,input$SuperSlide[1],input$SuperSlide[2])
       & between(Coarse_Aggregate,input$CoarseSlide[1],input$CoarseSlide[2])
       & between(Fine_Aggregate,input$FineSlide[1],input$FineSlide[2])
       & between(Age,input$AgeSlide[1],input$AgeSlide[2])
       & between(Concrete_Compressive_Strength,input$ConcreteSlide[1]        ,input$ConcreteSlide[2])) %>%
       select(input$dtcolumns)
    }else{
     ##Return all data otherwise
     fullData 
    }
    
  })

##Render Table using Reactive Data    
  output$tableResults<- DT::renderDataTable({
                        dataOut()},
                        options = list(scrollY = '500px',
                                    scrollX='200px',paging=FALSE,
                                    searching=FALSE) )

##Used download code example 
##  https://shiny.rstudio.com/articles/download.html  
##Setup to create .csv file based on Reactive Data  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$subdata){
        "ConcreteSubsetData.csv"
      }else{
        "ConcreteFullData.csv"
      }
    },
    content = function(file) {
      write_csv(dataOut(), file)
    }
  )  

  
#####Code for Data Exploration Page Handling#########################
  
  
  ##Adding Ratio Data for Summaries
  ratioData<-reactive({
    dataExploreOut<-fullData %>%
      mutate(cfRatio=round(Coarse_Aggregate/Fine_Aggregate,digits=2))%>%
      select(Coarse_Aggregate,Fine_Aggregate,cfRatio,everything()) 
  })
  
  
  ##Dynamic Ratio Slider Render for Filtering
  output$slideRatio<-renderUI({
    ratioOut<-ratioData()
    sliderInput("cfSlide","Coarse to Fine Aggregate Ratio",
                min = min(ratioOut$cfRatio),
                max = max(ratioOut$cfRatio),
                value = c(min(ratioOut$cfRatio),
                          max(ratioOut$cfRatio)))
    })
     
  ##Summary data filtering 
  dataExplore<-reactive({
  if (input$filterData){  
   fullOut<-ratioData()
   if (input$superAdded==1){
     dataExploreOut<-fullOut %>%
        filter(Superplasticizer==0)
   }else if (input$superAdded==2){
     dataExploreOut<-fullOut %>%
       filter(Superplasticizer>0)
   } else{
     dataExploreOut<-fullOut
   }
    
    if (input$blastAdded==1){
      dataExploreOut<-dataExploreOut %>%
        filter(Blast_Furnace_Slag==0)
    }else if (input$blastAdded==2){
      dataExploreOut<-dataExploreOut %>%
        filter(Blast_Furnace_Slag>0)
    }     
    
     ##Need to make sure the Slider has rendered before
     ## attempting to filter so use req()
     req(input$cfSlide)
    dataExploreOut<-dataExploreOut %>%
      filter(between(cfRatio,input$cfSlide[1],input$cfSlide[2]))   

   
#    observe({print(input$superAdded )})
#    observe({print(input$cfSlide )})
    
#    observe({print(dataExploreOut)})
#    observe({print(summary(dataExploreOut$cfRatio))})
    dataExploreOut
  } else{
    fullData
  }  
  })  
  
##Graphical Summary Section  
output$PlotOut <-renderPlot({
  summaryData<-dataExplore()
  if (input$radioGraph==1){
    ##Scatterplot section
    output$graphText<-  renderText({"<b>Scatter Plot<b>"})
    g <- ggplot(summaryData, 
                aes(x = !!sym(input$selectX),
                    y = !!sym(input$selectY))) 
    g + geom_point()
  } else if (input$radioGraph==2) {
    ##Corrplot section
    output$graphText<-  renderText({"<b>Correlation Plot<b>"})
#    observe({print(summaryData)})
    corrData<-summaryData %>% select(input$corSelect)
    #observe({print(corrData)})
    ##Special handling for variables filtered to be all 0
    ## in the filtering section.  This condition would lead
    ## to 0 SD so these variables need to be removed before
    ## correlation is computed
    if(input$filterData){
      if (input$superAdded==1 & 
          ("Superplasticizer" %in% names(corrData))){
         corrData<-corrData %>% select(-Superplasticizer)
      }
      if (input$blastAdded==1 & 
          ("Blast_Furnace_Slag" %in% names(corrData))){
          corrData<-corrData %>% select(-Blast_Furnace_Slag)
      }
    }  
    #observe({print(corrData)})
    Correlation<-cor(corrData,method = "spearman")
    corrplot(Correlation,type="upper",tl.pos="lt", tl.cex = 0.9)
    corrplot(Correlation,type="lower",method="number",
             add=TRUE,diag=FALSE,tl.pos="n",tl.cex = 0.9,number.cex = .75)
  } else{
    ##Histogram section
    output$graphText<-  renderText({"<b>Histogram<b>"})
    g <- ggplot(summaryData,aes(x=!!sym(input$selectHVar)))
    g + geom_histogram(bins=40,color = "brown", fill = "green", 
                       size = 1)
  }
  
  
})    

  ##Numerical Summary Section  
  output$numberOut <- renderTable({
    summaryData<-dataExplore()
    if (input$radioNum==1){
      output$numberText<-  renderText({"<b>Five Number Summary Table<b>"})
      summaryData %>% select(input$fiveSelect) %>%
      summary() %>% as.data.frame() %>%
      separate(Freq, c("Stat", "Value"), sep=":") %>%
      pivot_wider(names_from =Stat, values_from = Value) %>%
      select(-Var1,-`Mean   `) %>% rename(Variable=Var2)
    } else if(input$radioNum==2) {
      output$numberText<-  renderText({"<b>Mean, SD, IQR Table<b>"})
      outSummary<-apply(X = select(summaryData,all_of(input$threeSelect))
                    , MARGIN = 2,
            FUN = function(x) {
              temp <- c(mean(x), sd(x), IQR(x))
              names(temp) <- c("mean", "sd","IQR")
              temp
            }
      )
      as.data.frame(t(as.data.frame(outSummary))) %>% 
          rownames_to_column(var = "Variable")
    } else{
      output$numberText<-  renderText({"<b>Variance-Covariance Table<b>"})
      summaryData<-summaryData %>% select(input$contSelect)
      covOut<-cov(summaryData)
      covOut<-as.data.frame(covOut)%>%rownames_to_column(var = "Variable")
    }
    
  })
  
#####Code for Modeling Page Handling#########################
  
####Code for Model Fitting Tab Part###########################  
 
  ##Split data into train and test sets
  ## with proportion based on user input
  ##Only run when submit button selected
  splitData <- eventReactive(input$submit,{
    dataIndex <-createDataPartition(fullData$Concrete_Compressive_Strength,
                                    p = input$splitSlide, list = FALSE)
    dataTrain <-fullData[dataIndex,]
    dataTest <-fullData[-dataIndex,]
    list(dTrain = dataTrain, dTest=dataTest)    
  })
  
   
  ##Use eventReactive() to Fit Models ONLY when button submitted

    
  ##Multiple Linear Regression Model
  regModel <- eventReactive(input$submit,{
    splitResults <- splitData()
    regForm<-reformulate(input$regVars,
                         response="Concrete_Compressive_Strength")
    withProgress(message=
        "Fitting Mulitple Linear Regression Model.  This may take a few minutes  ",
                value = NULL,{
                   
    regFit <- train(regForm, 
                     data = splitResults$dTrain,
                     method = "lm",
                     preProcess = c("center", "scale"),
                    trControl = trainControl(method = "repeatedcv",
                                  number = as.numeric(input$numFolds),
                                  repeats = as.numeric(input$numRepeats)))
    regFit
  })
  })
  
  observe({print(regModel())})
  
  ##Regression Tree Model  
  treeModel <- eventReactive(input$submit,{
    splitResults <- splitData()
    treeForm<-reformulate(input$treeVars,
                         response="Concrete_Compressive_Strength")
    withProgress(message=
                   "Fitting Regression Tree Model.  This may take a few minutes  ",
                 value = NULL,{
                   
    treeFit <- train(treeForm, 
                data = splitResults$dTrain,
                method = "rpart",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                        number = as.numeric(input$numFolds),
                       repeats = as.numeric(input$numRepeats)),
                tuneGrid = data.frame(cp = seq(0,0.1, 0.01)))
    treeFit
  })
  })  
  
  observe({print(treeModel())})
  
  ##random forest model
  rfModel <- eventReactive(input$submit,{
    splitResults <- splitData()
    #observe({str(splitResults$dTrain)})
    withProgress(message=
          "Fitting Random Forest Model.  This may take a few minutes  ",
                 value = NULL,{
    
    rfFit <- train(Concrete_Compressive_Strength ~ ., 
                    data = splitResults$dTrain,
                    method = "rf",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "repeatedcv",
                                    number = as.numeric(input$numFolds),
                                    repeats = as.numeric(input$numRepeats)),
                    tuneGrid = data.frame(mtry = 1:8))
    rfFit
  })    
  })

observeEvent(input$submit,{
  print(rfModel())
})  
  
output$modTest<-renderPrint({
  rfModel()
}) 

output$rfPlot<-renderPlot({
  rfImp <- varImp(rfModel(), scale = TRUE)
  plot(rfImp, top = 5, main="Random Forest Model\n Importance Plot")
  
})



    
})
