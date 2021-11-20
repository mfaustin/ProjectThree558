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

  
##Graphical Summary Section  
output$PlotOut <-renderPlot({
  if (input$radioGraph==1){
    g <- ggplot(fullData, 
                aes(x = !!sym(input$selectX),
                    y = !!sym(input$selectY))) 
    g + geom_point()
  } else {
    corrData<-fullData %>% select(input$corSelect)
    Correlation<-cor(corrData,method = "spearman")
    #corrplot(Correlation)
    corrplot(Correlation,type="upper",tl.pos="lt", tl.cex = .70)
    corrplot(Correlation,type="lower",method="number",
             add=TRUE,diag=FALSE,tl.pos="n",tl.cex = .70,number.cex = .75)
  }
  
  
})    

  ##Numerical Summary Section  
  #observe({print(input$dtcolumns )})
  #observe({print(input$corSelect )})
  
  output$numberOut <- renderTable({
    if (input$radioNum==1){    
     fullData %>% select(input$fiveSelect) %>%
      summary() %>% as.data.frame() %>%
      separate(Freq, c("Stat", "Value"), sep=":") %>%
      pivot_wider(names_from =Stat, values_from = Value) %>%
      select(-Var1,-`Mean   `) %>% rename(Variable=Var2)
    } else if(input$radioNum==2) {
      outSummary<-apply(X = select(fullData,all_of(input$threeSelect))
                    , MARGIN = 2,
            FUN = function(x) {
              temp <- c(mean(x), sd(x), IQR(x))
              names(temp) <- c("mean", "sd","IQR")
              temp
            }
      )
      as.data.frame(t(as.data.frame(outSummary))) %>% 
          rownames_to_column(var = "Variable")
      
    }
    
    #sapply(fullData, function(x) 
    #  list(means = mean(x), sds = sd(x), IQR = IQR(x)))
      
  })
  

  
})
