Project Three
================
Mark Austin


## Purpose

This App allows the user to interactively explore Concrete Compressive Strength data both as raw data and in select statistical models.  The data source used is the [Concrete Compressive Strength Data Set](https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength) from the UCI machine learning repository.  

The App has pages for Data Table viewing and saving, Data Exploration, and Modeling.  All those pages give the user an interactive experience using R Shiny Functionality.  


## Required R Packages

The following R packages are required to run R code used in this
project.

-   `shiny` The shiny package is used to provide interactive App capacities.  

-   `shinythemes` The shinythemes package is used to apply one of the themes to enhance presentation.  

-   `readxl` The readxl package is used to read Microsoft Excel data.  

-   `DT` The DT package is used for [DataTables](https://datatables.net/) functionality in handling tables.  

-   `tidyverse` The tidyverse package is used for data handling and
    plotting.  
    
-   `caret` The caret package is used for working with data and models.

-   `randomForest` The randomForest package is needed so that caret can fit the random forest model.

-   `ggcorrplot` The ggcorrplot package is used to produce a visual correlation plot.  

-   `plotly` The plotly package is used to produce interactive plots.

-   `shinyWidgets` The shinyWidgets package is used for UI enhancements like a bolder action button.  

## Package Installation Code

``` r
install.packages(c("shiny","shinythemes","readxl","DT","tidyverse","caret","ggcorrplot","randomForest","plotly","shinyWidgets")) 

```

## Code to run this App

``` r
shiny::runGitHub('ProjectThree558',username = 'mfaustin', subdir = 'concrete_project_558', ref = 'main')
```

