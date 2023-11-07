# Digital-Twin-Analytics-SEARCH

Data scientists use machine learning to analyze and predict information for digital twins, but the selection of an appropriate model is time consuming, repetitive, requires specialized knowledge, and can lead to an unintended bias or preference for the chosen model. The SEARCH (Store, Explore, Assess, Reduce, Confirm, and Holistic) digital analytics platform will provide initial data exploration of an unknown data set. The initial framework will perform analysis on a given dataset to pre-process the data, determine which algorithms are candidates for analysis, perform dimension reduction, validate results over multiple imputations of the data, provide documentation for ease of use, and display the analytics on a graphical user interface (GUI).

## About

The SEARCH package currently supports regression analysis. 

### High Dimensional Models
* <b>P</b>rincipal <b>C</b>omponet <b>R</b>egression (PCR)
* <b>E</b>lastic <b>Net</b> Regression (ENET)
* Ridge Regresssion (RIDGE)
* <b>L</b>east <b>A</b>bsolute <b>S</b>hrinkage and <b>S</b>election <b>O</b>perator Regression (LASSO)
* <b>R</b>andom <b>F</b>orest (RF)
* <b>S</b>upport <b>V</b>ector <b>M</b>achine (SVM)
* <b>M</b>ulti<b>L</b>ayer <b>P</b>erceptron (MLP)

### Low Dimensional Models
* Includes all high dimensional models plus ...
* <b>L</b>inear <b>M</b>odel (LM)
* <b>G</b>eneralized <b>A</b>dditive <b>M</b>odel (GAM)


## Getting Started

1. Install [R](https://cran.r-project.org/bin/macosx/) 

2. Install [RStudio](https://www.rstudio.com/products/rstudio/download/#download) 

3. Install [Miniconda](https://docs.conda.io/projects/miniconda/en/latest/miniconda-install.html)

4. Install package dependencies 
```
Run install_dependencies_rbased.R
```

5. Build and check a package in devtools
```
devtools::check()
```

6. Install the SEARCH package 
```
Build -> Install 
```

7. Navigate to Shiny App
```
inst/shiny/app.R
```

8. Start the Shiny app
```
Run App
```