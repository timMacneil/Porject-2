#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(tree)
library(class)

# Reads in pokemon data, adds a Total Statistic setting, renames data with problematic names
# and sorts appropriately
pokeData = read_csv("pokemon.csv")
colnames(pokeData)[8] = "SpAtk"
colnames(pokeData)[9] = "SpDef"
pokeData[is.na(pokeData)] = "N/A"
pokeData <- mutate(pokeData, StatTotal = Attack + Defense + Speed 
                                         + HP + SpDef + SpAtk)  
pokeData <- pokeData[,c(1:10,13,11:12)]

# Define UI for application 
shinyUI(fluidPage(
  
  # Application tile
  titlePanel("Pokemon Stastistics Data"),
  
  # Sidebar with a selections for pokemon types and statistics
  sidebarLayout(
    sidebarPanel(
      h3("Enter Statistics for your Pokemon"),
      selectInput("type1", "Type 1", choices = c("All",pokeData$`Type 1`)),
      selectInput("type2", "Type 2", choices = c("All",pokeData$`Type 2`)),
      checkboxInput("legCheck", h4("Select Legendary Pokemon?")),
      checkboxInput("genCheck", h4("Select Pokemon by Generation?")),
      conditionalPanel(condition ="input.genCheck",
                       numericInput("gen", "Generation", min = 1, max = 6, value = 1)),
      checkboxInput("modified", h4("Show Pokemon with these traits?")),
      downloadButton("dlData", "Download Pokemon List")
    ),
    
    # Show a plot of the relevant pokemon
    mainPanel(
      tabsetPanel(
        #displays pokemon data
        tabPanel("Pokemon Table",tableOutput("list")),
        
        #displays histogram of stat totals of described pokemon
        tabPanel("Histogram", plotOutput("statTotPlot"),
                 downloadButton("dlPng", "Download Histogram")), 
        
        #displays plots display stat trends
        tabPanel("Comparing Offense/Defense", plotOutput("statCompPlot", click = "plotClick"),
                 verbatimTextOutput("pokeInfo")),
        
        #displays Stat Total tree based off of a single user picked stat
        tabPanel("Tree", verbatimTextOutput("regTreeInfo"),  
                 plotOutput("regTree"),
                 selectizeInput("treeStat", "Tree Stat", 
                                choices = c('HP','Attack', 'Defense', 'Speed', 'Sp. Atk', 'Sp. Def'))),
        
        #displays Stat Total linear regression based off of single user stat 
        tabPanel("Regression", verbatimTextOutput("regInfo"),
                 plotOutput("regrLin"),
                 selectizeInput("regStat", "Prediction Stat", 
                                choices = c('HP','Attack', 'Defense', 'Speed', 'Sp. Atk', 'Sp. Def')),
                 numericInput("pred", "Predictor Value", min = 0, max = 200, value = 0),
                 verbatimTextOutput("predInfo")),
        
        #displays clusters for stat totals by user defined clusters
        tabPanel("Clustering", verbatimTextOutput("clustInfo"),
                 plotOutput("clustPlot"),
                 selectizeInput("clustStat", "Stat to compare with Stat Total", 
                                choices = c('HP','Attack', 'Defense', 'Speed', 'Sp. Atk', 'Sp. Def')),
                 selectizeInput("clustType", "Cluster by Height or Number of Clusters?", 
                                choices = c('Height','Clusters')),
                 conditionalPanel(condition = "input.clustType=='Height'",
                                  sliderInput("heightClu", "Cluster Height", 
                                              min = 5, max = 200, step = 10, value = 80)),
                 conditionalPanel(condition = "input.clustType=='Clusters'",
                                  sliderInput("clustNum", "Number of clusters", 
                                              min = 1, max = 20, step = 1, value = 5))),
          
        #displays information about the app
        tabPanel("Information", 
                 h2("Pokemon Data Organizer for Project 3 of NCSU ST590 Fall 2018"),
                 p("This App was constructed by Tim MacNeil for Project 3 of the NCSU ST590 course in Fall 2018.", 
                 "It loads a data file of information pertinent to pokemon.  It allows the user to sort through the ", 
                 "data based on specifications to find the desired pokemon and to see representations of total stats",
                 "Total stats are calculated by",
                 withMathJax(helpText("$$HP + Attack + Defense + Sp. Atk + Sp. Def + Speed = Total$$"))),
                 br(),
                 
                 #Pokemon Table
                 h3("Pokemon Table"),
                 p("This tab displays all of the pokemon battle statistics, as well as the types, the generation in 
                   which the pokemon was released and whether it is a legendary pokemon.  Users are allowed to enter
                   specifications for those variables to pinpoint pokemon meeting their specifications as well as download 
                   a csv of the relevant pokemon"),
                 br(),
                 
                 #Histogram
                 h3("Histogram"),
                 p("This tab creates a histogram of the total stats of the pokemon.  Users are able to download",
                   "this histogram as a png file"),
                 br(),
                 
                 #Offense/Defense Comparison
                 h3("Comparing Offense/Defense"),
                 p("This tab creates a plot showing the total attack and total defense statistics of the pokemon specified.",
                   "Total Attack and Total Defense are calculated as such:",
                 withMathJax(helpText("$$Attack + Sp. Atk + Speed = Total Attack$$")),
                 withMathJax(helpText("$$HP+ Defense + Sp. Def + Speed = Total Defense$$"))),
                 
                 #Tree
                 h3("Tree"),
                 p("This tab creates a regression tree of the total stats of the pokemon compared to a user selected stat"), 
                 br(),
                 
                 #Regression
                 h3("Regression"),
                 p("This tab creates a regression line of the total stats of the pokemon compared to a user selected stat",
                   "It also allows the user to find the predicted outcome of the Total Stats depended on a given value"),
                 br(),
                 
                 #Clustering
                 h3("Clustering"),
                 p("This tab creates a Hierarchical Cluster of the total stats of the pokemon compared to a user selected", 
                   "stat.  It also allows to specify whether the heirarchy depends on height or number of clusters, and ",
                   "displays a graph of the clusters comapred to the stat"),
                 br(),
                 
                 #Sources
                 h4("Sources"),
                 uiOutput("webLink"))
      )
    )
  )
))
