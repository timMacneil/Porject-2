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
        tabPanel("Stats of Pokemon",tableOutput("list")),
        
        #displays histogram of stat totals of described pokemon
        tabPanel("Histogram of Stat Totals", plotOutput("statTotPlot"),
                 downloadButton("dlPng", "Download Histogram")), 
        
        #displays plots display stat trends
        tabPanel("Plots Comparing Offense/Defense", plotOutput("statCompPlot", click = "plotClick"),
                 verbatimTextOutput("pokeInfo")),
        
        #displays Stat Total tree based off of a single user picked stat
        tabPanel("Stat Total Regression Tree", verbatimTextOutput("regTreeInfo"),  
                 plotOutput("regTree"),
                 selectizeInput("treeStat", "Tree Stat", 
                                choices = c('HP','Attack', 'Defense', 'Speed', 'Sp. Atk', 'Sp. Def'))),
        
        #displays Stat Total linear regression based off of single user stat 
        tabPanel("Stat Total Regression", verbatimTextOutput("regInfo"),
                 plotOutput("regrLin"),
                 selectizeInput("regStat", "Prediction Stat", 
                                choices = c('HP','Attack', 'Defense', 'Speed', 'Sp. Atk', 'Sp. Def')),
                 numericInput("pred", "Predictor Value", min = 0, max = 200, value = 0),
                 verbatimTextOutput("predInfo")),
        
        #displays clusters for stat totals by user defined clusters
        tabPanel("Stat Total Clustering", verbatimTextOutput("clustInfo"),
                 plotOutput("clustPlot"),
                 selectizeInput("clustStat", "What type of display?", 
                                choices = c('Dendrogram','Plot')),
                 sliderInput("clust", "Number of clusters", min = 1, max = 15, step = 1, value = 3)),
          
        #displays information about the app
        tabPanel("Information", 
                 h2("Pokemon Data Organizer for Project 2 of NCSU ST590 Fall 2018"),
                 p("This App was constructed by Tim MacNeil for Project 2 of the NCSU ST590 course in Fall 2018.", 
                 "It loads a data file of information pertinent to pokemon.  It allows the user to sort through the ", 
                 "data based on specifications to find the desired pokemon.  It also creates graphical displays to ",
                 "compare the total stats and the combined offense and defense"),
                 br(),
                 h3("The Stats of Pokemon"),
                 p("This tab displays all of the pokemon battle statistics, as well as the types, the generation in 
                   which the pokemon was released and whether it is a legendary pokemon.  Users are allowed to enter
                   specifications for those variables to pinpoint pokemon meeting their specifications as well as download 
                   a csv of the relevant pokemon"),
                 br(),
                 h3("Histogram of Stat Totals"),
                 p("Given the specifications of the first tab, this tab creates a histogram of the total stats of the 
                   pokemon.  Total stats are calculated as such:"),
                 withMathJax(helpText("$$HP + Attack + Defense + Sp. Atk + Sp. Def + Speed = Total$$")),
                 p("Users are able to download this histogram as a png file"),
                 br(),
                 h3("Plots Comparing Offense and Defense"),
                 p("Again, using specification from the first tab, this tab creates a plot showing the total attack and
                   total defense statistics of the pokemon specified.  Total Attack and Total Defense are calculated as such:"),
                 withMathJax(helpText("$$Attack + Sp. Atk + Speed = Total Attack$$")),
                 withMathJax(helpText("$$HP+ Defense + Sp. Def + Speed = Total Defense$$")),
                 h4("Sources"),
                 uiOutput("webLink"))
      )
    )
  )
))
