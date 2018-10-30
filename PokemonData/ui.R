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


# Reads in pokemon data and converts NA values to "None".  Only 'Type 2' has NA values because the Pokemon
# is only one type.  This translates that appropriately for the data.
pokeData = read_csv("pokemon.csv")
pokeData[is.na(pokeData)] = "None"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application tile
  titlePanel("Pokemon Stastistics Data"),
  
  # Sidebar with a selections for pokemon types and statistics
  sidebarLayout(
    sidebarPanel(
      h3("Enter Statistics for your Pokemon"),
      selectizeInput("type1", "Type 1", choices = as.factor(pokeData$'Type 1')),
      selectizeInput("type2", "Type 2", choices = as.factor(pokeData$'Type 2')),
      numericInput("hp", "< Pokemon Hit Points", min = 1, max = 10, value = 0),
      numericInput("att", "< Pokemon Attack", min = 1, max = 10, value = 0),
      numericInput("def", "< Pokemon Defense", min = 1, max = 10, value = 0),
      numericInput("spAtt", "< Pokemon Special Attack", min = 1, max = 10, value = 0),
      numericInput("spDef", "< Pokemon Special Defense", min = 1, max = 10, value = 0),
      numericInput("spd", "< Pokemon Speed", min = 1, max = 10, value = 0),
      checkboxInput("modified", h4("Show Pokemon with these traits?")),
      downloadButton("dlData", "Download Pokemon List")
    ),
    
    # Show a plot of the relevant pokemon
    mainPanel(
      tabsetPanel(
        tabPanel("Stats of Pokemon",tableOutput("list")), #displays pokemon data
        tabPanel("Plot of Stat Totals", plotOutput("statTotPlot"), #displays histogram of stat totals of described pokemon
                 downloadButton("dlPng", "Download Histogram")), 
        tabPanel("Plots Comparing Offense/Defense", plotOutput("statCompPlot", click = "plotClick"), #displays plots display stat trends
                 verbatimTextOutput("location")), 
<<<<<<< HEAD
        tabPanel("Information", #displays information about the app
                 h3("Pokemon Data Organizer for NCSU ST590"),
                 paste0("This App was constructed by Tim MacNeil for Project 2 of the NCSU ST590 course in Fall 2018.", 
                 "It loads a data file of information pertinent to pokemon.  It allows the user to sort through the ", 
                 "data based on specifications to find the desired pokemon.  It also creates graphical displays to ",
                 "compare the total stats and the combined offense and defense"))
=======
        tabPanel("Information",textOutput("Info")) #displays information about the app
>>>>>>> 3a727f1d1b9c7097d9a8ee93b7bb1587d0a3cddf
      )
    )
  )
))
