#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

shinyServer(function(input, output, session) {
  
  # Download button for the table display - not finished
  output$dlData = downloadHandler(
    filename = function() {
      paste("Pokemon List.csv")
    },
    content = function(file) {
      write.csv(pokeData, file)
    }
  )
  
  # table for pokemon data
  output$list <- renderTable({   
    
    #data has not been adjusted
    if(!input$modified){          
      pokeData
    }
    
    #data has been adjusted to meet specifications given by user
    else{
      pokeData[pokeData$'Type 1'== input$type1
               & pokeData$'Type 2'== input$type2
               & pokeData$HP>input$hp
               & pokeData$Attack>input$att
               & pokeData$Defense>input$def
               & pokeData$Speed>input$spd
               & pokeData$'Sp. Atk'>input$spAtt
               & pokeData$'Sp. Def'>input$spDef,]
    }
    
  })
})
