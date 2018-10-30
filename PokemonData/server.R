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
  
  #reactive value to adjust displayed data
  displayData <- reactive({
    
    #data has not been adjusted
    if(!input$modified){        
      newData <- pokeData
    }
    
    #data has been adjusted to meet specifications given by user
    else{
      newData <- pokeData[pokeData$'Type 1'== input$type1
                          & pokeData$'Type 2'== input$type2
                          & pokeData$HP>input$hp
                          & pokeData$Attack>input$att
                          & pokeData$Defense>input$def
                          & pokeData$Speed>input$spd
                          & pokeData$'Sp. Atk'>input$spAtt
                          & pokeData$'Sp. Def'>input$spDef,]
    }
  })
  
  # Download button for the table display
  output$dlData <- downloadHandler(
    filename = "Pokemon List.csv",
    content = function(file) {
      write.csv(displayData(), file)
    },
    contentType = "csv"
  )
  
  # Download button for the histogram plot
  output$dlPng <- downloadHandler(
    filename = "Pokemon Histogram.png",
    
    content = function(file) {
      #creates histogram to download
      png(file)
      plotData <- displayData()
      statTotals <- plotData$Attack + plotData$Defense + plotData$Speed + 
        plotData$HP + plotData$'Sp. Atk' + plotData$'Sp. Def'
      bins <- seq( min(statTotals), max(statTotals), length.out = 20)
      hist(statTotals, breaks = bins, xlab = "Stat Total for Pokemon",
           main = "Histogram of Stat Totals")
      dev.off()
    },
    contentType = "png"
  )
  
  #creates histogram of total statistics of the pokemon currently selected
  output$statTotPlot <- renderPlot({
    
    #gets data to display and establishes stat total for each pokemon
    plotData <- displayData()
    statTotals <- plotData$Attack + plotData$Defense + plotData$Speed + 
      plotData$HP + plotData$'Sp. Atk' + plotData$'Sp. Def'
    
    #creates bin sizes and creates histogram
    bins <- seq( min(statTotals), max(statTotals), length.out = 20)
    hist(statTotals, breaks = bins, xlab = "Stat Total for Pokemon",
         main = "Histogram of Stat Totals")
  })
  
  #creates plots for attack and defense of the pokemon currently selected
  output$statCompPlot <- renderPlot({
    
    #gets data to display
    plotData <- displayData()
    
    #creates plots comparing total attack to total defense
    g <- ggplot(plotData,aes(x = Attack+`Sp. Atk`+Speed, y = Defense+`Sp. Def`+HP))
    g + geom_point(size = 2) + xlab("Combined Attack Stats") + ylab("Combined Defense Stats")
  })
  
  # table for pokemon data
  output$list <- renderTable({   
    displayData()
  })
  
  # Shows location of cursor for data in the attack/defense plot
  output$location <- renderText({
    paste0("x=", input$plotClick$x, "\ny=", input$plotClick$y)
  })
})
