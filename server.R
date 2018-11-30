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
library(tree)

# Reads in pokemon data, adds a Total Statistic setting, renames data with problematic names
# and sorts appropriately
pokeData = read_csv("pokemon.csv")
colnames(pokeData)[8] = "SpAtk"
colnames(pokeData)[9] = "SpDef"
pokeData[is.na(pokeData)] = "N/A"
pokeData <- mutate(pokeData, StatTotal = Attack + Defense + Speed 
                                         + HP + SpDef + SpAtk)
pokeData <- pokeData[,c(1:10,13,11:12)]

shinyServer(function(input, output, session) {
  
  #reactive value to adjust displayed data
  displayData <- reactive({
    
    if(!input$modified){ #data has not been adjusted     
      newData <- pokeData
    }
    
    else{ #data has been adjusted to meet specifications given by user
      
      # if not selecting all pokemon
      if(input$type1!="All")
      {
        # if type 2 is a value
        if(input$type2!="All"){
          newData <- pokeData[pokeData$`Type 1`== input$type1 &
                                pokeData$`Type 2`== input$type2,]
        }
        else{
          newData <- pokeData[pokeData$`Type 1`== input$type1,]         
        }
      }
      else
      {
        newData <- pokeData
      }
      
      #if it should check the generation
      if(input$genCheck)
      {  
        newData <- newData[newData$Generation == input$gen,]
      }
      else{
        newData
      }
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
      bins <- seq( min(plotData$StatTotal), max(plotData$StatTotal), length.out = 20)
      hist(plotData$`Stat Total`, breaks = bins, xlab = "Stat Total for Pokemon",
           main = "Histogram of Stat Totals")
      dev.off()
    },
    contentType = "png"
  )
  
  #creates histogram of total statistics of the pokemon currently selected
  output$statTotPlot <- renderPlot({
    
    #gets data to display 
    plotData <- displayData()
    
    #creates bin sizes and creates histogram
    bins <- seq( min(plotData$StatTotal), max(plotData$StatTotal), length.out = 20)
    hist(plotData$StatTotal, breaks = bins, xlab = "Stat Total for Pokemon",
         main = "Histogram of Stat Totals")
  })
  
  #Creates linear regression model of stat totals
  output$regrLin <- renderPlot({
    
    #gets data to display
    plotData <- displayData()

    #creates linear regression model and plots graph
    if(input$regStat == 'Attack')
    {
      g <- ggplot(plotData, aes(x = Attack, y = StatTotal))
      g + geom_point() + xlab("Attack") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    else if(input$regStat == 'Defense')
    {
      g <- ggplot(plotData, aes(x = Defense, y = StatTotal))
      g + geom_point() + xlab("Defense") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    else if(input$regStat == 'HP')
    {
      g <- ggplot(plotData, aes(x = HP, y = StatTotal))
      g + geom_point() + xlab("HP") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    else if(input$regStat == 'Speed')
    {
      g <- ggplot(plotData, aes(x = Speed, y = StatTotal))
      g + geom_point() + xlab("Speed") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    else if(input$regStat == 'Sp. Atk')
    {
      g <- ggplot(plotData, aes(x = SpAtk, y = StatTotal))
      g + geom_point() + xlab("Special Attack") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    else
    {
      g <- ggplot(plotData, aes(x = SpDef, y = StatTotal))
      g + geom_point() + xlab("Special Defense") + ylab("Stat Total") + geom_smooth(method = "lm")
    }
    
  })
  
  #Creates Regression Tree of stat totals
  output$regTree <- renderPlot({
    
    #gets data to display
    plotData <- displayData()
    
    #creates tree based on user input
    if(input$treeStat == 'Attack')
    {
      fitTree <- tree(StatTotal~ Attack, data = plotData)
    }
    else if(input$treeStat == 'Defense')
    {
      fitTree <- tree(StatTotal~ Defense, data = plotData)
    }
    else if(input$treeStat == 'HP')
    {
      fitTree <- tree(StatTotal~ HP, data = plotData)
    }
    else if(input$treeStat == 'Speed')
    {
      fitTree <- tree(StatTotal~ Speed, data = plotData)
    }
    else if(input$treeStat == 'Sp. Atk')
    {
      fitTree <- tree(StatTotal~ SpAtk, data = plotData)
    }
    else
    {
      fitTree <- tree(StatTotal~ SpDef, data = plotData)     
    }
    plot(fitTree, uniform = TRUE)
    text(fitTree)
  })
  
  #creates plots for attack and defense of the pokemon currently selected
  output$statCompPlot <- renderPlot({
    
    #gets data to display
    plotData <- displayData()

    # creates plots comparing total attack to total defense
    g <- ggplot(plotData, aes(x = Attack + Speed + SpAtk, y = HP + Defense + SpDef))
    g + geom_point() + xlab("Combined Attack Stats") + ylab("Combined Defense Stats")
  })
  
  # table for pokemon data
  output$list <- renderTable({   
    displayData()
  })
  
  # Shows location of cursor for data in the attack/defense plot
  output$pokeInfo <- renderText({
    paste0("Total Attack=", input$plotClick$x, "\nTotal Defense=", input$plotClick$y)
  })
  
  #Display for regression tree
  output$regTreeInfo <- renderText({
    paste("Mean Stat Total Tree for selected Pokemon split by", input$treeStat)
  })
  
  #Display for regression tree
  output$regInfo <- renderText({
    paste("Linear regression predicting Stat Total for selected Pokemon with", 
          input$regStat, "as the predictor")
  })
  
  #Display for prediction data
  output$predInfo <- renderText({
    predVal = 0
    plotData <- displayData()
    
    # establishes the correct value for prediction
    if(input$regStat == 'Attack')
    {
      linMod <- lm(StatTotal~ Attack, data = plotData)
      p <- predict(linMod, newdata = data.frame(Attack = input$pred))
      predVal = round(p, digits = 0)
    }
    else if(input$regStat == 'Defense')
    {
      linMod <- lm(StatTotal~ Defense, data = plotData)
      p <- predict(linMod, newdata = data.frame(Defense = input$pred))
      predVal = round(p, digits = 0)
    }
    else if(input$regStat == 'HP')
    {
      linMod <- lm(StatTotal~ HP, data = plotData)
      p <- predict(linMod, newdata = data.frame(HP = input$pred))
      predVal = round(p, digits = 0)
    }
    else if(input$regStat == 'Speed')
    {
      linMod <- lm(StatTotal~ Speed, data = plotData)
      p <- predict(linMod, newdata = data.frame(Speed = input$pred))
      predVal = round(p, digits = 0)
    }
    else if(input$regStat == 'Sp. Atk')
    {
      linMod <- lm(StatTotal~ SpAtk, data = plotData)
      p <- predict(linMod, newdata = data.frame(SpAtk = input$pred))
      predVal = round(p, digits = 0)
    }
    else
    {
      linMod <- lm(StatTotal~ SpDef, data = plotData)
      p <- predict(linMod, newdata = data.frame(SpDef = input$pred))
      predVal = round(p, digits = 0)     
    }
    
    paste(predVal, "is the predicted Stat Total when", input$regStat, "is", input$pred)
  })
  
  #Display for link to data
  output$webLink <- renderUI({
    url <- a("the Hackathon challenge", href="https://www.kaggle.com/terminus7/pokemon-challenge")
    tagList("Data set was provided from kaggle by T7 as part of ", url)
  })
})
