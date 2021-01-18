#
#

library(shiny)
library(plotly)
library(tidyverse)
# Define server 
shinyServer(function(input, output) 
{
    library(plotly)
    library(tidyverse)
    output$distPlot <- renderPlotly({
        # Calculate Height in Centimeters
        height <- if(input$UT == "I")
        {round((input$FT * 12 + input$IN)*2.54,2)}
        else {input$MT*100}
        # Calculate Weight in Kilograms
        weight <- if(input$UT == "I")
        {round(input$LB*0.453592,2)}
        else {input$KG}
        # Calculate BMR
        BMR <- if(input$BMR == "MSJ" & input$Sex == "M")
        {round(10*weight + 6.25* height - 5*input$AG + 5,2)}
        else if(input$BMR == "MSJ" & input$Sex == "F")
        {round(10*weight + 6.25* height - 5*input$AG + -161,2)}
        else if(input$BMR == "RHB" & input$Sex == "M")
        {round(13.397*weight + 4.799*height - 5.677*input$AG + 88.362,2)}
        else if(input$BMR == "RHB" & input$Sex == "F")
        {round(9.247*weight + 3.098*height - 4.330*input$AG + 447.593,2)}
        else 
        {round(370 + 21.6*(1-input$BF)*weight,2)}
        # Calculate Minimum Protein
        MP <- case_when(
            0 <= input$AG & input$AG < 1 ~ 1.5,
            1 <= input$AG & input$AG < 3 ~ 1.1,
            3 <= input$AG & input$AG < 13 ~ .95,
            13 <= input$AG & input$AG < 18 ~ .85,
            TRUE ~ 0.8
        )
        proteinG <- if(MP > 1.2) {c(MP,1.6,2)*weight}
        else {c(MP,1.2,1.6,2)*weight}
        proteinM <- if(MP > 1.2) {c(MP,1.6,2)*weight*4/BMR}
        else {c(MP,1.2,1.6,2)*weight*4/BMR}
        
        # Calculate Carb percentage based on Goal
        CP <- case_when(input$Goal == "IM" ~ 55/(55+20),
                        input$Goal == "FL" ~ 25/(25+40),
                        input$Goal == "BP" ~ 40/(40+30)
        )
        # Calculate Fat percentage based on Goal
        FP <- case_when(input$Goal == "IM" ~ 20/(55+20),
                        input$Goal == "FL" ~ 40/(25+40),
                        input$Goal == "BP" ~ 30/(40+30)
        )
        
        # Determine Calories
        tcal <-  if(input$UT == "I")
        {BMR - (input$LLB*3500/7)}
        else {BMR - input$LKG*7700/7}
        calName = paste("Calories:","BMR =",BMR,"Deficit =",BMR-tcal,"Total =",tcal,sep = " ")
        
        Cal <- data.frame(Grams_Protein_Per_KG_Weight = proteinG/weight,
                          Protein = proteinG*4) %>%
            mutate( Protein = pmin(Protein,tcal),
                    Carbohydrates = ifelse( Protein > tcal, 0,CP*(tcal-Protein)),
                    Fat = ifelse(Protein > tcal,0,FP*(tcal-Protein)))
        Cals <- plot_ly(data = Cal, x = ~Grams_Protein_Per_KG_Weight,
                        y = ~Protein, type = 'bar', name = 'Protein')
        Cals <- Cals %>% add_trace(y = ~Carbohydrates, name = 'Carbohydrates')
        Cals <- Cals %>% add_trace(y = ~Fat, name = 'Fat')
        Cals <- Cals %>% layout(yaxis = list(title = 'Percent'),barmode = 'stack', title = calName)
        
        
        # Determine Macros
        Macro <- Cal %>%
            mutate(Protein = Protein/tcal,
                   Carbohydrates = Carbohydrates/tcal,
                   Fat = Fat/tcal)
        # draw the histogram with the specified number of bins
        Macros <- plot_ly(data = Macro, x = ~Grams_Protein_Per_KG_Weight,
                          y = ~Protein, type = 'bar', name = 'Protein')
        Macros <- Macros %>% add_trace(y = ~Carbohydrates, name = 'Carbohydrates')
        Macros <- Macros %>% add_trace(y = ~Fat, name = 'Fat')
        Macros <- Macros %>% layout(yaxis = list(title = 'Percent'),barmode = 'stack', title = "Macros")
        
        
        
        # Determine Grams
        Gram <- Cal %>%
            mutate(Protein = Protein/4,
                   Carbohydrates = Carbohydrates/4,
                   Fat = Fat/9)
        Grams <- plot_ly(data = Gram, x = ~Grams_Protein_Per_KG_Weight,
                         y = ~Protein, type = 'bar', name = 'Protein')
        Grams <- Grams %>% add_trace(y = ~Carbohydrates, name = 'Carbohydrates')
        Grams <- Grams %>% add_trace(y = ~Fat, name = 'Fat')
        Grams <- Grams %>% layout(yaxis = list(title = 'Grams'),barmode = 'group', title = "Grams")
        
        # Output
        if(input$OT == "M") {Macros}
        else if(input$OT == "C") {Cals}
        else {Grams}
        
        
    })
}
    
    )
