

library(shiny)
library(plotly)
library(tidyverse)
# Define UI
shinyUI(
    fluidPage(
        
        # Application title
        titlePanel("Macrometer"),
        
        # Sidebar Content 
        sidebarLayout(
            sidebarPanel(
                wellPanel(id = "Units",
                          radioButtons("UT","Unit Selection",c("Imperial" = "I","Metric"= "M"))
                ),
                wellPanel(id = "Output",
                          radioButtons("OT","Output Type",c("Macros" = "M","Calories"= "C","Grams"="G"))
                ),
                tabsetPanel(
                    tabPanel("Height",
                             conditionalPanel(condition = "input.UT == 'I'",
                                              numericInput("FT", "Feet",5,min = 1, max = 10),
                                              numericInput("IN", "Inches",0,min = 0, max = 12)
                             ),
                             conditionalPanel(condition = "input.UT == 'M'",
                                              numericInput("MT", "Meters",1.5,min = 0, max = 10)
                             )
                    ),
                    tabPanel("Weight",
                             conditionalPanel(condition = "input.UT == 'I'",
                                              numericInput("LB", "Pounds",185,min = 0, max = 1000)
                             ),
                             conditionalPanel(condition = "input.UT == 'M'",
                                              numericInput("KG", "Kilograms",85,min = 0, max = 1000)
                             )
                    ),
                    tabPanel("Age",
                             numericInput("AG", "Years",30,min = 0, max = 150)
                    ),
                    tabPanel("Sex",
                             radioButtons("Sex","Sex",c("Male" = "M","Female"= "F"))
                    ),
                    tabPanel("Goals",
                             radioButtons("Goal","Goal",c("Increased Muscle/Endurance" = "IM",
                                                          "Fat Loss" = "FL",
                                                          "Boost Power" = "BP")),
                             h3("Weight Loss Per Week"),
                             conditionalPanel(condition = "input.UT == 'I'",
                                              numericInput("LLB", "Pounds",0,min = 0, max = 1000)
                             ),
                             conditionalPanel(condition = "input.UT == 'M'",
                                              numericInput("LKG", "Kilograms",0,min = 0, max = 1000)
                             )
                    ),
                    tabPanel("Advanced",
                             radioButtons("BMR","Base Metabolic Rate Formula",c("Mifflin-St Jeor Equation:" = "MSJ",
                                                                                "Revised Harris-Benedict Equation:" = "RHB",
                                                                                "Katch-McArdle Formula:" = "KM")),
                             numericInput("BF", "Body Fat Percentage (Required For Katch-McArdle Formula)",0.15,min = 0, max = 1)
                             
                    )
                )
            ),
            
            
            # Show a plot of the generated distribution
            mainPanel(
                plotlyOutput("distPlot")
                
                
            )
        )
    )
)
