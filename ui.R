library(markdown)
library(plotly)
navbarPage("Word Prediction Application",
           tabPanel("Word Prediction App",
                    sidebarLayout(
                      sidebarPanel(
                        textInput("txtSentenceInput", "Enter a sentence to predict next word", "Hello"),
                        selectInput("iNumber", "Select number of Words to predict", c(1,2,3,4),selected = 4)
                      ),
                      mainPanel(h4("Predicted next word(s)"),
                                tableOutput('table'),
                                fluidRow( column(12,fluidRow(
                                  column(6, plotOutput("barplot")),
                                  column(6, h4("Frequency Word Cloud of Predicted Words"), plotOutput("wordcloudplot"))
                                )
                                )
                                )
                        
                      )
                    )
           ),
           tabPanel("Help",
                    mainPanel(
                      includeMarkdown("Documentation.md")
                    ) # mainPanel
                    
           ))
