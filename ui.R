library(markdown)
library(plotly)
navbarPage("Word Prediction Application",
           tabPanel(
                      mainPanel(
                                includeMarkdown("Documentation.md")
                              ) # mainPanel

           ),
           tabPanel("Propane Price App",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("iPeriodInput", "Select Period", format(ISOdate(2017,1:12,1),"%B")),
                        textInput("iPriceInput", "Enter Crude Oil Price", 0),
                        submitButton("Calculate")
                        
                      ),
                      mainPanel(
                        plotlyOutput("plot1"),
                        plotlyOutput("plot2"),
                        tableOutput("predResults")
                        
                      )
                    )
           ))
