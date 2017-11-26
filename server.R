library(shiny)
library(readxl)
library(plotly)
dfData<-read_xlsx("Data.xlsx")
dfData$MonthVal<-format(as.Date(dfData$Period), "%B")
shinyServer(function(input, output) {
  
  
  modelcrude<-lm(Propane~CrudeOil,data=dfData)
  modelAll<-lm(Propane~CrudeOil+MonthVal,data=dfData)
  
  output$plot1 <- renderPlotly({
    plot_ly(dfData,x=~Period,y=~CrudeOil,name="Cushing WTI Spot",type = 'scatter',mode = 'lines')%>%
      add_trace(y=~Propane,name="Propane",mode='line')%>%
      add_lines(x = ~Period, y = fitted(modelcrude), name="Model1 Fitted",line = list(color = 'rgb(0,100,0)', width = 4, dash = 'dot'),opacity = 0.5)%>%
      add_lines(x = ~Period, y = fitted(modelAll), name="Model2 Fitted",line = list(color = 'rgb(255,0,0)', width = 4, dash = 'dot'),opacity = 0.5)
    
  })
  
  output$plot2 <- renderPlotly({
    monthInput<-input$iPeriodInput
    priceInput<-as.integer(input$iPriceInput)
    dfOutput=data.frame(ModelTypes=c("CrudeOil","CrudeOil+Month"),PredictedValues=c(predict(modelcrude,newdata=data.frame(CrudeOil=priceInput)),predict(modelAll,newdata=data.frame(CrudeOil=priceInput,MonthVal=monthInput))))
    
    plot_ly(dfOutput,x=~ModelTypes,y=~PredictedValues,type="bar")
  })
    
  
})
