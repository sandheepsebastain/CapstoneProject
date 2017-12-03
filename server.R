library(shiny)
library(tidyverse)
library(plotly)
library(gridExtra)
library(wordcloud)
require(RColorBrewer)
pal <- brewer.pal(8,"Dark2")

load("./ProcessedData/dfUnigramPredModel.Rda")
load("./ProcessedData/dfBigramPredModel.Rda")
load("./ProcessedData/dfTrigramPredModel.Rda")

predictBigram<-function(sentence){
  bigramdiscount=0.5
  if(sentence %in% dfBigramPredModel$Word1){
    unigramCount <- filter(dfUnigramPredModel, PredWord==sentence)$Frequency[1]
    if (is.na(unigramCount)){
      unigramCount<- sum(filter(dfBigramPredModel, Word1==sentence)$Frequency)
    }
    dfFoundWords<-dfBigramPredModel %>%
      filter(Word1 %in% sentence)%>%
      mutate(Probability=((Frequency - bigramdiscount) / unigramCount))
    return(dfFoundWords)
  }
  else{
    unigramCount<- sum(dfUnigramPredModel$Frequency)
    
    return(dfUnigramPredModel %>%
             mutate(Probability=((Frequency) / unigramCount)))
  }
}

predictTrigram<-function(sentence){
  trigramdiscount=0.5
  w1=strsplit(sentence," ")[[1]][1]
  w2=strsplit(sentence," ")[[1]][2]
  if(sentence %in% do.call(paste, c(dfTrigramPredModel[c("Word1", "Word2")], collapse = " "))){
    bigramCount <- filter(dfBigramPredModel, rownames(dfBigramPredModel)==sentence)$Frequency[1]
    dfFoundWords<-dfTrigramPredModel %>%
      filter(Word1 %in% w1 & Word2 %in% w2)%>%
      mutate(Probability=((Frequency - trigramdiscount) / bigramCount))
    return(dfFoundWords)
  }
  else{
    test1<-predictBigram(w2)
    return(test1)
  }
}


predictnextword<-function(sentence,n){
  formatsentence<-trimws(sentence)
  formatsentence<-tolower(formatsentence)
  nwords<-sapply(strsplit(formatsentence, " "), length)
  if (formatsentence==""){
    unigramCount<- sum(dfUnigramPredModel$Frequency)
    prediction = dfUnigramPredModel %>%
      mutate(Probability=((Frequency) / unigramCount)) %>%  
      top_n(n)
  }
  else if(nwords==1){
    prediction<-predictBigram(formatsentence)%>%
      top_n(n)
  }
  else if(nwords==2){
    prediction<-predictTrigram(formatsentence)%>%
      top_n(n)
  }
  else{
    newsentence<-paste(sapply(strsplit(formatsentence, " "), tail, 2),collapse = " ")
    prediction<-predictTrigram(newsentence)%>%
      top_n(n)
  }
  return(head(prediction,n))
}

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    predWord<-predictnextword(input$txtSentenceInput,as.numeric(input$iNumber))
    topWord<-predictnextword(input$txtSentenceInput,50)
    return(list(predWord,topWord))
  })
  
  output$table <- renderTable({
    predWord<-dataInput()[[1]]
    formatpredword<-predWord[,"PredWord", drop=FALSE]
    return(formatpredword)
    })
  output$barplot <- renderPlot({
    d <- dataInput()[[1]]
    d <- d[with(d,order(-Probability)),] 
    print(d)
    p<-ggplot(data=d, aes(x=PredWord, y=Probability)) +
      geom_bar(stat="identity",fill="steelblue")+
      coord_flip()+
      theme_minimal()+
      ggtitle("Predicted Word Probability Graph")+
      theme(plot.title = element_text(hjust = 0.5,face="bold", size=15))
    p
  })
  output$wordcloudplot <- renderPlot({
    d <- dataInput()[[2]]
    text(x=0.5, y=0.5, "Word Cloud")
    w<-wordcloud(d$PredWord,d$Frequency,max.words=Inf,random.order = FALSE,colors = pal)
    w
  })
  
  
    
  
})
