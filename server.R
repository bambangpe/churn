

library(shiny)
library(shinythemes)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(dplyr)
library(shiny)
churn <- read.csv("./data/Telco-Customer-Churn.csv")
churn
source("./keras/global.r")
server <- shinyServer(function(input,output,session){
  output$churn1 <- renderDataTable({
    churn1}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  output$churn <- renderDataTable({
    churn}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  output$Plot1<-renderPlot({
    plot(pg1)
  })
  output$Plot2<-renderPlot({
    plot(pg2)
  })
  output$Plot3 <-renderPlot({
    plot(pg3)
  })
  output$Plot4 <-renderPlot({
    plot(pg4)
  })
  # output$Plot5<-renderPlot({
  #   plot(model)
  # }) 
  output$Plot6<-renderPlot({
    plot(tree, type='simple')
  })
  output$Plot7<-renderPlot({
    plot(rfmodel)
  })
  
  ##output$tb1 <- renderTable({
  ##  tb1},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  ##output$tb2 <- renderTable({
  ## tb2},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  ##output$tb3 <- renderTable({
  ##  tb3},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  ##output$tb4 <- renderTable({
  ##  tb4},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  ####
  #column(10,h4(strong('Model Regresi')),plotOutput("Plot5")),
  #column(10,h3("Summary(logmodel)"),verbatimTextOutput("ps")),
  #column(10,h4(strong('confusion_matrix_log')),tableOutput("confusion_matrix_log")),
  #column(10,h4(strong('Accuracy')),verbatimTextOutput("p_error"))
  ####  
  output$psum <- renderPrint({
    psum
  })
  output$a1sum <- renderPrint({
    a1sum
  })
  output$p_or <- renderPrint({
    p_or
  })
  
  output$ptree <- renderPrint({
    ptree
  })
  output$psum_rf <- renderPrint({
    psum_rf
  })
  
  output$confusion_matrix_log <- renderTable({
    confusion_matrix_log},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  output$p_error <- renderPrint({
    p_error
  })
  
  output$ptree_error <- renderPrint({
    ptree_error
  })
  
  output$confusion_matrix_tree <- renderTable({
    confusion_matrix_tree},options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  output$p_imp <- renderPrint({
    p_imp})
  
  output$confusionMatrix_rf <- renderPrint({
    confusionMatrix_rf})
  #output$table<-renderDataTable({
  # MyData()
  
})