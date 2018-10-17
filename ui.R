
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
ui <- shinyUI(navbarPage(
  theme = shinytheme("superhero"),
  strong("Churn Analisys "),
  navbarMenu("Data",
             tabPanel("Raw Data",
                      mainPanel(
                        fluidRow(
                          h3(strong('Data Source')),
                          p('data:/www.ibm.com/communities/analitycs/warson-analityc-blog/',align='Justify'),
                          p('predictive-insight-in-the-telco-customer-churn-data-set',align='Justify'),
                          h3(strong('Table')),
                          dataTableOutput("churn")))),
             
             tabPanel("Data Preparation",
                      mainPanel(
                        fluidRow(
                          h3(strong('Code')),
                          p('sapply(churn, function(x) sum(is.na(x)))',align='Justify'),
                          p('churn <- churn[complete.cases(churn), ]',align='Justify'),
                          p('cols_recode1 <- c(10:15)',align='Justify'),
                          p('for(i in 1:ncol(churn[,cols_recode1])) {',align='Justify'),
                          p('churn[,cols_recode1][,i] <- as.factor(mapvalues',align='Justify'),
                          p('                                     (churn[,cols_recode1][,i],',align='Justify'),
                          p('                                             from =c("No internet service"),to=c("No")))}',
                            align='Justify'),
                          p('churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines,',align='Justify'),
                          p('                                           from=c("No phone service"),',align='Justify'),
                          p('                                           to=c("No")))',align='Justify'),
                          
                          
                          h3(strong('Table')),
                          dataTableOutput("churn1"))))),      
  
  navbarMenu("Exploratory Data Analisys",       
             tabPanel("Gender,Senior Citizen,Partner,Dependent",
                      
                      mainPanel(
                        fluidRow(
                          #h3('Graph 1'),
                          p('Gender,SeniorCtz,Partner,Dependent',
                            align='justify'),
                          
                          column(10,h4(strong('Graph')),plotOutput("Plot1")),
                          column(10,h4(strong('table1')),tableOutput("tb1"))
                          #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot2"))
                        )
                      )
             ),
             tabPanel("Phone Servise,Multiple Line,Internet Service,Online Security",
                      
                      mainPanel(
                        fluidRow(
                          #h3('Graph 2'),
                          p('PhoneServ,Multiline,Intservice,Olsqurity',align='Justify'),
                          
                          column(10,h4(strong('Graph')),plotOutput("Plot2")),
                          
                          column(10,h4(strong('table2')),tableOutput("tb2"))
                          #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot4"))
                        )
                      )
             ),
             tabPanel("Online Backup,Device Protection,Technical Support,Streaming Tv",
                     
                      mainPanel(
                        fluidRow(
                          #h3('Graph 3'),
                          p('Olbackup,Deviceprotect,TechSupport,StreamingTv',align='Justify'),
                          
                          column(10,h4(strong('Graph')),plotOutput("Plot3")),
                          
                          column(10,h4(strong('table3')),tableOutput("tb3"))
                          #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot4"))
                        )
                      )
             ),
             tabPanel("Streaming Tv,Contract,Paperless Billing,Payment Method,Group Tenure",
                      
                      mainPanel(
                        fluidRow(
                          #h3('Graph 4'),
                          p('StreamTv,Contract,PaperlessBill,Paymentmtd,Grouptenure',align='Justify'),
                          column(10,h4(strong('Graph')),plotOutput("Plot4")),
                          column(10,h4(strong('table4')),tableOutput("tb4"))
                          #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot6"))
                        )
                      )
             )),
  tabPanel("Model Logistic Regresi",
           
           mainPanel(
             fluidRow(
               h3('Fitting the Logistic Regression Model'),
               p('Analyzing the deviance table we can see the drop in deviance when adding 
                 each variable one at a time. Adding InternetService, Contract and tenure_group significantly
                 reduces the residual deviance. The other variables such as PaymentMethod and Dependents 
                 seem to improve the model less even though they all have low p-values.
                 (or predictors).',align='Justify'),
               #column(10,h4(strong('Model Regresi')),plotOutput("Plot5")),
               column(10,h3("Summary of Logistic Regression"),verbatimTextOutput("psum"),
                      
                      h3('Anova analisys'),
                      p('we can run the anova() function on the model to analyze 
                        the table of deviance',align='Justify'),
                      column(10,h3("Summary of Anova"),verbatimTextOutput("a1sum"),     
                             #column(10,h4(strong('confusion_matrix_log')),tableOutput("confusion_matrix_log")),
                             
                             column(10,h4(strong('Accuracy')),verbatimTextOutput("p_error")),
                             
                             column(10,h4('Odds Ratio'),
                                    p('One of the interesting performance measurements in logistic regression,
                                      is Odds Ratio.Basically, Odds ratio is what the odds of an event is happening',
                                      align='Justify'),
                                    
                                    column(10,h4(strong('Odds Ratio')),verbatimTextOutput("p_or"))
                                    #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("plot8"))
                                    )
                      )
                      )))),
  
  tabPanel("Model Decicion Tree",
           #sidebarLayout(
           # sidebarPanel(
           #    sliderInput("CI4",label="Confidence Interval",min=0.01,max=0.99,value=0.9)
           #  ),
           
           mainPanel(
             fluidRow(
               
               h3('Decision Tree visualization'),
               p('For illustration purpose, we are going to use only three variables 
                 for plotting Decision Trees, they are ?Contract?,
                 ?tenure_group? and ?PaperlessBilling?.
                 1. Out of three variables we use, Contract is the most important variable to predict customer churn or not churn.
                 2. If a customer in a one-year or two-year contract, no matter he (she) has PapelessBilling or not, 
                    he (she) is less likely to churn.
                 3. On the other hand, if a customer is in a month-to-month contract, and in the tenure group of 0?12 month,
                    and using PaperlessBilling,then this customer is more likely to churn.',align='Justify'),
               
               column(10,h4(strong('Tree diagram')),plotOutput("Plot6")),
               column(10,h3("Summary of Decicion tree"),verbatimTextOutput("ptree"),
                      column(10,h4(strong('Accuracy')),verbatimTextOutput("ptree_error"))
                      #column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot10"))
               )
               )
             )),
  
  tabPanel("Model Random Forest",
           #sidebarLayout(
           #  sidebarPanel(
           #    sliderInput("CI5",label="Confidence Interval",min=0.01,max=0.99,value=0.9)
           #  ),
           mainPanel(
             fluidRow(
               h3('Random forest model'),
               p('Accuracy : 0.7875          
                 ',align='Justify'),
               
               #column(10,h4(strong('Forecasting Plot')),plotOutput("Plot7")),
               column(10,h3("Summary of Random forrest"),verbatimTextOutput("psum_rf"),
                      column(10,h4(strong('Variable Important')),verbatimTextOutput("p_imp")),
                      column(10,h4(strong('Confusion Matrix and Statistics')),verbatimTextOutput("confusionMatrix_rf"))
               )
               )
           )),
  tabPanel("Conclusion",
           h3(strong("Conclusion")),
           br(),
           p("1. Features such as tenure_group, Contract, PaperlessBilling, MonthlyCharges and InternetService 
             appear to play a role in customer churn."),
           p("2. There does not seem to be a relationship between gender and churn."),
           p("3. Customers in a month-to-month contract, with PaperlessBilling and are within 12 months tenure,"),
           p("   are more likely to churn; On the other hand, customers with one or two year contract,"),
           p("   with longer than 12 months tenure, that are not using PaperlessBilling, are less likely to churn."),
           br(),
           
           p("Next, I will analize with the Keras and Tensorflow, cheer..Bambangpe")
           )
  
  ))