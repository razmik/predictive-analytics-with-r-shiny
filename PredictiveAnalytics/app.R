#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(Amelia)
library(ggplot2)
library(corrplot)
library(caTools)
library(dplyr)
library(plotly)

boston_dataset <- read.csv(file="BostonHousingReduced.csv", header=TRUE, sep=",")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel('Predictive Analytics - La Trobe'),
  
  br(),
  hr(),
  
  fluidRow(
    column(6, offset = 1, uiOutput("introduction_html"))
  ),
  
  hr(),

   fluidRow(
     column(3,
            fluidRow(
              column(12,
                     h4("Boston Housing dataset description")
              )
            ),
            fluidRow(
              column(12,
                     uiOutput("variable_list")
              )
            )
     ),
     column(9,
            fluidRow(
              column(12,
                     ""
              )
            ),
            fluidRow(
              column(12,
                     DT::dataTableOutput("head_table")
              )
            )
     )
   ),
  
  br(),
  hr(),
   
   fluidRow(
     h1("Data Exploration")
   ),
   
   fluidRow(
     column(2,
            selectInput("selected_predictors", "Select Predictors to explore correlationship:",
                        names(boston_dataset), multiple = TRUE)
     ),
     column(10,
            fluidRow(
              column(6,
                     plotOutput("correlation_plot")
              ),
              column(6,
                     plotOutput("pairwise_correlation_plot")
              )
            )
     )
   ),
  
  hr(),
   
   fluidRow(
     column(2,
            selectInput("histogram_selected", "Select Predictor to visualize histogram:",
                        names(boston_dataset))
     ),
     column(10,
            fluidRow(
              column(12,
                     ""
              )
            ),
            fluidRow(
              column(6,
                     plotOutput("selected_histogram")
              ),
              column(6,
                     plotOutput("selected_density")
              )
            )
     )
   ),
  
  br(),
  hr(),
  
   fluidRow(
     h1("Predictive Analytics")
   ),
  
  br(),
   
   fluidRow(
     column(2,
            fluidRow(
              sliderInput(inputId = "train_data_perc",
                          label = "Training Data Percentage:",
                          min = 1,
                          max = 100,
                          value = 70)
            ),
            fluidRow(
              selectInput("target_variable", "Target Variable:",
                          names(boston_dataset), selected = names(boston_dataset)[13])
            ),
            fluidRow(
              selectInput("target_predictors", "Select Predictors:",
                          names(boston_dataset), multiple = TRUE, selected = names(boston_dataset)[12])
            ),
            fluidRow(
              numericInput("prediction_seed", "Seed:", 2,
                           min = 1, max = 100)
            )
     ),
     column(10,
            fluidRow(
              column(12,
                     h3("Linear Regression - Model")
              )
            ),
            fluidRow(
              column(6,
                     plotOutput("linear_regression_plot")
              ),
              column(6,
                     verbatimTextOutput("lr_model_summary")
              )
            ),
            
            hr(),
            
            fluidRow(
              column(12,
                     h3("Linear Regression - Evaluation")
              )
            ),
            fluidRow(
              column(6, 
                     plotlyOutput("linear_regression_test_plotly")
              ),
              column(6, 
                     verbatimTextOutput("lr_model_test_summary")
              )
            )
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output the introductoin
  output$introduction_html <- renderUI(HTML(
    "<p>This tool can be used for data exploration and predictive analytics on the Boston Housing dataset.</p>"
  ))
  
  # output the variable list
  output$variable_list <- renderUI(HTML("<ul>
                                <li>CRIM - per capita crime rate by town</li>
                                 <li>ZN - proportion of residential land zoned for lots over 25,000 sq.ft. </li>
                                  <li>INDUS - proportion of non-retail business acres per town.</li>
                                  <li>CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)</li>
                                  <li>NOX - nitric oxides concentration (parts per 10 million).</li>
                                  <li>ROOMS - average number of rooms per dwelling.</li>
                                  <li>AGE - proportion of owner-occupied units built prior to 1940.</li>
                                  <li>DISTANCE - weighted distances to five Boston employment centres.</li>
                                  <li>RADIAL - index of accessibility to radial highways.</li>
                                  <li>TAX - full-value property-tax rate per $10,000.</li>
                                  <li>PTRATIO - pupil-teacher ratio by town.</li>
                                  <li>LSTAT - % lower status of the population.</li>
                                  <li>MVALUE - Median value of owner-occupied homes in $1000's.</li>
                                 </ul>"))
  
  
  output$head_table = DT::renderDataTable({
    boston_dataset
  })
  
  correlations_data <- reactive({
    # filter the dataset based on selected variables
    if (!is.null(input$selected_predictors) && length(input$selected_predictors) > 1){
      boston_dataset_selected <- boston_dataset[,input$selected_predictors]
    }else{
      boston_dataset_selected <- boston_dataset
    }
  })
  
  output$correlation_plot <- renderPlot({
    # draw the correlation plot
    corrplot(cor(correlations_data()))
  })
  
  output$pairwise_correlation_plot <- renderPlot({
    # draw the pairwise correlation plot
    pairs(correlations_data(), main = "Boston Housing Attribute correlation")
    
  })
  
  histogram_data <- reactive({
    # filter the dataset based on selected variables
    if (!is.null(input$histogram_selected)){
      histogram_selected_inner <- boston_dataset[,input$histogram_selected]
    }else{
      histogram_selected_inner <- boston_dataset[,c("mvalue")]
    }
  })
  
  output$selected_histogram <- renderPlot({
    # draw the histogram plot
    hist(histogram_data(), main=paste("Histogram for ", input$histogram_selected), xlab=input$histogram_selected)
  })
  
  output$selected_density <- renderPlot({
    # draw the density plot
    plot(density(histogram_data()), main=paste("Density of ", input$histogram_selected))
  })
  
  lr_model <- reactive({
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    test <- subset(boston_dataset,split==FALSE)
    
    lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
  })

  output$linear_regression_plot <- renderPlot({
    "
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    test <- subset(boston_dataset,split==FALSE)
    
    model <- lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
    "
    
    layout(matrix(c(1,2,3,4),2,2))
    plot(lr_model())
    #with(train,plot(mvalue, crim))
    #abline(model)
  })
  
  output$lr_model_summary <- renderPrint({
    "
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    test <- subset(boston_dataset,split==FALSE)
    
    model <- lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
    "
    summary(lr_model())
  })
  
  test_data <- reactive({
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    subset(boston_dataset,split==FALSE)
  })
  
  predicted_target <- reactive({
    test <- test_data()
    test$target <- test[,input$target_variable]
    test$predicted.target <- predict(lr_model(),test_data())
    test
  })
  
  output$linear_regression_test_plotly <- renderPlotly({
    "
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    test <- subset(boston_dataset,split==FALSE)
    "
    #model <- lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
    
    #test$predicted.mvalue <- predict(lr_model(),test)
    
    #test <- test_data()
    #test$target <- test[,input$target_variable]
    #test$predicted.target <- predict(lr_model(),test_data())
    
    pl1 <-predicted_target() %>% 
      ggplot(aes(target,predicted.target)) +
      geom_point(alpha=0.5) + 
      stat_smooth(aes(colour='black')) +
      xlab('Actual value of the target') +
      ylab('Predicted value of target')+
      theme_bw()
    
    print(ggplotly(pl1))
    
  })
  
  output$lr_model_test_summary <- renderPrint({
    "
    set.seed(input$prediction_seed)
    split <- sample.split(boston_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(boston_dataset,split==TRUE)
    test <- subset(boston_dataset,split==FALSE)
    
    model <- lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
    "
    
    test <- predicted_target()
    error <- test$target - test$predicted.target
    rmse <- sqrt(mean(error)^2)
    
    print(paste("Root Mean Square Error: ", rmse))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

