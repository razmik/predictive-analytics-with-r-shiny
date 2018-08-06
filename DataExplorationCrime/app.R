#
# This visualization tool can be used for data exploration and predictive analytics on the Communities 
# and Crime Unnormalized Data Set. The main purpose of this activity is to explore, clean, and pre-process 
# the data for Predictive Analytics task.
#
# Author: Rashmika Nawaratne
# Date: 05 - Aug - 2018
#

library(shiny)
library(DT)
library(ggplot2)
library(corrplot)
library(calibrate)
library(psych)
library(caTools)
library(shinythemes)


# Load main dataset
crime_dataset <- read.csv(file="CommViolPredUnnormalizedData.csv", header=TRUE, sep=",")

# Create new crime column
crime_dataset$crime <- crime_dataset$ViolentCrimesPerPop + crime_dataset$nonViolPerPop

# Filter only numerical variables
numeric_columns_global <- unlist(lapply(crime_dataset, is.numeric))
crime_dataset_numeric <- crime_dataset[,numeric_columns_global]


# Define UI for application that draws a histogram
# Theme: https://rstudio.github.io/shinythemes/
ui <- fluidPage(theme = shinytheme("yeti"),
                
  # Application title
  HTML("<div class='card text-white bg-primary mb-3'>
  <div class='card-header' style='padding:5px;'><h2>Predictive Analytics - Explore, clean, and pre-process the data</h2></div>
</div>"),
  titlePanel('', windowTitle ='Explore, clean, and pre-process the data'),
  
  fluidRow(
    column(12, HTML("<p>This visualization tool can be used for data exploration and predictive analytics on the Communities 
    and Crime Unnormalized Data Set. The main purpose of this activity is to explore, clean, and pre-process 
                 the data for Predictive Analytics task. More information about the dataset and its attributes can be 
                 found through <a href='http://archive.ics.uci.edu/ml/datasets/communities+and+crime' target='_blank'>
                 UCI Machine Learning Repository URL</a>.</p>"))
  ),
  
  hr(),
  
  # Dataset Table View

  fluidRow(
    column(12,
           h4("Communities and Crime dataset description")
    )
  ),
  
  fluidRow(
    column(12,
           DT::dataTableOutput("head_table")
    )
  ),
  
  br(),
  hr(),
  
  fluidRow(
    h2("Data Exploration")
  ),
  
  # Data Distriution
  h3("Explore Individual Attributes"),
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("distribution_selected", "Select Predictor to visualize Distribution:",
                  names(crime_dataset))
    ),
    mainPanel(
      h4("Explore the data distribution"),
      plotOutput("selected_distribution")
    )
  ),
  
  hr(),
  br(),
  
  # Histogram and Density Distriution - One Variable
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("histogram_selected", "Select Predictor to visualize histogram:",
                  names(crime_dataset_numeric))
    ),
    mainPanel(
      h4("Explore the Histogram and Density Distribution of data"),
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
  
  # Boxplot diagrams
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("boxplot_selected", "Select Predictor to visualize boxplot:",
                  names(crime_dataset_numeric))
    ),
    mainPanel(
      h4("Explore statistics of attributes"),
      fluidRow(
        column(6, plotOutput("selected_boxplot_plot")),
        column(6, verbatimTextOutput("boxplot_summary_stats"))
      )
    )
  ),
  
  br(),
  hr(),
  
  # Histogram Distriution - Two Variables
  h3("Explore Multiple Attributes with their Relations"),
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("histogram_two_variable_selected_1", "Select Predictor 1 For comparison:",
                  names(crime_dataset_numeric)),
      selectInput("histogram_two_variable_selected_2", "Select Predictor 2 For comparison:",
                  names(crime_dataset_numeric))
    ),
    mainPanel(
      h4("Explore multivariable correlation"),
      fluidRow(
        column(4, plotOutput("selected_histogram_two_variable_comparison")),
        column(4, plotOutput("selected_histogram_two_variable_comparison_log")),
        column(4, plotOutput("selected_histogram_two_variable_comparison_sqrt"))
      )
    )
  ),
  
  br(),
  hr(),
  
  # Relationship Visualization - Two Variables
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("rel_viz_two_variable_selected_1", "Select Predictor 1 For visualization:",
                  names(crime_dataset_numeric)),
      selectInput("rel_viz_two_variable_selected_2", "Select Predictor 2 For visualization:",
                  names(crime_dataset_numeric))
    ),
    mainPanel(
      h4("Explore multivariable correlation - Referenced"),
      fluidRow(
        column(4, plotOutput("selected_rel_viz_two_variable_comparison")),
        column(4, plotOutput("selected_rel_viz_two_variable_comparison_log")),
        column(4, plotOutput("selected_rel_viz_two_variable_comparison_sqrt"))
      )
    )
  ),
  
  br(),
  hr(),
  
  
  # Correlation Plots
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("corr_selected_predictors", "Select Predictors to explore correlationship:",
                  names(crime_dataset_numeric), multiple = TRUE)
    ),
    mainPanel(
      h4("Explore correlation among attributes"),
      fluidRow(
        column(12, plotOutput("correlation_plot"))
      ),
      fluidRow(
        column(12, plotOutput("pairwise_correlation_plot"))
      )
    )
  ),
  
  br(),
  hr(),
  
  # Handle Missing Values
  h3("Data Cleaning: Handling Missing Values"),
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      selectInput("missing_val_col_selected", "Select a Predictor to replace missing values:",
                  names(crime_dataset_numeric)),
      selectInput("miss_val_clean_type", "Replace Type:",
                  c("Replace with Zero" = "zero_empty",
                    "Delete records" = "del",
                    "Replace with mean" = "mean"))
    ),
    mainPanel(
      fluidRow(
        column(7, plotOutput("missing_val_distribution_plot")),
        column(5, verbatimTextOutput("missing_val_col_mean"))
      )
    )
  ),
  
  br(),
  hr(),
  
  # Predictive Analytics
  h3("Predictive Analytics"),
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(
      numericInput("prediction_seed", "Seed:", 2,
                   min = 1, max = 100),
      sliderInput(inputId = "train_data_perc",
                  label = "Training Data Percentage:",
                  min = 1,
                  max = 100,
                  value = 70),
      selectInput("target_variable", "Target Variable:",
                  names(crime_dataset_numeric), selected = names(crime_dataset_numeric)[130]),
      selectInput("target_predictors", "Select Predictors:",
                  names(crime_dataset_numeric), multiple = TRUE, selected = names(crime_dataset_numeric)[128])
    ),
    mainPanel(
      fluidRow(
        column(12, h3("Linear Regression - Model"))
      ),
      fluidRow(
        column(6,
               plotOutput("linear_regression_plot")
        ),
        column(6,
               verbatimTextOutput("lr_model_summary")
        )
      )
    )
  ),
  
  hr(),
  
  HTML("<div style='margin-left: 25%;'>Copyrights &copy; 2018 <a href='http://latrobe.edu.au/cdac' target='_blank'>Research Centre for Data Analytics & Cognition</a>, La Trobe Business School.</div>"),
  br()

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$head_table = DT::renderDataTable({
    crime_dataset
  })
  
  #Distribution of data
  output$selected_distribution <- renderPlot({
    if (!is.null(input$distribution_selected)){
      distribution_selected_inner <- crime_dataset[,input$distribution_selected]
    }else{
      distribution_selected_inner <- crime_dataset[,c("ViolentCrimesPerPop")]
    }
    plot(distribution_selected_inner, main=paste("Distribution of ", input$distribution_selected), xlab=input$distribution_selected)
  })
  
  #Histogram of data
  histogram_data <- reactive({
    # filter the dataset based on selected variables
    if (!is.null(input$histogram_selected)){
      histogram_selected_inner <- crime_dataset[,input$histogram_selected]
    }else{
      histogram_selected_inner <- crime_dataset[,c("mvalue")]
    }
  })
  
  output$selected_histogram <- renderPlot({
    # draw the histogram plot
    hist(histogram_data(), main=paste("Histogram for ", input$histogram_selected), xlab=input$histogram_selected)
  })
  
  output$selected_density <- renderPlot({
    # draw the density plot
    plot(density(histogram_data(), na.rm = TRUE), main=paste("Density of ", input$histogram_selected))
  })
  
  
  
  
  # Boxplots
  output$selected_boxplot_plot <- renderPlot({
    if (!is.null(input$boxplot_selected)){
      data <- crime_dataset[,input$boxplot_selected]
    }else{
      data <- crime_dataset[,c("ViolentCrimesPerPop")]
    }
    boxplot(data, main=paste("Boxplot of ", input$boxplot_selected), xlab=input$boxplot_selected)
  })
  
  #Summary
  output$boxplot_summary_stats <- renderPrint({
    
    data_row = crime_dataset[,input$boxplot_selected]
    
    print("Summary Statistics")
    
    summary(data_row)
    
  })
  
  #Comparison of two variables
  
  output$selected_histogram_two_variable_comparison <- renderPlot({

    target_x <- crime_dataset[,input$histogram_two_variable_selected_1]
    target_y <- crime_dataset[,input$histogram_two_variable_selected_2]
    title <- paste(input$histogram_two_variable_selected_1, input$histogram_two_variable_selected_2, sep=" vs ")
    
    
    plot(x=target_x, y=target_y, col="blue", main=title, xlab=input$histogram_two_variable_selected_1, 
         ylab=input$histogram_two_variable_selected_2)
  })
  
  output$selected_histogram_two_variable_comparison_log <- renderPlot({
    
    target_x <- crime_dataset[,input$histogram_two_variable_selected_1]
    target_y <- crime_dataset[,input$histogram_two_variable_selected_2]
    title <- paste("LOG: ", paste(input$histogram_two_variable_selected_1, input$histogram_two_variable_selected_2, sep=" vs "), sep = " ")
    
    
    plot(x=log(target_x), y=log(target_y), col="blue", main=title, xlab=paste("LOG:", input$histogram_two_variable_selected_1, sep = " "), 
         ylab=paste("LOG:", input$histogram_two_variable_selected_2, sep = " "))
  })
  
  output$selected_histogram_two_variable_comparison_sqrt <- renderPlot({
    
    target_x <- crime_dataset[,input$histogram_two_variable_selected_1]
    target_y <- crime_dataset[,input$histogram_two_variable_selected_2]
    title <- paste("SQRT: ", paste(input$histogram_two_variable_selected_1, input$histogram_two_variable_selected_2, sep=" vs "), sep = " ")
    
    
    plot(x=sqrt(target_x), y=sqrt(target_y), col="blue", main=title, xlab=paste("SQRT:", input$histogram_two_variable_selected_1, sep = " "), 
         ylab=paste("SQRT:", input$histogram_two_variable_selected_2, sep = " "))
  })
  
  #Relationship Visualization of two variables
  
  output$selected_rel_viz_two_variable_comparison <- renderPlot({
    target_x <- crime_dataset[,input$rel_viz_two_variable_selected_1]
    target_y <- crime_dataset[,input$rel_viz_two_variable_selected_2]
    title <- paste(input$rel_viz_two_variable_selected_1, input$rel_viz_two_variable_selected_2, sep=" vs ")
    
    plot(x=(target_x), y=(target_y), col="red", main=title, type="n", 
         xlab=input$rel_viz_two_variable_selected_1, ylab=input$rel_viz_two_variable_selected_2)
    
    segments(x0=(target_x), y0=(target_y), x1=(target_x), 
             y1=rep((mean(target_y, na.rm = TRUE)), length(target_y)), 
             col=rgb(0,0,1,0.3))
    
    points(x=(target_x), y=(target_y), col=rgb(0,0,1,0.1), pch=20, cex=1)
    abline(h=(mean(target_y, na.rm = TRUE)), col="orange")
    
    target_y.highest <- match(max(target_y, na.rm = TRUE), target_y)
    
    textxy((target_x[target_y.highest]), (target_y[target_y.highest]), 
           crime_dataset$communityname[target_y.highest], col="red", cex=0.7)
  })
  
  output$selected_rel_viz_two_variable_comparison_log <- renderPlot({
    target_x <- crime_dataset[,input$rel_viz_two_variable_selected_1]
    target_y <- crime_dataset[,input$rel_viz_two_variable_selected_2]
    title <- paste("LOG: ", paste(input$rel_viz_two_variable_selected_1, 
                                  input$rel_viz_two_variable_selected_2, sep=" vs "), sep = " ")
    
    plot(x=log(target_x), y=log(target_y), col="red", main=title, type="n", 
         xlab=paste("LOG:", input$rel_viz_two_variable_selected_1, sep = " "), 
         ylab=paste("LOG:", input$rel_viz_two_variable_selected_2, sep = " "))
    
    segments(x0=log(target_x), y0=log(target_y), x1=log(target_x), 
             y1=rep(log(mean(target_y, na.rm = TRUE)), length(target_y)), 
             col=rgb(0,0,1,0.3))
    
    points(x=log(target_x), y=log(target_y), col=rgb(0,0,1,0.1), pch=20, cex=1)
    abline(h=log(mean(target_y, na.rm = TRUE)), col="orange")
    
    target_y.highest <- match(max(target_y, na.rm = TRUE), target_y)
    
    textxy(log(target_x[target_y.highest]), log(target_y[target_y.highest]), 
           crime_dataset$communityname[target_y.highest], col="red", cex=0.7)
  })
  
  output$selected_rel_viz_two_variable_comparison_sqrt <- renderPlot({
    target_x <- crime_dataset[,input$rel_viz_two_variable_selected_1]
    target_y <- crime_dataset[,input$rel_viz_two_variable_selected_2]
    title <- paste("SQRT: ", paste(input$rel_viz_two_variable_selected_1, 
                                  input$rel_viz_two_variable_selected_2, sep=" vs "), sep = " ")
    
    plot(x=sqrt(target_x), y=sqrt(target_y), col="red", main=title, type="n", 
         xlab=paste("SQRT:", input$rel_viz_two_variable_selected_1, sep = " "), 
         ylab=paste("SQRT:", input$rel_viz_two_variable_selected_2, sep = " "))
    
    segments(x0=sqrt(target_x), y0=sqrt(target_y), x1=sqrt(target_x), 
             y1=rep(sqrt(mean(target_y, na.rm = TRUE)), length(target_y)), 
             col=rgb(0,0,1,0.3))
    
    points(x=sqrt(target_x), y=sqrt(target_y), col=rgb(0,0,1,0.1), pch=20, cex=1)
    abline(h=sqrt(mean(target_y, na.rm = TRUE)), col="orange")
    
    target_y.highest <- match(max(target_y, na.rm = TRUE), target_y)
    
    textxy(sqrt(target_x[target_y.highest]), sqrt(target_y[target_y.highest]), 
           crime_dataset$communityname[target_y.highest], col="red", cex=0.7)
  })
  
  
  # Correlations
  
  correlations_data <- reactive({
    # filter the dataset based on selected variables
    if (!is.null(input$corr_selected_predictors) && length(input$corr_selected_predictors) > 1){
      data_sel <- crime_dataset[,input$corr_selected_predictors]
    }else{
      default_predictors <- c("crime", "PctUnemployed", "PctPopUnderPov")
      data_sel <- crime_dataset[,default_predictors]
    }
  })
  
  output$correlation_plot <- renderPlot({
    # draw the correlation plot
    corrplot(cor(correlations_data()))
  })
  
  output$pairwise_correlation_plot <- renderPlot({
    # draw the pairwise correlation plot
    pairs.panels(correlations_data(), col="red")
  })
  
  # Handling missing values
  
  cleaned_data <- reactive({
    
    cd <- crime_dataset[,]
    selected <- input$missing_val_col_selected
    
    if(input$miss_val_clean_type == "zero_empty"){
      commands = c("cd$", selected, "[is.na(cd$", selected, ")] <- 0")
    }else if(input$miss_val_clean_type == "del"){
      commands = c("cd <- cd[complete.cases(cd), ]")
    }else if(input$miss_val_clean_type == "mean"){
      commands = c("cd$", selected, "[is.na(cd$", selected, ")] <- mean(cd$", selected, ", na.rm = TRUE)")
    }
    
    eval(parse(text=paste(commands, collapse='')))
    cd[, selected]
    
  })
  
  # Show the mean
  output$missing_val_col_mean <- renderPrint({
    
    if(input$miss_val_clean_type == "zero_empty"){
      print(paste("Summary of Zero Replacement: ", input$missing_val_col_selected))
    }else if(input$miss_val_clean_type == "del"){
      print(paste("Summary of NA Deleted: ", input$missing_val_col_selected))
    }else if(input$miss_val_clean_type == "mean"){
      print(paste("Summary of Mean Replacement: ", input$missing_val_col_selected))
    }
    
    summary(cleaned_data())
    
  })
  
  # OUtput comparison plot
  output$missing_val_distribution_plot <- renderPlot({
    title <- paste(input$missing_val_col_selected, " original (Blue) vs Transformed (Red)")
    plot(density(cleaned_data()), col="red", main=title) 
    lines(density(crime_dataset[, input$missing_val_col_selected], na.rm = TRUE), col="blue")
  })
  
  #Predictive Analytics
  lr_model <- reactive({
    set.seed(input$prediction_seed)
    split <- sample.split(crime_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(crime_dataset,split==TRUE)
    test <- subset(crime_dataset,split==FALSE)
    
    lm(as.formula(paste(input$target_variable," ~ ", paste(input$target_predictors, collapse="+"))), data = train)
  })
  
  output$linear_regression_plot <- renderPlot({
    layout(matrix(c(1,2,3,4),2,2))
    plot(lr_model())
  })
  
  output$lr_model_summary <- renderPrint({
    summary(lr_model())
  })
  
  test_data <- reactive({
    set.seed(input$prediction_seed)
    split <- sample.split(crime_dataset,SplitRatio = (input$train_data_perc/100))
    train <- subset(crime_dataset,split==TRUE)
    subset(crime_dataset,split==FALSE)
  })
  
  predicted_target <- reactive({
    test <- test_data()
    test$target <- test[,input$target_variable]
    test$predicted.target <- predict(lr_model(),test_data())
    test
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

