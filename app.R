
############# Load the necessary Library#############################

library(shiny)
library(shinyWidgets)
library(readr)
library(data.table)
library(DT)
library(nlme)

################### User interface page################################

ui <- fluidPage(
  titlePanel("Nonlinear Regression with Random Effect"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "filedata",
        label = "Upload Training Data (.csv)",
        multiple = FALSE,
        accept = c(".csv"),
        buttonLabel = "Choose...",
        placeholder = "No files selected yet"
      ),
      # Drop-down menu to select a column from the training data
      selectInput(
        inputId = "column_selector",
        label = "Select Region Column",
        choices = NULL,
        selected = NULL
      ),
      
      #Drop-down menu to select a factor level from the selected column
      selectInput(
        inputId = "factor_selector",
        label = "Select Region",
        choices = NULL,
        selected = NULL
     ),
      
     # Drop-down menu to select a column from the training data
     selectInput(
       inputId = "column_selector_2",
       label = "Select Species Column",
       choices = NULL,
       selected = NULL
     ),
     
     #Drop-down menu to select a factor level from the selected column
     selectInput(
       inputId = "factor_selector_2",
       label = "Select Species",
       choices = NULL,
       selected = NULL
     ),
      # Drop-down menu to select a factor level from the selected column

      
      ########### Upload file for test data######### 
      fileInput(
        inputId = "filedata1",
        label = "Upload Hieght to be Predicted (.csv)",
        multiple = FALSE,
        accept = c(".csv"),
        buttonLabel = "Choose...",
        placeholder = "No files selected yet"
      ),
     
     # New dropdown menus for selecting columns from Height to be Predicted Data
     selectizeInput(
       inputId = "predict_region_selector",
       label = "Select Region Column (Predict)",
       choices = NULL,
       selected = NULL,
       options = list(`style` = "btn-info")
     ),
     selectizeInput(
       inputId = "predict_species_selector",
       label = "Select Species Column (Predict)",
       choices = NULL,
       selected = NULL,
       options = list(`style` = "btn-info")
     ),
      
      ## Panel to show the output tabele#########
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Select Variables",
          uiOutput("xvariable"),
          uiOutput("yvariable"),
          uiOutput("revariable"),
          tabPanel(
            "Coefficients",
            sliderInput("b1", "b1", min = -100, max = 100, step = 0.01, value = 6.58172),
            sliderInput("b2", "b2", min = -100, max = 100, step = 0.01, value = -7.58311),
            sliderInput("b3", "b3", min = -100, max = 100, step = 0.01, value = -0.41703),
            downloadButton("coefficientsTableDownload", "Download Coefficients Table"),
            tableOutput("coefficientsTable")
          )
        )
      )
    ), #sidebarpanel
    
    mainPanel(
      div(),
      DTOutput("tb1"),
      fluidRow(column(6, verbatimTextOutput('lmSummary')), 
               column(6, plotOutput('diagnosticPlot')),
               downloadButton("download_data", "Download Predictions"),
               column(6, tableOutput('Predictions')))
    )
  ) #sidebarlayout
)


#################################### Server page ###############################################
server <- function(input, output) {
  
  # Set maximum upload size to 100MB
  options(shiny.maxRequestSize = 100*1024^2)
  
  ################### Read the data and subset it##########################
  data <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)) {
      return(NULL)
    }
    mydata <- read.csv(inData$datapath, header = TRUE, sep = ",")
    
    # Update the column_selector input with column names from the data
    choices <- colnames(mydata)
    selected <- choices[1]
    if (!is.null(input$column_selector)) {
      selected <- input$column_selector
    }
    updateSelectInput(
      inputId = "column_selector",
      label = "Select Region Column",
      choices = choices,
      selected = selected
    )
    
    # Update the factor_selector choices based on the selected column
    if (!is.null(input$column_selector)) {
      factor_choices <- unique(mydata[[input$column_selector]])
      factor_selected <- factor_choices[1]
      if (!is.null(input$factor_selector)) {
        factor_selected <- input$factor_selector
      }
      updateSelectInput(
        inputId = "factor_selector",
        label = "Select Region",
        choices = factor_choices,
        selected = factor_selected
      )
      
      # Subset the data based on the selected factor level
      if (input$factor_selector != "") {
        mydata <- subset(mydata, mydata[[input$column_selector]] %in% input$factor_selector)
      }
    }
    
    # Update the column_selector_2 input with column names from the data
    choices_2 <- colnames(mydata)
    selected_2 <- choices_2[1]
    if (!is.null(input$column_selector_2)) {
      selected_2 <- input$column_selector_2
    }
    updateSelectInput(
      inputId = "column_selector_2",
      label = "Select Species Column",
      choices = choices_2,
      selected = selected_2
    )
    
    # Update the factor_selector_2 choices based on the selected second column
    if (!is.null(input$column_selector_2)) {
      factor_choices_2 <- unique(mydata[[input$column_selector_2]])
      factor_selected_2 <- factor_choices_2[1]
      if (!is.null(input$factor_selector_2)) {
        factor_selected_2 <- input$factor_selector_2
      }
      updateSelectInput(
        inputId = "factor_selector_2",
        label = "Select Species",
        choices = factor_choices_2,
        selected = factor_selected_2
      )
      
      # Subset the data based on the selected second factor level
      if (input$factor_selector_2 != "") {
        mydata <- subset(mydata, mydata[[input$column_selector_2]] %in% input$factor_selector_2)
      }
    }
    
    return(mydata)
  })
  
  
  ################Varibales to be used 
  
  output$xvariable <- renderUI({
    req(data())
    xa <- colnames(data()) 
    pickerInput(inputId = 'xvar',
                label = 'Select x variable',
                choices = c(xa[1:length(xa)]), 
                selected = xa[1],
                options = list(`style` = "btn-info"))
  })
  
  output$yvariable <- renderUI({
    req(data())
    ya <- colnames(data()) 
    pickerInput(inputId = 'yvar',
                label = 'Select y variable',
                choices = c(ya[1:length(ya)]), 
                selected = ya[2],
                options = list(`style` = "btn-info"))
  })
  
  output$revariable <- renderUI({
    req(data())
    ra <- colnames(data()) 
    pickerInput(inputId = 'revar',
                label = 'Select random effect variable',
                choices = c(ra[1:length(ra)]), 
                selected = ra[3],
                options = list(`style` = "btn-info"))
  })
  
  
######################## Non-linear model build#################################  
  nlmeModel <- reactive({
    req(data(), input$xvar, input$yvar, input$revar, input$b1, input$b2, input$b3)
    x <- as.numeric(data()[[as.name(input$xvar)]])
    y <- as.numeric(data()[[as.name(input$yvar)]])
   re <- as.factor(round(data()[[as.name(input$revar)]], -2))
   if (length(x) == length(y) && length(y) == length(re)) {
     gd <- groupedData(y ~ x | re, data = data.frame(x = x, y = y, re = re))
      model <- nlme(y ~ 4.5 + exp((b1+u1)+ b2*(x^b3)),
                    data = gd,
                    fixed = b1 + b2 + b3 ~ 1,
                    random =u1~ 1 | re,
                    start = c(b1 = input$b1, b2 = input$b2, b3 = input$b3))
   } else {
      model <- NULL
    }
    
    return(model)
  })
  

  
  output$lmSummary <- renderPrint({
    req(nlmeModel())
    summary(nlmeModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    req(nlmeModel())
    plot(nlmeModel(), col = "blue")
  })
  
  output$coefficientsTable <- renderTable({
   req(nlmeModel())
    coef(nlmeModel())
  })
  
  output$coefficientsTableDownload <- downloadHandler(
    filename = function() {
      paste("coefficientsTable", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(coefficients(nlmeModel()), file, row.names = FALSE)
    }
  )
  
  output$Predictions <- renderTable({
    req(nlmeModel(), input$filedata1, input$xvar, input$revar)
    
 # Load test data
    test_data <- read.csv(input$filedata1$datapath, header = TRUE, sep = ",")
    
 # Make predictions using trained model
    x_test <- as.numeric(test_data[[as.name(input$xvar)]])
    re_test <- as.factor(round(test_data[[as.name(input$revar)]], -2))
    predictions <- predict(nlmeModel(), newdata = data.frame(x = x_test, re = re_test))
    
    # Combine predictions with test data
    test_data$predictions <- predictions
    
    # Return table with test data and predictions
    return(test_data)
  })
  
  # Download handler for test_data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Predictions", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Load test data and add predictions
      test_data <- read.csv(input$filedata1$datapath, header = TRUE, sep = ",")
      x_test <- as.numeric(test_data[[as.name(input$xvar)]])
      re_test <- as.factor(round(test_data[[as.name(input$revar)]], -2))
      predictions <- predict(nlmeModel(), newdata = data.frame(x = x_test, re = re_test))
      test_data$predictions <- predictions
      
      # Write data to CSV file
      write.csv(test_data, file, row.names = FALSE)
    }
  )
  
  
  
}

############## Run app ###############
shinyApp(ui, server) 