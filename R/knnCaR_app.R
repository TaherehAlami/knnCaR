library(shiny)
library(shinythemes) #themeSelector()
library(ggplot2)
library(knnCaR)
library(DT)
library(htmltools)

# fixed split function
make_split <- function(data, prop = 0.7) {
  n <- nrow(data)
  train_idx <- sample(n, size = floor(prop * n))
  list(train = data[train_idx, ], test = data[-train_idx, ])
}


ui <- tagList(
  themeSelector(),
  navbarPage(
    title = tags$b("k-NN Regression and Classification App"),
    inverse = TRUE,
    tabPanel(tags$b("Analysis of the model"),
             sidebarPanel(
               selectInput("dataset", "Select Dataset:",
                           choices = c("mtcars", "iris"), selected = "mtcars"),
               actionButton("loadData", "Load Dataset", class = "btn-primary"),
               h1(),
               uiOutput("response_select"),
               uiOutput("feature_select"),
               #hr(),
               sliderInput("k", "Number of neighbors (k):",
                           min = 1, max = 20, value = 5, step = 1),
               #hr(),
               #radioButtons("backend", "Compute backend:",
               #             choices = c("R","cpp"), selected = "R"),
               radioButtons("distmethod", "Distanse method:",
                            choices = c("sse","sad"), selected = "sse"),
               tags$b("New train/test split?" ),
               actionButton("resample", "Ok", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          h4(),
                          tags$b("Plot of actual vs predicted values on the test set"),
                          h4(),
                          plotOutput("predPlot", height = 450)),
                 tabPanel("Histogram",
                          h4(),
                          tags$b("Histogram of distances"),
                          h4(),
                          plotOutput("distPlot", height = 450)),
                 tabPanel("Performance",
                          h4(),
                          tags$b("Performance of the knn model"),
                          h4(),
                          verbatimTextOutput("trainPerf"),
                          verbatimTextOutput("testPerf")),
                # tabPanel("Table",
                #         tableOutput("predTable")),
                 tabPanel("Table",
                          h4(),
                          tags$b("Table of actual vs predicted values on the test set"),
                          h4(),
                          fluidRow(
                            column(12, DTOutput("predTable"))),
                          fluidRow(
                            column(12, downloadButton("downloadData",
                                                      "Download Table as CSV",
                                                      class = "btn-success")))
                  )
               )
             )
    ),
    tabPanel(tags$b("About the model"),
             fluidPage(
               h3("About the k-NN Model App"),
               p("This app allows you to train k-NN models for regression or classification,
                 adjust parameters, and explore model performance and predictions.")))))


server <- function(input, output) {
  dataset <- reactiveVal(mtcars)

  # Load dataset
  observeEvent(input$loadData, {
    if (input$dataset == "mtcars") {
      dataset(mtcars)
    } else if (input$dataset == "iris") {
      dataset(iris)
    }
  })

  # response selection
  output$response_select <- renderUI({
    req(dataset())
    feature_names <- names(dataset())
    selectInput("response", "Select Response variable:",
                choices = feature_names,
                #selected = feature_names[1],
                multiple = FALSE)
  })

  # feature selection
  output$feature_select <- renderUI({
    req(dataset())
    feature_names <- names(dataset())
    selectInput("features", "Select Explanatory Features:",
                choices = feature_names,
                multiple = TRUE)
  })

  # resample split
  split <- eventReactive(input$resample, {
    req(dataset())
    n <- nrow(dataset())
    train_idx <- sample(n, size = floor(0.7 * n))
    list(train = dataset()[train_idx, ], test = dataset()[-train_idx, ])
  }, ignoreNULL = FALSE)

  # Fit models
  knn_model <- reactive({
    req(input$features)
    d <- split()$train
    knn_s3(as.formula(paste(input$response, "~", paste(input$features, collapse = "+"))),
           data = d, k = input$k)
  })

  # Compute predictions for both models
  predicted_knn <- reactive({
    predict(knn_model(), newdata = split()$test, method = "R")
  })


  # Compute Error metrics
  mse <- function(obs, pred) mean((obs - pred)^2)
  r2  <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  misclass <- function(obs, pred) 1 - mean(pred == obs)

  output$trainPerf <- renderPrint({
    obs <- split()$train[[input$response]]
    fit <- fitted(knn_model())
    if(knn_model()$type == "regression"){
      cat("Training set:\n")
      cat("  MSE =", round(mse(obs, fit), 3),
          "  R² =", round(r2(obs, fit), 3), "\n")
    } else {
      cat("Training set:\n")
      cat(" Misclassification Rate: ", misclass(obs, fit), "\n")
    }
  })

  output$testPerf <- renderPrint({
    obs <- split()$test[[input$response]]
    if(knn_model()$type == "regression"){
      cat("Training set:\n")
      cat("  MSE =", round(mse(obs, predicted_knn()), 3),
          "  R² =", round(r2(obs, predicted_knn()), 3), "\n")
    } else {
      cat("Test set:\n")
      cat(" Misclassification=", misclass(obs, predicted_knn()), "\n")
    }
  })


  # Plot of actual vs predicted on test set
  output$predPlot <- renderPlot({
    plot(knn_model())
  })

  # Plot of distances
  output$distPlot <- renderPlot({
    plotdist.knn_s3(knn_model(), distmethod = input$distmethod)
  })

  table_df <- reactive({
    df <- split()$test
    res <- input$response
    if (knn_model()$type == "regression") {
      data.frame(
        Observation = rownames(df),
        Actual = round(df[[res]], 2),
        Predicted = round(predicted_knn(), 2)
      )
    } else {
      data.frame(
        Observation = rownames(df),
        Actual = df[[res]],
        Predicted = predicted_knn()
      )
    }
  })

  # Show table of actual vs predicted on test set
  output$predTable <- renderDT({
    datatable(table_df(), rownames = FALSE)
  })

  # download table
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("knn_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(table_df(), file, row.names = FALSE)
    }
  )

}

shiny::shinyApp(ui, server)


