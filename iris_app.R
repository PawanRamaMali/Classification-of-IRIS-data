library(shiny)
library(tidyverse)
library(boot)
library(cowplot)

MyData <- iris
MyRegData <- iris[,-5]

ui <- fluidPage(
  headerPanel('Iris Classification App'),
  sidebarPanel(
    selectInput(inputId='xdata', label='First Variable', choices=names(MyData),
                selected=names(MyData)[[1]]),
    selectInput(inputId='ydata', label='Second Variable', choices=names(MyData),
                selected = names(MyData)[[2]]),
    sliderInput(inputId='xinput', label="First Variable Value =", min=0, max=8, value=5, step=0.25),
    sliderInput(inputId='yinput', label="Second Variable Value =", min=0, max=8, value=5, step=0.25),
    sliderInput(inputId='knn_k', label="K for KNN", min=1, max=nrow(MyData), value=15)
  ),
  mainPanel(
    plotOutput('plot1'),
    textOutput('knnresult'),
    textOutput('ldaresult'),
    textOutput('qdaresult')
  ),
  
  # ADDED MATERIAL
  headerPanel('Iris Prediction App'),
  sidebarPanel(
    checkboxInput(inputId = "species", label = "Separate By Species Then Predict", value = FALSE),
    conditionalPanel(
      condition = "input.species == true",
      selectInput(inputId='speciesFact', label='Species', choices=c("setosa", "versicolor", "virginica"))),
    selectInput(inputId='response', label='Response Variable', choices=names(MyRegData),
                selected=names(MyRegData)[[1]]),
    conditionalPanel(
      condition = "input.response != 'Sepal.Length'",
      checkboxInput(inputId = "slength", label = "Sepal.Length", value = FALSE),
      conditionalPanel(
        condition = "input.slength == true",
        sliderInput(inputId='slengthValue', label="Sepal.Length Value =", min=0, max=8, value=5, step=0.25))),
    conditionalPanel(
      condition = "input.response != 'Sepal.Width'",
      checkboxInput(inputId = "swidth", label = "Sepal.Width", value = FALSE),
      conditionalPanel(
        condition = "input.swidth == true",
        sliderInput(inputId='swidthValue', label="Sepal.Width Value =", min=0, max=8, value=5, step=0.25))),
    conditionalPanel(
      condition = "input.response != 'Petal.Length'",
      checkboxInput(inputId = "plength", label = "Petal.Length", value = FALSE),
      conditionalPanel(
        condition = "input.plength == true",
        sliderInput(inputId='plengthValue', label="Petal.Length Value =", min=0, max=8, value=5, step=0.25))),
    conditionalPanel(
      condition = "input.response != 'Petal.Width'",
      checkboxInput(inputId = "pwidth", label = "Petal.Width", value = FALSE),
      conditionalPanel(
        condition = "input.pwidth == true",
        sliderInput(inputId='pwidthValue', label="Petal.Width Value =", min=0, max=8, value=5, step=0.25))),
    conditionalPanel(
      condition = "input.slength == true || input.swidth == true || input.plength == true || input.pwidth == true",
      actionButton("runModel", "Predict!")
    )
  ),
  
  mainPanel(
    h4("Step 1: Choose whether you want to predict a variable by subsetting the data by species."),
    h4("Step 2: Select Your Response Variable."),
    h4("Step 3: Select what predictors you want to include in the model."),
    h4("Step 4: Enter the data for the flower you wish to predict the response variable."),
    h4("Step 5: Predict! Check out the results below!"),
    tableOutput('mseTable')
  )
)

server <- function(input, output) {
  values <- reactiveValues()
  values$error.table <- data.frame(Response = character(), 
                                   SpeciesSpecific = character(),
                                   Prediction = numeric(), 
                                   Factors = character(), 
                                   RSquared = numeric(), 
                                   MSE = numeric())
  
  observeEvent( input$runModel, {
    library(caret)
    control <- trainControl(method="cv", number=10)
    
    Var1 <- as.integer(input$slengthValue)
    Var2 <- as.integer(input$swidthValue)
    Var3 <- as.numeric(input$plengthValue)
    Var4 <- as.numeric(input$pwidthValue)
    
    TestData <- cbind(Var1, Var2, Var3, Var4)
    TestData <- as.data.frame(TestData)
    TestData <- as.data.frame(TestData[,c(input$slength, input$swidth, input$plength, input$pwidth)])
    Tnames <- names(MyData)
    Tnames <- Tnames[c(input$slength, input$swidth, input$plength, input$pwidth, FALSE)]
    colnames(TestData)<-t(Tnames)
    
    if(input$species){ MyData <- MyData %>% filter(Species == input$speciesFact) }
    MyLMData <- as.data.frame(MyData[c(input$slength, input$swidth, input$plength, input$pwidth, FALSE)])
    MyLMData <- as.data.frame(cbind(MyLMData, MyData[,input$response])) #note: for some reason this hates mutate
    colnames(MyLMData)[ncol(MyLMData)] <- input$response
    
    # Factors <- names(MyLMData)
    Factors <- names(MyData)
    Factors <- Factors[c(input$slength, input$swidth, input$plength, input$pwidth, FALSE)]
    
    lmModel <- train(reformulate(as.vector(as.character(Factors)), response = input$response), data=MyLMData, method="lm", trControl=control)
    lmPredict <- predict(lmModel, TestData)
    
    MyLMData <- as.data.frame(MyData[c(input$slength, input$swidth, input$plength, input$pwidth, FALSE)])
    msePredict <- predict(lmModel$finalModel, MyLMData)
    
    SpeciesSpecific <- input$species
    Factors <- paste0(Factors, collapse = ", ")
    MSE <- sum((MyData[,input$response] - msePredict)^2)
    RSquared <- lmModel$results[,3]
    Response <- input$response
    Prediction <- as.numeric(as.character(lmPredict))
    this.row <- as.data.frame(cbind(Response, SpeciesSpecific, Prediction, Factors, RSquared, MSE))
    isolate(values$error.table <- rbind(values$error.table, this.row))
  })
  
  output$mseTable <- renderTable({values$error.table})
  
  output$plot1 <- renderPlot({
    ggplot() +
      geom_point(aes(x=MyData[,input$xdata], y=MyData[,input$ydata], color=MyData[,"Species"], shape=MyData[,"Species"]), size=3) + 
      geom_point(aes(x=as.numeric(input$xinput), y=as.numeric(input$yinput)), fill="black", size=4, shape=21) + 
      labs(x=input$xdata, y=input$ydata, title="Iris Data")
  })
  
  output$mseTable <- renderTable({values$error.table})

  output$knnresult <- renderText({
    library(class)
    TestData <- as.data.frame(cbind(input$xinput,input$yinput))
    names(TestData) <- c(input$xdata,input$ydata)
    Model <- knn(train=MyData[,c(input$xdata, input$ydata)],test=TestData,cl=MyData[,"Species"], k=input$knn_k)
    TestData <- TestData %>%
      mutate(pred=Model)
    paste("The expected class given by a KNN Classifiction with K=",input$knn_k," is ",TestData[1,3],sep=" ")
    })

  output$ldaresult <- renderText({
    library(MASS)
    ClassData <- MyData %>%
      dplyr::select(input$xdata, input$ydata, Species)
    Model <- lda(Species ~ ., data=ClassData)
    TestData <- as.data.frame(cbind(input$xinput,input$yinput))
    names(TestData) <- c(input$xdata,input$ydata)
    
    bob <- predict(Model, TestData)
    TestData <- TestData %>%
      mutate(pred=ifelse(bob[1]==1,"setosa",ifelse(bob[1]==2,"versicolor","virginica")))
    paste("The expected class given by an LDA Classifiction is ",TestData[1,3],sep=" ")
  })
  
  output$qdaresult <- renderText({
    library(MASS)
    ClassData <- MyData %>%
      dplyr::select(input$xdata, input$ydata, Species)
    Model <- qda(Species ~ ., data=ClassData)
    TestData <- as.data.frame(cbind(input$xinput,input$yinput))
    names(TestData) <- c(input$xdata,input$ydata)
    
    bob <- predict(Model, TestData)
    TestData <- TestData %>%
      mutate(pred=ifelse(bob[1]==1,"setosa",ifelse(bob[1]==2,"versicolor","virginica")))
    paste("The expected class given by an QDA Classifiction is ",TestData[1,3],sep=" ")
  })
  
  
}

shinyApp(ui = ui, server = server)
