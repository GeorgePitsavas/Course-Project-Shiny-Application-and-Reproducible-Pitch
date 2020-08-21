# server.R for iris sepal length prediction app
# Input:
# 1) Species - radio button selection of the requested Iris species
# 2) target - the target sepal width for the prediction algorithm

# Output:
# 1) A plot with the following details:
#     a) All the data points for the requested Iris species (subset of the orginal Iris dataset)
#     b) A straight line that represents the linear prediction model
#     c) A green point to indicate the prediction for the input target
# 2) output$pred - prediction of sepal length using the target sepal width
# 3) output$speciesSelect - originally selected species so it can be printed in the output pane
# 4) output$sliderSelect - originally selected value of the slider (target)


library(shiny)

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    SpeciesSelect <- input$Species
    targetPetalWidth <- input$target
    
    irisSubset <- subset(iris, iris$Species == SpeciesSelect)
    
    model <- lm(Petal.Length ~ Petal.Width, data = irisSubset)
    
    modelPrediction <- reactive({
      predict(model, newdata = data.frame(Petal.Width = targetPetalWidth))}
    )
    
    output$pred <- renderText({modelPrediction()})
    output$speciesSelect <- renderText(SpeciesSelect)
    output$sliderSelect <- renderText(targetPetalWidth)
    
    plot(irisSubset$Petal.Width, irisSubset$Petal.Length, ylim = c(0, 10), xlim = c(0,5), xlab = "Petal Width (cm)", ylab = "Petal Length (cm)", col ="black")
    abline(model, col = "blue", lwd = 2)
    points(targetPetalWidth, modelPrediction(), col="red", pch = 16, cex =2)
    legend("topright", c("Data Points", "Linear Model", "User Requested Point"),title = "Legend",  text.col=c("black","blue","red"), title.col = "black")
  })
  
})