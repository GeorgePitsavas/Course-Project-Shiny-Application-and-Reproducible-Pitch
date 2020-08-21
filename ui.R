# User interface for app that runs a prediction algorithm for determining sepal length of an iris based on the sepal width

# Sidebar components:
# - Radio buttons to select the Iris species (setosa, versicolor, virginica)
# - A slider to select the target sepal width
# - A submit button that transfers control to the server.R file

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Predicting Iris Petal Length"),
  
  sidebarPanel(
    radioButtons("Species", "Iris Species:",
                 list("setosa" = "setosa",
                      "versicolor" = "versicolor",
                      "virginica" = "virginica"
                 )),
    
    sliderInput("target", 
                "Target Petal Width (cm)", 
                value = 0.5,
                min = 0.1, 
                max = 2.5,
                step = 0.1),
    submitButton("Submit")
  ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot"),
               tags$h3("For the Iris species"),
               tags$ul(
                 tags$h3(textOutput("speciesSelect"))
               ),
               tags$h3("at a target Petal Width of"),
               tags$ul(
                 tags$h3(textOutput("sliderSelect"))
               ),
               tags$h3(" Predicted Petal Length "),
               tags$ul(
                 tags$h3(textOutput("pred"))
               )),
      tabPanel("Instructions", fluidPage(
        tags$h2("App Instructions"), 
        tags$p("This web application allows user to predict petal length of an iris
                         based on a user input.Iris data is available in R"), 
        tags$h3(" Settings at Start up"),
        tags$p("At startup the slider defaults to 0.5 and the Species defaults to setosa"),
        tags$h3("Set Iris Type"), 
        tags$p("The user can select the following Species by click the appropriate radio button:"),
        tags$ul(
          tags$li("setosa"),
          tags$li("versicolor"),
          tags$li("virginica")
        ),
        tags$h3("Set Target Petal "),
        tags$p("Using a mouse control, the target petal width can be set. The slider control moves in 0.1 cm increments"),
        tags$h3("Submit button"),
        tags$p("Click submit button to calculate petal length and update the plot "),
        tags$p("The process may be redone as many times as required."))
      )
      
    )
  )
))
