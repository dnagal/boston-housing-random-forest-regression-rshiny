#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(randomForest)

#load saved objects from random forest r script
load("/Users/darrylnagal/Winter Break Project 2025/random_forest_winter_break_project.RData")

ui <- fluidPage(
  titlePanel("Boston Housing Price Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rm", "Average number of rooms:", min = 3.561, max = 8.78, value = 6.208),
      sliderInput("lstat", "Lower status population (%):", min = 1.73, max = 37.97, value = 11.38),
      sliderInput("dis", "Weighted distances to Boston employment centers:", min = 1.1296, max = 12.1265, value = 3.1827),
      sliderInput("crim", "Crime rate:", min = 0.00632, max = 88.9762, value = 0.26169),
      sliderInput("nox", "Nitrogen oxide concentration:", min = 0.385, max = 0.871, value = 0.538)
    ),
    
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  
  output$prediction <- renderText({
    req(input$rm, input$lstat, input$dis, input$crim, input$nox)
    
    newdata <- data.frame(
      rm = input$rm,
      lstat = input$lstat,
      dis = input$dis,
      crim = input$crim,
      nox = input$nox,
      zn = median(train_clean$zn, na.rm = TRUE),
      indus = median(train_clean$indus, na.rm = TRUE),
      chas = 0,
      age = median(train_clean$age, na.rm = TRUE),
      ptratio = median(train_clean$ptratio, na.rm = TRUE),
      rad = median(train_clean$rad, na.rm = TRUE),
      tax = median(train_clean$tax, na.rm = TRUE),
      b = median(train_clean$b, na.rm = TRUE)
    )
    
    prediction_result <- predict(rf_model, newdata)
    
    paste0("Estimated Property Value: $", round(prediction_result * 1000, 2))
  })
  
  output$varImportance <- renderPlot({
    varImpPlot(rf_model)
  })
}

shinyApp(ui = ui, server = server)