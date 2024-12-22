---
  title: "Python Project"
author: "Srinivas"
date: "2024-04-20"
output: html_document
---
  
  ```{r}
library(readxl)
library(dplyr)
library(caret)
library(shiny)
library(shinydashboard)
library(caret)

# Load the data
airbnb_data <- read_excel("~/Downloads/df3file.xlsx")
# Convert price from character to numeric
airbnb_data$price <- as.numeric(gsub("[$,]", "", airbnb_data$price))

# Selecting and preprocessing features for the model
features <- airbnb_data %>%
  select(neighbourhood, property_type, room_type, accommodates, bathrooms_count, beds) %>%
  mutate(across(where(is.character), as.factor))

# Creating dummy variables for features
dummies <- dummyVars(~ ., data = features)
features_transformed <- predict(dummies, newdata = features)
features_transformed <- data.frame(features_transformed, price = airbnb_data$price)

# Split the data
set.seed(123)
training_rows <- createDataPartition(features_transformed$price, p = 0.8, list = TRUE)$Resample1
train_data <- features_transformed[training_rows, ]
test_data <- features_transformed[-training_rows, ]

# Train the linear regression model
model <- lm(price ~ ., data = train_data)

# Save the model and the dummy variable setup
saveRDS(model, "airbnb_price_model.rds")
saveRDS(dummies, "airbnb_dummy_model.rds")


# Load the model and dummy variable setup
model <- readRDS("airbnb_price_model.rds")
dummies <- readRDS("airbnb_dummy_model.rds")

# UI Setup
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Price Prediction"),
  dashboardSidebar(
    selectInput("neighbourhood", "Choose a neighbourhood", choices = levels(features$neighbourhood)),
    selectInput("property_type", "Property Type", choices = levels(features$property_type)),
    selectInput("room_type", "Room Type", choices = levels(features$room_type)),
    numericInput("accommodates", "Accommodates", value = 1, min = 1, max = 15),
    numericInput("bathrooms_count", "Number of Bathrooms", value = 1, min = 0, max = 5),
    numericInput("beds", "Number of Beds", value = 1, min = 1, max = 10),
    actionButton("predict", "Predict Price")
  ),
  dashboardBody(
    textOutput("predicted_price")
  )
)

# Server logic for prediction
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      neighbourhood = input$neighbourhood,
      property_type = input$property_type,
      room_type = input$room_type,
      accommodates = input$accommodates,
      bathrooms_count = input$bathrooms_count,
      beds = input$beds
    )
    
    # Apply dummy encoding to new data
    new_data_transformed <- predict(dummies, new_data)
    new_data_transformed <- data.frame(new_data_transformed)
    
    # Predict using the model
    predicted_price <- predict(model, new_data_transformed)
    
    # Display the predicted price
    output$predicted_price <- renderText({
      paste("The predicted Airbnb price is $", round(predicted_price, 2))
    })
  })
}

# Run the Shiny application
shinyApp(ui, server)



```