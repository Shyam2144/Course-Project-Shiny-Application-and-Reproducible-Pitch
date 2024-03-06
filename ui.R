library(shiny)
library(HistData)
library(dplyr)
library(ggplot2)
library(shinythemes) # For the futuristic theme

# 1st step: pass inches to cm
gf <- GaltonFamilies
gf <- gf %>% mutate(father = father * 2.54,
                    mother = mother * 2.54,
                    childHeight = childHeight * 2.54)

# linear model
model1 <- lm(childHeight ~ father + mother + gender, data = gf)

# Function to generate plot
generatePlot <- function(father, mother, gender) {
  kid <- ifelse(gender == "female", "Daughter", "Son")
  df <- data.frame(father = father,
                   mother = mother,
                   gender = factor(gender, levels = levels(gf$gender)))
  ch <- predict(model1, newdata = df)
  yvals <- c("Father", kid, "Mother")
  df <- data.frame(x = factor(yvals, levels = yvals, ordered = TRUE),
                   y = c(father, ch, mother))
  ggplot(df, aes(x = x, y = y, color = c("red", "green", "blue"), fill = c("red", "green", "blue"))) +
    geom_bar(stat = "identity", width = 0.5) +
    xlab("") +
    ylab("Height (cm)") +
    theme_minimal() +
    theme(legend.position = "none")
}

shinyServer(function(input, output) {
  output$pText <- renderText({
    paste("Father's height is",
          strong(round(input$inFh, 1)),
          "cm, and mother's height is",
          strong(round(input$inMh, 1)),
          "cm, then:")
  })
  output$pred <- renderText({
    df <- data.frame(father = input$inFh,
                     mother = input$inMh,
                     gender = factor(input$inGen, levels = levels(gf$gender)))
    ch <- predict(model1, newdata = df)
    kid <- ifelse(
      input$inGen == "female",
      "Daughter",
      "Son"
    )
    paste0(em(strong(kid)),
           "'s predicted height is going to be around ",
           em(strong(round(ch))),
           " cm"
    )
  })
  output$Plot <- renderPlot({
    generatePlot(input$inFh, input$inMh, input$inGen)
  })
})

# UI enhancements
shinyUI(fluidPage(
  theme = shinytheme("cyborg"),  # Futuristic theme
  
  titlePanel("Prediction of Height"),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #222; color: #ccc;", # Darker sidebar 
      helpText("Prediction of the child's height considering gender and parent's height"),
      helpText("Parameters:"),
      sliderInput(inputId = "inFh",
                  label = "Father's height (cm):",
                  value = 150,
                  min = 150,
                  max = 220,
                  step = 1),
      sliderInput(inputId = "inMh",
                  label = "Mother's height (cm):",
                  value = 140,
                  min = 140,
                  max = 200,
                  step = 1),
      radioButtons(inputId = "inGen",
                   label = "Child's gender: ",
                   choices = c("Female" = "female", "Male" = "male"),
                   inline = TRUE)
    ),
    
    mainPanel(
      style = "background-color: #333; color: #ddd;", # Dark main area
      h3("Summary", style = "color: cyan;"), # Accent color
      htmlOutput("pText"),
      htmlOutput("pred"),
      plotOutput("Plot", width = "50%")
    )
  )
))