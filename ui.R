#Coding bibliography
#Solving the issue with the shiny req observe loop when using actionButton
#https://stackoverflow.com/questions/34583259/actionbutton-reset-needed-or-alternative?rq=1


#how to keep the uiButtons in the same row (inline):
#https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny


library(shiny)
library(stringi)

shinyUI(
  fluidPage(
    titlePanel("Word Prediction: Type your message here..."),
    
    sidebarLayout(
      sidebarPanel(
        div(style="vertical-align:bottom; horizontal-align:center;",uiOutput("input_textarea", width='700px')),
        div(style="display: inline-block;vertical-align:top;",uiOutput("but1")),
        div(style="display: inline-block;vertical-align:top;",uiOutput("but2")),
        div(style="display: inline-block;vertical-align:top;",uiOutput("but3")),
        div(style="display: inline-block;vertical-align:top;",uiOutput("but4")),
        div(style="display: inline-block;vertical-align:top;",uiOutput("but5")),
        width=4),
      mainPanel(
      )
    )
  )
)
