#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Coursera Data Science Capstone: Words Prediction"),
        
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        helpText("Enter words"),
                        textInput("input_text", "Sentence:",value = "We"),
                       
                        h5('Instructions'),
                        helpText("This application makes the prediction of next word based on input words."),
                        helpText("The word prediction algorithm uses natural language processing to build a model 
                  using N-grams which are continuous sequences of words in a document to predict the next words."), 
                        helpText("The corpus is build from text selected from blogs, news, and twitters provided by Swifkey.")
                        ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        h1("Next Word Prediction"),
                        #textOutput('next_word'),
                        verbatimTextOutput("Guessing..."),
                        h3(strong(code(textOutput('next_word')))),
                        br(),
                        br(),
                        h4(tags$b('Bi-gram:')),
                        textOutput('bigram'),
                        br(),
                        h4(tags$b('Tri-gram:')),
                        textOutput('trigram'),
                        br(),
                        h4(tags$b('Quad-gram:')),
                        textOutput('quagram'),
                )
        )
))