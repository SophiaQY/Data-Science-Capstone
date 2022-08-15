#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tm)
library(NLP)
library(shinycssloaders)

gram2<- readRDS("gram2.RData")
gram3<- readRDS("gram3.RData")
gram4<- readRDS("gram4.RData")

predictWord <- function(sentence, ngram=2){
        inputSentence<-stripWhitespace(removeNumbers(tolower(sentence),preserve_intra_word_dashes = TRUE))
        #sentenceWords <- unlist(strsplit(inputSentence, split = " " ))
        sentenceWords<- strsplit(inputSentence," ")[[1]]
        qwords<-length(sentenceWords)
        ngram2='?'
        ngram3='?'
        ngram4='?'
        if(qwords > 0 & ngram == 2) { 
                lastWords <- tail(sentenceWords, 1)
                #indx <- head((gram2[gram2$w1==lastWords,]),1)
                ngram2<-as.character(head((gram2[gram2$w1==lastWords,])$w2,1))
                #freq<-as.integer(head((gram2[gram2$w1==lastWords,])$n,1))
                return(ngram2)
        }  
        
        if(qwords > 1 & ngram == 3) { 
                lastWords <- tail(sentenceWords, 1)
                lastWords_2 <- tail(sentenceWords, 2)
                ngram3<-as.character(head((gram3[gram3$w1==lastWords_2 
                                                 & gram3$w2==lastWords,])$w3,1))
                return(ngram3)
        }  
        
        if(qwords > 2 & ngram == 4) { 
                lastWords <- tail(sentenceWords, 1)
                lastWords_2 <- tail(sentenceWords, 2)
                lastWords_3 <- tail(sentenceWords, 3)
                ngram4<-as.character(head((gram4[gram4$w1==lastWords_3 & gram4$w2==lastWords_2
                                                 & gram4$w3==lastWords,])$w4,1))
                return(ngram4)
        }  
        
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$next_word <- renderText({
                word <- as.character(input$input_text)
                paste0(input$input_text," ",predictWord(word))
        })
        
        output$bigram <- renderText({
                word <- as.character(input$input_text)
                predictWord(word,2)
        })
        
        output$trigram <- renderText({
                word <- as.character(input$input_text)
                predictWord(word,3)
        })
        
        output$quagram <- renderText({
                word <- as.character(input$input_text)
                predictWord(word,4)
        })
        
})