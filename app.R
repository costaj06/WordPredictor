#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Typing Assistant"),
  
  # Sidebar with a textbox input for receiving freeform text 
  # sidebarLayout(
  #    sidebarPanel(
  #print out session info
#  verbatimTextOutput(outputId = "session_info"),
h3('Try it out!  Demo the new Typing Assistant widget that you can insert in your own app!'),
h4('Enter text in the box below and click on word suggestion buttons to add words to your text.'),

div(style="display:inline-block",textInput(inputId = "phrase", label = "Enter text", "Enter your text here")),
div(style="display:inline-block",uiOutput("ResetButton")),
#textInput(inputId = "phrase", label = "Enter text", "Enter your text here"), 
  
  #    mainPanel(
#  h3('The text you entered is:'),
#  textOutput(outputId = "inputphrase"),
  h4('Click on a word to append it to your input.'),
  h5('(Note: Buttons are ordered Most Probable Word on left to Less Probable Word on Right)'),
  textOutput(outputId = "predictedWord"),
  div(style="display:inline-block",uiOutput("button1")),
  div(style="display:inline-block",uiOutput("button2")),
  div(style="display:inline-block",uiOutput("button3")),
  div(style="display:inline-block",uiOutput("button4")),
  div(style="display:inline-block",uiOutput("button5"))

)


monograms <- readRDS("monograms")
bigrams <- readRDS("bigrams")
trigrams <- readRDS("trigrams")
quadgrams <- readRDS("quadgrams")

# predictNextWord takes as an input a string of words, the monograms, bigrams, trigrams, and quadgrams dataframes, and returns?
predictNextWord <- function(inputstring, monograms, bigrams, trigrams, quadgrams) {
  #inputstring is strings of words - split into first, second, etc
  inputstring <- as.character(inputstring)
  tokenedWords <- strsplit(inputstring, " ")
  n <- length(tokenedWords[[1]])
  lastword <- tokenedWords[[1]][n]
  secondlastword <- tokenedWords[[1]][n-1]
  thirdlastword <- tokenedWords[[1]][n-2]
  
  if (n>=3) {
    #create a temporary dataframe containing potential matching quadgrams
    qgdf <- quadgrams[quadgrams$firstword==tokenedWords[[1]][n-2] & quadgrams$secondword==tokenedWords[[1]][n-1] & quadgrams$thirdword==tokenedWords[[1]][n], ]
    #for each matching case (i.e., each row), calculate probabilites of next word: 
    # P = quadgramCount / sum of counts of all potential matching quadrams
    qgdf$P <- qgdf$quadgramCount / sum(qgdf$quadgramCount)
    #generate weighted probability - highest weight because highest level of apriori information about query string
    qgW <- 0.8
    qgdf$WP <- qgW * qgdf$P
    #sort in descending order of probability
    qgdf <- qgdf[order(-qgdf$P), ]
  }
  
  if (n>=2) {
    #create a temporary dataframe containing potential matching trigrams
    tgdf <- trigrams[trigrams$firstword==tokenedWords[[1]][n-1] & trigrams$secondword==tokenedWords[[1]][n], ]
    #for each matching case (i.e., each row), calculate probabilites of next word: 
    # P = trigramCount / sum of counts of all potential matching trigrams
    tgdf$P <- tgdf$trigramCount / sum(tgdf$trigramCount)
    #generate weighted probability - relatively higher weight because relatively more apriori information about query string
    tgW <- 0.6
    tgdf$WP <- tgW * tgdf$P
    #sort in descending order of probability
    tgdf <- tgdf[order(-tgdf$P), ]
  }
  if (n>=1) {
    #create a temporary dataframe containing potential matching bigrams
    bgdf <- bigrams[bigrams$firstword==tokenedWords[[1]][n], ]
    #for each matching case (i.e., each row), calculate probabilites of next word: 
    # P = bigramCount / sum of counts of all potential matching birams
    bgdf$P <- bgdf$bigramCount / sum(bgdf$bigramCount)
    #generate weighted probability - lower weight because less apriori information about query string
    bgW <- 0.4
    bgdf$WP <- bgW * bgdf$P
    #sort in descending order of probability
    bgdf <- bgdf[order(-bgdf$P), ]
  }
  
  if (n>=0) {
    #create a temporary dataframe containing potential matching monograms
    mgdf <- monograms[1:10, ]
    #for each matching case (i.e., each row), calculate probabilites of next word: 
    # P = monogramCount / sum of counts of all potential matching birams
    mgdf$P <- mgdf$monogramCount / sum(mgdf$monogramCount)
    #generate weighted probability - lower weight because least amount of prior information about query string
    mgW <- 0.2
    mgdf$WP <- mgW * mgdf$P
    #sort in descending order of weighted probability
    mgdf <- mgdf[order(-mgdf$P), ]
  }
  
  
  #Extract top 10 most likely candidates from each temporary quad-, tri-, bi- and monogram dataframe
  #if (n==0){candidates <- mgdf[1:10,mgdf$]}
  mgvars <- c("WP", "firstword")
  mgdf <-mgdf[mgvars]
  names(mgdf) <- c("WP", "candidate")
  
  
  candidates <- mgdf[1:10,]
  if (n>=1) {
    bgvars <- c("WP", "secondword")
    bgdf <-bgdf[bgvars]
    names(bgdf) <- c("WP", "candidate")
    candidates <- rbind(candidates, bgdf[1:10,])
  }  
  if (n>=2) {
    tgvars <- c("WP", "thirdword")
    tgdf <-tgdf[tgvars]
    names(tgdf) <- c("WP", "candidate")
    candidates <- rbind(candidates, tgdf[1:10,])
  } 
  if (n>=3) {
    qgvars <- c("WP", "fourthword")
    qgdf <-qgdf[qgvars]
    names(qgdf) <- c("WP", "candidate")
    candidates <- rbind(candidates, qgdf[1:10,])
  }
  #Sort final candidates by weighted probabilities
  candidates <- candidates[order(-candidates$WP), ]
  
  #get rid of duplicates
  candidates <- candidates[, 2]
  candidates <- unique(candidates)
  #return most likely candidate (highest weighted probability)
  return(candidates)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    
    
  #make dynamic reset button
  output$ResetButton <- renderUI({
    actionButton("B0", label = "Clear Input")
  })
  #make dynamic button 1
  output$button1 <- renderUI({
    button1_label <- predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[1]
    actionButton("B1", label = button1_label)
  })
  #make dynamic button 2
  output$button2 <- renderUI({
    button2_label <- predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[2]
    actionButton("B2", label = button2_label)
  })
  #make dynamic button 3
  output$button3 <- renderUI({
    button3_label <- predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[3]
    actionButton("B3", label = button3_label)
  })
  #make dynamic button 4
  output$button4 <- renderUI({
    button4_label <- predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[4]
    actionButton("B4", label = button4_label)
  })
  #make dynamic button 5
  output$button5 <- renderUI({
    button5_label <- predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[5]
    actionButton("B5", label = button5_label)
  })


  observeEvent(input$B0, {
    updateTextInput(session, "phrase", value = "")
  })
  observeEvent(input$B1, {
    currentInput <- as.character(input$phrase)
    button1_word <- as.character(predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[1])
    updateTextInput(session, "phrase", value = paste(currentInput, button1_word))
  })
  observeEvent(input$B2, {
    currentInput <- as.character(input$phrase)
    button2_word <- as.character(predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[2])
    updateTextInput(session, "phrase", value = paste(currentInput, button2_word))
  })
  observeEvent(input$B3, {
    currentInput <- as.character(input$phrase)
    button3_word <- as.character(predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[3])
    updateTextInput(session, "phrase", value = paste(currentInput, button3_word))
  })
  observeEvent(input$B4, {
    currentInput <- as.character(input$phrase)
    button4_word <- as.character(predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[4])
    updateTextInput(session, "phrase", value = paste(currentInput, button4_word))
  })
  observeEvent(input$B5, {
    currentInput <- as.character(input$phrase)
    button5_word <- as.character(predictNextWord(input$phrase, monograms, bigrams, trigrams, quadgrams)[5])
    updateTextInput(session, "phrase", value = paste(currentInput, button5_word))
  })

  })
}

shinyApp(ui = ui, server = server)
