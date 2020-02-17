predictionMatch <- function(userInput, ngrams) {
     initialPrediction <-readRDS("./Next_Word_Prediction/start-word-prediction.RData")
  
    # trigram
    if (ngrams == 3) {
        userInput1 <- paste(userInput[length(userInput)-1], userInput[length(userInput)])
        dataTokens <- trigrams_freq %>% filter(token == userInput1)
        if (nrow(dataTokens) >= 1) {
            return(dataTokens$outcome[1:3])
        }
        return(predictionMatch(userInput, ngrams - 1))
    }
    
    # bigram (and lower)
    if (ngrams < 3) {
        userInput1 <- userInput[length(userInput)]
        dataTokens <- bigrams_freq %>% filter(token == userInput1)
        return(dataTokens$outcome[1:3])

    }
    
    # unigram: not implemented to enhance performance
    return(NA)
}

cleanInput <- function(input) {
    

    
    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- tolower(input)
    
    # remove URL, email addresses, Twitter handles and hash tags
    input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    
    # remove ordinal numbers
    input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
    
    # remove punctuation
    input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    
    # remove punctuation (leaving ')
    input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
    
    # trim leading and trailing whitespace
    input <- gsub("^\\s+|\\s+$", "", input)
    input <- stripWhitespace(input)
    
    
    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- unlist(strsplit(input, " "))
    
    return(input)
    
}

predictNextWord <- function(input, word = 0) {
    
    input <- cleanInput(input)
    
    if (input[1] == "") {
        output <- initialPrediction
    } else if (length(input) == 1) {
        output <- predictionMatch(input, ngrams = 2)
    } else if (length(input) >= 2) {
        output <- predictionMatch(input, ngrams = 3)
    }
    if (word == 0) {
        return(output)
    } else if (word == 1) {
        return(output[1])
    } else if (word == 2) {
        return(output[2])
    } else if (word == 3) {
        return(output[3])
    }
    
}

shinyServer(function(input, output) {
    
    # original sentence
    output$userSentence <- renderText({input$userInput});
    
    # intereactivity
    observe({
        numPredictions <- input$numPredictions
        if (numPredictions == 1) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- NULL
            output$prediction3 <- NULL
        } else if (numPredictions == 2) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
            output$prediction3 <- NULL
        } else if (numPredictions == 3) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
            output$prediction3 <- reactive({predictNextWord(input$userInput, 3)})
        }
    })
    
})