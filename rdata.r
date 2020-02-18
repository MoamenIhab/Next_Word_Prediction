library(stringi)
library(tm)
library(SnowballC)
library(RWeka)
library(ggplot2)
library(wordcloud)
set.seed(2727)
ftwitter <- "~/WorkSpace/final/en_US/en_US.twitter.txt"
fblog <- "~/WorkSpace/final/en_US/en_US.blogs.txt"
fnews <- "~/WorkSpace/final/en_US/en_US.news.txt"
ftwitter <- readLines(ftwitter, skipNul = TRUE, encoding = "UTF-8")
fblog <- readLines(fblog, skipNul = TRUE, encoding = "UTF-8")
fnews <-readLines(fnews, skipNul = TRUE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(sampleData))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
#Cleaning all non ASCII characters
corpus <- tm_map(corpus,toSpace,"[^[:graph:]]")
#Transforming all data to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
#Deleting all English stopwords and any stray letters left by the non-ASCII removal
corpus <- tm_map(corpus,removeWords,c(stopwords("english"),letters))
#Removing Punctuation
corpus <- tm_map(corpus,removePunctuation)
#Removing Numbers
corpus <- tm_map(corpus,removeNumbers)
#Striping extra whitespace
corpus <- tm_map(corpus,stripWhitespace)
unigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
unigrams <- DocumentTermMatrix(corpus, control = list(tokenize = unigramTokenizer))
freqTerms <- findFreqTerms(unigrams,lowfreq = 10000)
unigrams_freq <- sort(colSums(as.matrix(unigrams[,freqTerms])),decreasing = TRUE)
unigrams_freq <- data.frame(word = names(unigrams_freq), frequency = unigrams_freq)
BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
freqTerms <- findFreqTerms(bigrams,lowfreq = 1000)
bigrams_freq <- sort(colSums(as.matrix(bigrams[,freqTerms])),decreasing = TRUE)
bigrams_freq <- data.frame(word = names(bigrams_freq), frequency = bigrams_freq)
TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
trigrams <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
freqTerms <- findFreqTerms(trigrams,lowfreq = 100)
trigrams_freq <- sort(colSums(as.matrix(trigrams[,freqTerms])),decreasing = TRUE)
trigrams_freq <- data.frame(word = names(trigrams_freq), frequency = trigrams_freq)
wordcloud(unigrams_freq$word, unigrams_freq$frequency,scale = c(3,.1),rot.per = 0.30, colors = brewer.pal(5, "Blues"))
redictionMatch <- function(userInput, ngrams) {
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
