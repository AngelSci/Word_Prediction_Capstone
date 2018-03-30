#credit to https://stackoverflow.com/questions/34583259/actionbutton-reset-needed-or-alternative?rq=1 for
#solving the issue with the shiny req observe loop when using actionButton

library(shiny)
library(stringi)
library(tidytext)
library(dplyr)

#init - set working directory
#setwd('C:/Users/AngelSparkles/Documents/Coursera/10 - Data Science Capstone/final')
newWD <- getwd()

#get the data used to predict words
load(paste0(newWD,"/unigram_probs.RData"))
load(paste0(newWD,"/bigram_probs.RData"))
load(paste0(newWD,"/trigram_probs.RData"))
load(paste0(newWD,"/normalized_prob.RData"))

#Flag test to see if the input is a new word or not
isNewWord_f <- function(x) {
  if (x == ''){x <- TRUE} else {x <- substr(x, nchar(x), nchar(x)) == " " }
}

#Function to add the word selected by one of the buttons to the text box
modify_text_input <- function(input_text, isNewWord, word_to_remove, selected_word) {
  if (isNewWord == TRUE) {
    #only add the new word to the input
    out <-paste0(input_text, selected_word,' ')
    return(out)
  } else {
    #Remove the partial word and replace with the new word
    #The selected word replaces the partial word in the text box
    out <- paste0(stri_replace_last_regex(input_text, word_to_remove, selected_word), " ")
    return(out)
  }
}

textToList <- function(input_text){
  phrase <- gsub("\\(?[0-9/:/;,./?/!]+\\)?"," ",input_text) 
  #remove non-alphanumeric characters that are not part of a word.
  phrase <- gsub("([ ][^a-zA-Z \'][ ])"," ",phrase)
  #remove the 's from the end of a word
  phrase <- gsub("('s )"," ",phrase)
  #remove any extra spaces
  phrase <- gsub("[ ]{2,11}"," ",phrase)
  #split the phrase into a list of words
  words <- lapply(strsplit(phrase,' '), tolower)
  return(words)
}

howManyWords <- function(input_text){
  if (is.null(input_text)) { return(0)} else {
    words <- textToList(input_text)
    numWords <- length(words[[1]])
    return(numWords)}
}

lookupSkipwords <- function(Rterms){
  #print(Rterms)
  matches <- normalized_prob[(normalized_prob$word1 %in% Rterms) | (normalized_prob$word2 %in% Rterms),]
  #the words we need can be either in word1 or word2
  #eliminates records where the words are the same
  matches <- matches[matches$word1 != matches$word2,]
  #m1 is just the word1 matches
  m1 <- select(filter(matches, !word1 %in% Rterms),'word2','n')
  colnames(m1) = c('word','n')
  #m2 is just the word2 matches
  m2 <- select(filter(matches, !word2 %in% Rterms),'word2','n')
  colnames(m2) = c('word','n')
  #combine the m1 and m2 matches into a single dataframe, M
  M <- rbind(m1,m2)[ order(-rbind(m1,m2)$n), ] 
  #remove and words that are one of our terms
  skipgrams <- filter(M, !word %in% Rterms)
  #remove and words that are stopwords
  skipgrams<- filter(skipgrams, !word %in% stop_words$word)
  #remove the duplicate records
  skipgrams <- skipgrams[!duplicated(skipgrams$word),]
  #convert the n values to a probability score
  skipgrams$p <- skipgrams$n/sum(skipgrams$n)
  #snip out only the data we need
  skipgrams <- select(skipgrams,'word','p')
  colnames(skipgrams) <- c('word','pS')
  #print(arrange(skipgrams,desc(pS)))
  #combine the skipgram data to the ngram dataframe
  return(skipgrams)
}

getWordChoices <- function(input_text,isNewWord){
  if (is.null(input_text)) { return(word<- as.data.frame(c('a','b','c','d','e')))} else {
    numWords <- howManyWords(input_text)
    if (isNewWord==TRUE) {
      #we need to guess the new word based on the previous words
      #how many words does the phrase have?
      if (numWords >= 2) {
        #The phrase has at least two words
        words <- textToList(input_text)

        #make a ngram variable of just the last two words
        first = numWords-1
        if (first < 0) {first=0}
        ngram <- words[[1]][first:numWords]
        #print(ngram)
        
        #Find best Trigram matches
        tri <- select(trigram_probs[((trigram_probs$word1==ngram[1])&(trigram_probs$word2==ngram[2])),],word3,p)
        onerecord <- sum(tri$p)/dim(tri)[[1]]
        tri$score <- (tri$p+onerecord)/(sum(tri$p)+onerecord)
        tri <- select(tri,word3,score)
        colnames(tri) <- c('word','p3')

        #Find the best Bigram matches
        bi <- select(bigram_probs[bigram_probs$word1==ngram[2],],word2,p)
        onerecord <- sum(bi$p)/dim(bi)[[1]]
        bi$score <- (bi$p+onerecord)/(sum(bi$p)+onerecord)
        bi <- select(bi,word2,score)
        colnames(bi) <- c('word','p2')
        
        #Find the most common unigrams
        unigram <- select(unigram_probs, word, p)
        
        #join the unigram, bigram, and trigram matches into a single dataframe, ngram_join
        unibi_join <- left_join(unigram,bi,by='word')
        ngram_join <- left_join(unibi_join,tri,by='word')

        #Fill in the NA values of the bigrams with a probability of one extra bigram * unigram probability
        ngram_join$p2[is.na(ngram_join$p2)] <- (1/dim(bigram_probs)[[1]])*ngram_join$p[is.na(ngram_join$p2)]
        
        #Fill in the NA values with a probability of one extra trigram * bigram probability
        ngram_join$p3[is.na(ngram_join$p3)] <- (1/dim(trigram_probs)[[1]])*ngram_join$p2[is.na(ngram_join$p3)]
        
        #Use a list of terms in the text to find skipgram matches
        Rterms <- unique(as.character(as.data.frame(words)[!as.data.frame(words)[,1] %in% stop_words$word,]))
        #if we have any terms listed in Rterms...
        if (is.null(Rterms)==FALSE & length(Rterms) > 0) { 
          skipgrams <- lookupSkipwords(Rterms)
          ngram_join <- left_join(ngram_join,skipgrams,by='word')
    
          #Fill in the NA values of the skipgrams with a probability of one / the number of normalized skipgrams records
          ngram_join$pS[is.na(ngram_join$pS)] <- (1/dim(normalized_prob)[[1]])
          
          #compute a score that multiplies the original score with half the skipgrams' probability 
          #so that the trigram is still weighted higher
          ngram_join$score <- (ngram_join$p3)*(ngram_join$pS/2)#+ngram_join$p3+ngram_join$p)/3
          #order the results from highest score to lowest
          ngram_join <- arrange(ngram_join,desc(score))
          
        }else{
          #no terms, so now skipgrams - score and return just the trigram bigram unigram scores
          ngram_join$score <- (ngram_join$p3)
          #order the results from highest score to lowest
          ngram_join <- arrange(ngram_join,desc(score))}
        return(ngram_join)}
      
      else if (numWords == 1) { #so far only one words has been entered into the text
        #get a list of words
        words <- textToList(input_text)
        ngram <- words[[1]][numWords]
        #find the best matching bigram
        bi <- select(bigram_probs[bigram_probs$word1==ngram[1],],word2,p)
        #prep the unigram match dataframe
        unigram <- select(unigram_probs, word, p)
        colnames(bi) <- c('word','p2')
        #join the bigram and unigram dataframes
        ngram_join <- left_join(unigram,bi,by='word')
        #Fill in the NA values of the bigrams with a probability of one extra bigram * unigram probability
        ngram_join$p2[is.na(ngram_join$p2)] <- (1/dim(bigram_probs)[[1]])*ngram_join$p[is.na(ngram_join$p2)]
        #score 
        ngram_join$score <- (ngram_join$p2)
        arrange(ngram_join,desc(score))
        return(ngram_join)}
      else {
        #there are no words yet.
        return(arrange(unigram_probs,desc(p) ) ) }
    }
    else{
      #These are partial words. We have some clue as to what the intended word will be
      if (numWords >= 3) { 
        words <- textToList(input_text)
        #two full words, one partial word
        #make a ngram variable of just the last three words
        first = numWords-2
        if (first < 0) {first=0}
        ngram <- words[[1]][first:numWords]
        print(ngram)
      
        #Find best Trigram matches
        tri <- select(trigram_probs[((trigram_probs$word1==ngram[1])&(trigram_probs$word2==ngram[2])),],word3,p)
        #tri <- tri[grep(paste0("^",ngram[3]),tri),]
        tri <- tri[grep(paste0('^',ngram[3]),tri$word3),]
        print(tri$word3)
        onerecord <- sum(tri$p)/dim(tri)[[1]]
        tri$score <- (tri$p+onerecord)/(sum(tri$p)+onerecord)
        tri <- select(tri,word3,score)
        colnames(tri) <- c('word','p3')
        
        #Find the best Bigram matches
        bi <- select(bigram_probs[bigram_probs$word1==ngram[2],],word2,p)
        bi <- bi[grep(paste0('^',ngram[3]),bi$word2),]
        onerecord <- sum(bi$p)/dim(bi)[[1]]
        bi$score <- (bi$p+onerecord)/(sum(bi$p)+onerecord)
        bi <- select(bi,word2,score)
        colnames(bi) <- c('word','p2')
        
        #Find the most common unigrams
        unigram <- select(unigram_probs, word, p)
        unigram <- unigram[grep(paste0('^',ngram[3]),unigram$word),]

        #join the unigram, bigram, and trigram matches into a single dataframe, ngram_join
        unibi_join <- left_join(unigram,bi,by='word')
        ngram_join <- left_join(unibi_join,tri,by='word')
        
        #Fill in the NA values of the bigrams with a probability of one extra bigram * unigram probability
        ngram_join$p2[is.na(ngram_join$p2)] <- (1/dim(bigram_probs)[[1]])*ngram_join$p[is.na(ngram_join$p2)]
        
        #Fill in the NA values with a probability of one extra trigram * bigram probability
        ngram_join$p3[is.na(ngram_join$p3)] <- (1/dim(trigram_probs)[[1]])*ngram_join$p2[is.na(ngram_join$p3)]
        
        #Use a list of terms in the text to find skipgram matches
        Rterms <- unique(as.character(as.data.frame(words)[!as.data.frame(words)[,1] %in% stop_words$word,]))
        #if we have any terms listed in Rterms...
        if (is.null(Rterms)==FALSE & length(Rterms) > 0) { 
          skipgrams <- lookupSkipwords(Rterms)
          ngram_join <- left_join(ngram_join,skipgrams,by='word')
          
          #Fill in the NA values of the skipgrams with a probability of one / the number of normalized skipgrams records
          ngram_join$pS[is.na(ngram_join$pS)] <- (1/dim(normalized_prob)[[1]])
          
          #compute a score that multiplies the original score with half the skipgrams' probability 
          #so that the trigram is still weighted higher
          ngram_join$score <- (ngram_join$p3)*(ngram_join$pS/2)#+ngram_join$p3+ngram_join$p)/3
          #order the results from highest score to lowest
          ngram_join <- arrange(ngram_join,desc(score))
          
        }else{
          #no terms, so now skipgrams - score and return just the trigram bigram unigram scores
          ngram_join$score <- (ngram_join$p3)
          #order the results from highest score to lowest
          ngram_join <- arrange(ngram_join,desc(score))}
        return(ngram_join)}
      else if (numWords == 2) { #so far only one word and a partial word has been entered into the text
          #get a list of words
          words <- textToList(input_text)
          
          first = numWords-1
          if (first < 0) {first=0}
          ngram <- words[[1]][first:numWords]
          print(ngram)
          
          #find the best matching bigram
          bi <- select(bigram_probs[bigram_probs$word1==ngram[1],],word2,p)
          bi <- bi[grep(paste0('^',ngram[2]),bi$word2),]
          #print(bi)
          #prep the unigram match dataframe
          unigram <- select(unigram_probs, word, p)
          unigram <- unigram[grep(paste0('^',ngram[2]),unigram$word),]
          colnames(bi) <- c('word','p2')
          #print(unigram)
          #join the bigram and unigram dataframes
          ngram_join <- left_join(unigram,bi,by='word')
          #Fill in the NA values of the bigrams with a probability of one extra bigram * unigram probability
          ngram_join$p2[is.na(ngram_join$p2)] <- (1/dim(bigram_probs)[[1]])*ngram_join$p[is.na(ngram_join$p2)]
          #score 
          ngram_join$score <- (ngram_join$p2)
          arrange(ngram_join,desc(score))
          return(ngram_join)}
        else if (numWords==1){
          #only a partial word so far.
          words <- textToList(input_text)
          ngram <- words[[1]][numWords]
          print('one word')
          print(ngram)
          unigram <- select(unigram_probs, word, p)
          unigram <- unigram[grep(paste0('^',ngram),unigram$word),]
          print(unigram)
          return(arrange(unigram,desc(p) ) )   
        } else {
          #empty text - return the most likely unigrams
          return(arrange(unigram_probs,desc(p) ) )
        }
    }
  }
}

#set up the variables as blank - they'll be populated later
inputText <- ""
ngram_input <- ""

#Initialize the first set of words
words <- list(head(unigram_probs,5)$word)[[1]]

## Start shiny app - this is the part that loops
shinyServer(function(input, output) {
  
  ## Partial word or a new word? Get a boolean (True = New)
  isNewWord <- reactive({isNewWord_f(input$text_in)})
  
  #render the text box with empty text
  output$input_textarea <- renderUI({tags$textarea(id="text_in", rows=6, cols=80, inputText)})
  
  #Send the predicted words to the buttons
  output$but1 <- renderUI({actionButton(inputId = "action1", label = words[1])})
  output$but2 <- renderUI({actionButton(inputId ="action2", label = words[2])})
  output$but3 <- renderUI({actionButton(inputId="action3", label = words[3])})
  output$but4 <- renderUI({actionButton(inputId="action4", label = words[4])})
  output$but5 <- renderUI({actionButton(inputId="action5", label = words[5])})
  
  #input_text, isNewWord, word_to_remove, selected_word
  
  ###Event Handlers
  
  ##Keep the input text variable current
  observe({
    #look for any changes in the status of the text_in
    if (is.null(input$text_in)==FALSE) { 
      if (inputText != input$text_in) {
        inputText <- input$text_in
        ##change the button labels to the best word predictors
        wordPred <- getWordChoices(input_text=isolate(inputText),
                                  isNewWord=isolate(isNewWord()))
        #update the top five words in the words list
        words <<- wordPred$word[1:5]
        #update the buttons
        output$but1 <- renderUI({actionButton(inputId='action1',label= words[1])})
        output$but2 <- renderUI({actionButton(inputId='action2',label= words[2])})
        output$but3 <- renderUI({actionButton(inputId='action3',label= words[3])})
        output$but4 <- renderUI({actionButton(inputId='action4',label= words[4])})
        output$but5 <- renderUI({actionButton(inputId='action5',label= words[5])})
        #print(output$but1$label)
      }
    }
  })
  
  ## Button1 button press
  observeEvent(input$action1, {
    output$input_textarea <- renderUI({
    tags$textarea(id="text_in", rows=6, cols=80, modify_text_input(input_text=isolate(input$text_in), 
                                                                   isNewWord=isolate(isNewWord()), 
                                                                   word_to_remove=isolate(tail(strsplit(input$text_in," ")[[1]],1)),
                                                                   selected_word=isolate(words[1])))})})
  ##Button2
  observeEvent(input$action2, {output$input_textarea <- renderUI({
    tags$textarea(id="text_in", rows=6, cols=80, modify_text_input(isolate(input$text_in), 
                                                                   isolate(isNewWord()), 
                                                                   isolate(tail(strsplit(input$text_in," ")[[1]],1)), 
                                                                   isolate(words[2])))})})
  ##Button3
  observeEvent(input$action3, {output$input_textarea <- renderUI({
    tags$textarea(id="text_in", rows=6, cols=80, modify_text_input(isolate(input$text_in), 
                                                                   isolate(isNewWord()), 
                                                                   isolate(tail(strsplit(input$text_in," ")[[1]],1)), 
                                                                   isolate(words[3])))})})
  ##Button4
  observeEvent(input$action4, {output$input_textarea <- renderUI({
    tags$textarea(id="text_in", rows=6, cols=80, modify_text_input(isolate(input$text_in), 
                                                                   isolate(isNewWord()), 
                                                                   isolate(tail(strsplit(input$text_in," ")[[1]],1)), 
                                                                   isolate(words[4])
                                                                   ))})})
  ##Button5
  observeEvent(input$action5, {output$input_textarea <- renderUI({
    tags$textarea(id="text_in", rows=6, cols=80, modify_text_input(isolate(input$text_in), 
                                                                   isolate(isNewWord()), 
                                                                   isolate(tail(strsplit(input$text_in," ")[[1]],1)), 
                                                                   isolate(words[5])
                                                                   ))})})

})
