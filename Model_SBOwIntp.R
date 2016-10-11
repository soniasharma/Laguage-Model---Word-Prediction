library(tm); library(quanteda); library(dplyr); library(magrittr)

# load n grams
unigram.df <- read.csv("/Users/saurabh/Desktop/Data Science/Capstone/final/ngrams2/unigrams.pruned1.csv") # create a data frame with unigrams and frequencies
bigram.df <- read.csv("/Users/saurabh/Desktop/Data Science/Capstone/final/ngrams2/bigrams.pruned1.csv") # create a data frame with bigrams and frequencies
trigram.df <- read.csv("/Users/saurabh/Desktop/Data Science/Capstone/final/ngrams2/trigrams.pruned1.csv") # create a data frame with trigrams and frequencies

N = sum(unigram.df$freq)
V = dim(unigram.df)[1]
lambda = .0001

# A function which takes a sentence and returns a character vector with the last m words (removing punctuations, numbers and in lower case)
#the function below uses quanteda package

# using the pipe operator in magrittr
chooseLastWords <-function(text, m){
  lastwords <- text %>%
    toLower %>% # qunateda package
    quanteda::tokenize(removeNumbers = TRUE, removePunct = TRUE, ngrams = 1, concatenator = " ") %>% # qunateda package
    extract2(1) %>% 
    tail(m) # pick the last element of the character vector of ngrams
  
  return(lastwords)
}

##### Function which finds the the indices of ngrams which match the expression
# n: n in the n gram model 
# ngram.df : data frame with the ngrams in the 'ngrams' col and frequncies in the 'freq' col 
# text: sentence entered by the user
# colName: name of the column with ngram

index.relevnt.ngm <-function(ngram.df, text, n, colName){
  if (n != 1){
    word <- paste(tail(chooseLastWords(text, 2), n-1), collapse = " ") # chooses the last n-1 words
    
    ngram.df %>%
      extract2(colName) %>%  # subsets to the ngrams col
      grep(paste0("^",word,"\ "), x = . , perl = TRUE) #find indices which match the expression
  }
}

# m := number of top predicted words to output

BackOffInterpolation.Pred <- function(text, m){
  u = chooseLastWords(text, 2)[1] # first word
  v = chooseLastWords(text, 2)[2] # second word
  
  # extract traigrams begining with (u v)
  relv.trigram <-  trigram.df %>% 
    index.relevnt.ngm(., text, 3, "trigrams") %>%
    trigram.df[.,]
  
  s = dim(relv.trigram)[1] # number of distinct w which follow (u v)
  if (s > 0){
    ct_v <- unigram.df[grepl(paste0('^', v,'$'), unigram.df$unigrams), "freq"]
    
    # extract all the possible w following (u v)
    w <- relv.trigram %>% 
      extract2("trigrams") %>%
      as.character %>%
      strsplit(split = " ", perl =TRUE) %>%
      sapply('[',3) %>%
      unname
    
    ct_uvw <- relv.trigram$freq
    ct_w <- unname(sapply(w, function(x) unigram.df[grepl(paste0('^', x,'$'), unigram.df$unigrams), "freq"]))
    ct_vw <- unname(sapply(w, function(x) bigram.df[grepl(paste0('^',v, '\ ', x,'$'), bigram.df$bigrams), "freq"]))
    
    q_uvw <- ct_uvw + lambda * V * ((ct_vw + lambda * V * ((ct_w + lambda)/(N+lambda * V))) /(ct_v + lambda * V))
    final <- data.table(w=w, q = q_uvw)
    final %>% arrange(desc(q)) %>% extract2('w') %>% head(m)
    #q_uvw %>% sort(decreasing = TRUE) %>% head(m) %>% names # predicting the top three answers
  } else {
    relv.bigram <-  bigram.df %>% 
      index.relevnt.ngm(., text, 2, "bigrams") %>%
      bigram.df[.,]
    r=dim(relv.bigram)[1]
    if (r > 0){
      w <- relv.bigram %>% 
        extract2("bigrams") %>%
        as.character %>%
        strsplit(split = " ", perl =TRUE) %>%
        sapply('[',2) %>%
        unname
      
      ct_vw <- relv.bigram$freq
      ct_w <- unname(sapply(w, function(x) unigram.df[grepl(paste0('^', x,'$'), unigram.df$unigrams), "freq"]))
      q_vw <-ct_vw + lambda * V * (ct_w + lambda)/(N + lambda * V)
      final <- data.table(w=w, q = q_vw)
      final %>% arrange(desc(q)) %>% extract2('w') %>% head(m)
    }
  }
  
}


#### Model Evaluation
#check the accuracy of our model on the held out set


trigram.heldoff <- read.csv("/Users/saurabh/Desktop/Data Science/Capstone/final/ngrams2_test/trigrams.csv")

trigram.heldoff.pruned <- trigram.heldoff[trigram.heldoff$freq >1,]
trigrams.split <- sapply(as.character(trigram.heldoff.pruned$trigrams),  function(x) strsplit(x, " ")) # gives a list of all unique trigrams split traigrams.test[[n]] gives the nth

# split the trigram from helpd out data to fist two (input) and last word (output)
trueOutWords <-   unname(sapply(trigrams.split, "[[", 3))
inputWords <-  unname(sapply(trigrams.split, function(x) paste(x[[1]], x[[2]], sep = " ")))


system.time(predictedWords <- unname(sapply(inputWords, function(x) BackOffInterpolation.Pred(x, 1))))

predictedWords <- as.character(predictedWords)

sum(predictedWords == trueOutWords)/length(predictedWords)*100
