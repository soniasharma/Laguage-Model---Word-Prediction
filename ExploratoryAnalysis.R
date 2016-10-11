## We tokenize the sampled data and create plots and tables to look at
## 1. distribution of the tokens such as words, punctuations, commas etc.
## 2. Distribution of the frequency of the words
## 3. Coverage: The percentage of words our sampled data covers from the total vocabulary


#library(tm)
#library(koRpus)
#library(reader)
library(data.table)
library(quanteda)
library(textcat)
library(ggplot2)
library(wordcloud)
library(gridExtra)



###====================================================
## Tokenization of the sampled data set using korPus package
## Create a summary data frame with words, lines, sentence, punct count of the three files
## Make plots of tokens and word dsitributions

setwd("/Users/saurabh/Desktop/Data Science/Capstone/final/train_data")

conTrain <- list.files() # lists files in the corpus
p <- length(conTrain) # number of text files in the corpus
file.train.size <- file.nrow(all.in.dir=TRUE) # length of texts files in the corpus (reader package)
dt <- data.table(token = character(0), lttr = numeric(0), wclass = character(0))
tokenized.version.dt <- list(blog.token.dt = dt, news.token.dt = dt, twitter.token.dt = dt) # list with three empty data tables 
sampleDataSummary <- data.frame() 


par(mfcol = c(2,3), cex.sub = 1.3, col.sub = "blue", font.sub = 2)
# Start the clock!
ptm <- proc.time() 
for (i in 1:p) {
textData <- n.readLines(conTrain[i], n = file.train.size[i], header = FALSE, comment = "")
#tokenized <- tokenize(txt = textData, format = "obj", lang = "en")
tokenized <- tokenize(txt = conTrain[i], format = "file", lang = "en") # from koRpus package

tokenDesc <- slot(tokenized, name="desc")

tokensdf <-slot(tokenized, name="TT.res")
tokensdf$wclass <- as.factor(tokensdf$wclass)
#summary(tokensdf$wclass) # Frequencies of different tokens like fullstop, comma, number punctuations etc. 

# Make a summary table of important features of the files, wordcount, linecount etc.
df<- data.frame(fileName = conTrain[i], lineCount = tokenDesc$lines, wordCount = tokenDesc$words, sentCount = tokenDesc$sentences, punctCount = tokenDesc$punct) 
sampleDataSummary <- rbind(sampleDataSummary, df)

# tokenized data table: drop irrelevant columns and save a Data Table with relevent info (token and type)
tokenized.version.dt[[i]] <- rbind(tokenized.version.dt[[i]], as.data.table(tokensdf[, c("token", "lttr", "wclass")])) 

# following three plot the distribution of wordclass in the tokenized data
#barplot(summary(tokensdf$wclass), xlab="", ylab="frequency", main = "word class distribution", cex.axis = .7, las=2 )
#OR
#plot(tokenized, what="wclass") # plots the frequencies of different tokens
#OR
ggplot(tokensdf, aes(x=wclass)) +geom_bar() # best looking 

plot(tokenized, what="letters") # line plot of distribution of word lengths in number of letters
title (sub =conTrain[i])



#df <- slot(object = tokenized, name = "TT.res") # extract the data frame with the tokenized version

remove(tokensdf); gc() # free up some space
}
# Stop the clock
proc.time() - ptm





#####=========================================================================xxxxx

# Use tm package
docs <- Corpus(DirSource("/Users/saurabh/Desktop/Data Science/Capstone/final/train_data")) # Load the data
# Preprocessing

docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument) # treat your preprocessed documents as text documents

dtm <- DocumentTermMatrix(docs)

dim(dtm)
inspect(dtm[, 100:108])

# Number of total words in the three text files
wordcount <- c(sum(dtm[1, ]), sum(dtm[2, ]), sum(dtm[3, ]))

# Number of words coming from foreign language using textcat package
textcat(dtm[["dimnames"]][["Terms"]][1000:1010])



findFreqTerms(dtm, 1000) # terms which occur atleast 10 times
findFreqTerms(dtm, lowfreq=100, highfreq =200)
length(findFreqTerms(dtm, lowfreq=4, highfreq =4)) # terms which occur only once 

# function which count words by the frequency of their occurance 

countwords.by.freq <- function(p){
len <- numeric(0)
for (i in 1:p+1)
  {if (i < p+1)
  {len[i] <- length(findFreqTerms(dtm, lowfreq=i, highfreq =i))}
  else
len[i] <-  length(findFreqTerms(dtm, lowfreq=i))
  }
return(len)
}

barplot(countwords.by.freq(1000))
barplot(log(countwords.by.freq(100)+1)) # better see the data


plot(dtm, terms = findFreqTerms(dtm, lowfreq = 6)[1:25], corThreshold = 0.5)

# Organize terms by their frequency, most and least frequent words
freq <- colSums(as.matrix(dtm))   
#length(freq) 

ord <- order(freq, decreasing = TRUE)
freq[head(ord, 200)] # inspect most frequently occuring words in the corpus
freq[tail(ord, 100)] # insepct least frequently occuring words (shows words of other laguages like chinese(?) come up)
freq[ord[100:200]]

wf <- data.frame(word=names(freq), freq=freq)   

wf.ordered <-wf[order(wf[,2], decreasing = TRUE),]


#=======================================================
# words occuring more than 4000 times
p <- ggplot(subset(wf, freq>4000), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 


# words occuring more between 2000 and 3000 times
word.freq.df <- subset(wf, freq > 2000 & freq < 3000 )
p <- ggplot(word.freq.df[order(word.freq.df$freq, decreasing = TRUE), ], aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 


#=========================
# wordcloud 
## wordcloud of most frequntly appearing 100 words
set.seed(142)   
jpeg('rplot.jpg')
wordcloud(names(freq), freq, min.freq=500, max.words=100, colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=.15, scale=c(10,.9)) 
dev.off()

#==============================
# Coverage

sum=0
freq.colm=wf.ordered[, 2]
total.count = sum(freq.colm)
i=1
m=dim(wf.ordered)[1]
coverage = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95)
cutoff.index = numeric(length(coverage))
uniqword.percent = numeric(length(coverage))
cutoff.freq = numeric(length(coverage))

for (j in 1: length(coverage)){
  while(i <= m){
    if (sum < total.count * coverage[j]){
      sum=sum+freq.colm[i]; 
      i=i+1; 
      next
    }
    else {
      cutoff.index[j] = i-1; print(cutoff.index[j]); 
      uniqword.percent[j] = cutoff.index[j]/m*100; print(uniqword.percent[j]); 
      cutoff.freq[j] = freq.colm[i-1]; print(cutoff.freq[j]);
      break
    } 
  }
}

coverage.df <- data.frame(coverage, cutoff.index, uniqword.percent, cutoff.freq)


# coverage.plot1 <- ggplot(coverage.df, aes(x=coverage, y=cutoff.index)) +
#   geom_line() + 
#   theme_bw() + 
#   geom_point(color="blue") +
#   xlab("Percentage of coverage") +
#   ylab("Number of unique words")

coverage.plot2 <- ggplot(coverage.df, aes(x=coverage, y=uniqword.percent)) +
  geom_line() + 
  theme_bw() + 
  geom_point(color="blue") +
  xlab("Percentage of coverage") +
  ylab("Percentage of unique words")
  


coverage.plot3 <- ggplot(coverage.df, aes(x=coverage, y=cutoff.freq)) +
  geom_line() + 
  theme_bw() + 
  geom_point(color="blue") +
  xlab("Percentage of coverage") +
  ylab("Minimum frequency of the unique words needed")

grid.arrange( coverage.plot2, coverage.plot3, ncol=2)

coverage.df[ c(5,9),]

#=================================================================================================xxxxxxxx
