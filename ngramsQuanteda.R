library(data.table)
library(quanteda)
library(dplyr)
library(magrittr)
library(doParallel)
library(foreach)


conTrain <- list.files("/home/sonia/Data Science/COURSES/DS_Specialization/Capstone/final/train_data5minus")

filePaths.train <-c()
p=length(conTrain)
for (i in 1:p){
  filePaths.train[i] = paste0("/home/sonia/Data Science/COURSES/DS_Specialization/Capstone/final/train_data5minus/", conTrain[i])
}

train.text.file <- textfile(filePaths.train, cache=TRUE) # create a corpusSource-class object   
train.corpus <- corpus(train.text.file) # Load the data


#XXXXXXXXXX=========================XXXXXXXXXXXX
#PARALLEL PROCESSING

# Calculate the number of cores
no_cores <- detectCores() - 1 # number of clusters to make

# # Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)

 foreach(corpus = train.corpus,
        .combine = 'cbind', .packages = 'quanteda') %dopar%
  system.time(quanteda::tokenize(toLower(train.corpus), what = "fastestword", removePunct = TRUE, removeNumbers = TRUE, removeURL = TRUE, removeSymbols = TRUE, ngrams = 2, concatenator = " ", verbose = TRUE))
stopCluster(cl)
#XXXXXXXXXXX============================XXXXXXXXXXXX


## creating ngrams without paralle processing
ptm <- proc.time() # start clock
mytoken <- quanteda::tokenize(toLower(train.corpus), what = "fastestword", removePunct = TRUE, removeNumbers = TRUE, removeURL = TRUE, removeSymbols = TRUE, ngrams = 2, concatenator = " ", verbose = TRUE)
proc.time() - ptm# Stop the clock

ptm <- proc.time() # start clock
ngrams <- quanteda::dfm(mytoken)
#bigrams <- dfm(train.corpus, ngrams = 2, concatenator = " ")
proc.time() - ptm# Stop the clock


freq.ngm <- colSums(as.matrix(ngrams)) # bigram frequencies across all documents

# look at the most frequntly occuring bigrams 
#ord.2gm <- order(freq.2gm, decreasing = TRUE)
#freq.2gm[head(ord.2gm, 10)]

# Data table with bigrams and their counts
bigrams = names(freq.ngm)
names(freq.ngm) <- NULL
ngrams.dt <- data.table(bigrams, freq = freq.ngm)
ngrams.dt <- arrange(ngrams.dt, desc(freq))

# Remove n-grams with very low frequency
ngrams.pruned <- subset(ngrams.dt, freq >1)

# Write the bigrams and pruned bigrams to csv files
system.time(write.csv(ngrams.dt, "/home/sonia/Data Science/COURSES/DS_Specialization/Capstone/final/ngrams5/bigrams.csv"))
system.time(write.csv(ngrams.pruned, "/home/sonia/Data Science/COURSES/DS_Specialization/Capstone/final/ngrams5/bigrams.pruned1.csv"))






