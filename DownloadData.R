
library(tm)
library(koRpus)
library(reader)

# Download and unzip the text data

unzip("Coursera-SwiftKey.zip", list=TRUE) # lists the files and extracts none
unzip("Coursera-SwiftKey.zip") # Extract all files

# Size of the raw data
cons <- list.files() 
file.size <- file.nrow(all.in.dir=TRUE) # length of texts files in the corpus using reader package
p <- length(cons) # number of text files in the corpus

engDataSizeDT <- data.table(fileName = list.files(), sizeMB = round((sapply(cons, file.size))/1000000, 1), lineCount =  line.count) 
 

#1.  Create new text files with profanity filtered data 
source("ProfanityFilter.R") 


#2. Select a random sample of the data to work with for further analyses
source("SampleData.R")

#3. Create a heldoff subset for evaluation of the model
source("SampleHelfOff.R")







