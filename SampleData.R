## Random sampling of the corpus with no profanity and output 3 text files constituting the training corpus which is a certain percentage of the data (1% to 20%)


# We need the following packages loaded
# library(koRpus); library(reader)


conClean <- list.files(path = "/Users/saurabh/Desktop/Data Science/Capstone/final/en_US_NoProf") # list of files in the clean corpus
q <- length(conClean) # number of text files in the clean corpus


# length of text files in the clean corpus using reader package 
setwd("~/Desktop/Data Science/Capstone/final/en_US_NoProf") 
cleaned.file.size <- file.nrow(all.in.dir = TRUE) 
setwd("~/Desktop/Data Science/Capstone") 
# number of lines are the same after filtering because of left over white spaces probably


files2write <- character(q)
files.NoProf <- character(q)

ptm <- proc.time() # start clock
for (i in 1:q){
  chunkSize <- 200000 # number of lines of the text to be read at a time
  k = 0 # number of lines to skip
  files2write[i] <-paste0("/Users/saurabh/Desktop/Data Science/Capstone/final/train_data5/train.", conClean[i], sep
                          = NULL)
  conWrite <- file(files2write[i], open = "wt+") # open a connect to write the sampled data files

  # path to the text files in the profanity filtered corpus
  files.NoProf[i] <- paste0("/Users/saurabh/Desktop/Data Science/Capstone/final/en_US_NoProf/", conClean[i], sep =  NULL)

  # Read the file in chunks, sample it and write to a file
  while (k < cleaned.file.size[i]){
    chunk <- n.readLines(files.NoProf[i], n = chunkSize, skip = k, header = FALSE, comment = "")
    subset <- sample(chunk, size= round(length(chunk)* 0.05, 0)) # choose 5% of the data 
    remove(chunk); gc( verbose = TRUE)
    writeLines(subset, con = conWrite)
    k = k + chunkSize
    } 
  close(conWrite)
  }

proc.time() - ptm# Stop the clock


