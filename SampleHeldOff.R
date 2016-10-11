## Take the sampled data from the No Prof data, and further divide it into a training set  (train_data20minus) and a .5% HeldOff data (test_data20)


con.FileNames <- list.files(path = "/Users/saurabh/Desktop/Data Science/Capstone/final/en_US_NoProf") # list of files in the clean corpus
r <- length(con.FileNames) # number of text files in the clean corpus

testfiles2write <- character(r)
trainfiles2write <- character(r)


for (i in 1:r){
 
    # name of the test and train files where the data will be written 
    testfile2write <- paste0("/Users/saurabh/Desktop/Data Science/Capstone/final/test_data5/test.", con.FileNames[i], sep = NULL)
    trainfile2write <-  paste0("/Users/saurabh/Desktop/Data Science/Capstone/final/train_data5minus/train.", con.FileNames[i], sep = NULL)
    
    # open connections to write the files
    conTest <- file(testfile2write, open = "wt")
    conTrain <- file(trainfile2write, open = "wt")
    
    
    file2read <- paste0("/Users/saurabh/Desktop/Data Science/Capstone/final/train_data5/train.", con.FileNames[i], sep = NULL)
    #con.file2read <- file(file2read)
    textdata <- readLines(file2read)
    l <- length(textdata)
    sample.indicies <- sample.int(l, size = l*.02)
    test.subset <- textdata[sample.indicies]
    newtrain.set <- textdata[-sample.indicies]
    writeLines(test.subset, conTest)
    writeLines(newtrain.set, conTrain)
    
    close(conTest); close(conTrain) # close connections
}
