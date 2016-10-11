
##Language Model: Word Prediction



We build a language model using n-grams and back off with interpolation to predict the next word in a typed sentence.

We use [HC Corpora](http://www.corpora.heliohost.org/) which consists of text data from news, blogs and twitter. 

Here is an overview of the steps involved in the process of building and implementing the model

1. Clean the data by removing profane words
2. Sample the data into training and held off sets of varying sizes (1% - 20%)
3. Tokenize the training set into unigrams, bigrams, trigrams and quadgrams
4. Use ngrams model with various techniques backoff and interpolation techniques to build the prediction algorithm
5. Evaluate the model by comparing the accuracy rate on the held off data
6. Make the model efficient and fast by pruning the n-grams
7. Build a shiny app to showcase the model ([here](https://ssharma24.shinyapps.io/CapstoneShinyApp/))