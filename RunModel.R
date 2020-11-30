# Importing the data for the model

## Loading the required packages and functions
source("functions.R")

## List of ngram.csv available
files = list.files("./model", "*.csv", full.names = TRUE)

## Read all the .csv files to a list and bind them into 1 data.table
ngrams = lapply(files, fread)
ngrams = rbindlist(ngrams[4:1], use.names = TRUE, fill = TRUE)

## A list of words that are known/used in the data.table to make predictions off of
wordindx = do.call(c, ngrams[,-c(5,6)])
wordindx = unique(wordindx)

## make a column pasting all the words to use if they're all known
ngrams = ngrams[, known.:= do.call(paste, .SD[, -c(5,6)])]
setkey(ngrams, known.)

## Splitting each sentence of the test data
### Needs the tweets and blogs data from benchmark.R
test = sapply(c(tweets, blogs), function(l){
  t(split.sentence(l))
})

test =  do.call(rbind, test)
test = as.data.table(test)[sentence!=""]

## Run this line to check if the model produces predictions or not
predictions = sapply(test$sentence[1:246], PredictText)
