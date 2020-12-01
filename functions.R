# Define all the functions and libraries
library(quanteda)
library(dplyr)
library(stringr)
library(data.table)
library(doParallel)
library(compiler)

# Function to easily print object size
getObjSize = function(obj, unit. = "auto"){
    if(is.character(obj)){
        stopifnot(length(obj)==1)
        print(object.size(get0(obj)), unit = unit.)
    }
    print(object.size(obj), unit = unit.)
}

# Function to pre-process the documents and return a token list
preProcess = function(documents){
    toks = documents%>%
        # remove URLs
        str_replace_all("((http)|(www))\\S+\\b", " ")%>%
        # remove email addresses
        str_replace_all("\\S+@\\S+\\.\\S+\\b", " ")%>%
        # convert paragraphs/documents into vector of sentences
        str_split("(?<!\\w\\.\\w.)(?<![A-Z]\\.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s")%>%
        unlist()%>%
        tolower()%>%
        str_replace_all("[:punct:]|\\d", " ")%>%
        str_squish()
    # add a start and end of sentence token
    toks  = paste("<s>", toks, "</s>")
    # sentences to word tokens
    toks = tokenize_fastestword(toks)
    # convert list to tokens
    as.tokens(toks)
}


# Function to create ngrams from tokens and find their counts
createNgrams = function(toks, n=2L, ...){
    # Create a list of N-grams of word size n
    toksNgrams = tokens_ngrams(toks, n, ...)
    # convert into character vector to use data.table functions
    b = data.table(ngram = unlist(toksNgrams, use.names = FALSE))
    # Find the count of every unique ngram
    b = b[, count := as.double(.N), by = ngram]
    b
}

# Remove _'s
remUnderscore = function(string){
    # Replace multiple _ by just one (-___-) are very common
    string = str_replace_all(string, "-?_+-?", "_")
    string = str_replace_all(string, "_+", "_")
    # Remove _'s at the start of string
    string = str_remove_all(string, "^_+")
    # Remove _'s at the end of string
    str_remove_all(string, "_+$")
} 

# function to set the key for the ngrams

keying = function(.dt, n, pred = FALSE, namecol = names(.dt)){
    if(n<2&pred) {
        warning("\n Setting the first column as key \n Better use data.table::setkeyv() if this is not correct")
        pred = FALSE
    }
    stopifnot(n<length(namecol))
    if(pred){
        return(setkeyv(.dt, namecol[1:(n)]))
    }else{
        return(setkeyv(.dt, namecol[1:(n-1)]))
    }
}

# Clean the ngrams data
cleanPreprocess = function(nGram, n, threshold = 1){
    namecol = c("V5ngram_1", "V4ngram_1", "V3ngram_1", "V2ngram_1", "V1ngram_1", "prediction")
    # Split the ngram into character matrix
    dt = nGram[, tail(namecol, n) := tstrsplit(ngram, split = "_")]
    setcolorder(dt, c(tail(namecol, n) ,"count"))
    # # Encode the char matrix into integer matrix and make a list with each column of the matrix
    # ngramsplit = lapply(1:n, function(i) as.integer(factor(ngramsplit[,i], charLevels)))
    dt = dt[, ngram:=NULL]
    keying(dt, n, TRUE)
    # Count is the number of n grams (known+prediction words)
    dt = dt[, .(count = sum(count)), by = key(dt)]
    keying(dt, n, FALSE)
    # i.count is the number of n-1 grams (known words)
    dt = dt[, i.count := sum(count), key(dt)]
    dt = dt[ count>threshold, score := (count)/(i.count)*0.4^(5-n)]
    dt = na.omit(dt)
    colName = c(key(dt), "score")
    dt = setorderv(dt, colName, c(rep(1,n-1), -1))[, indx:=seq_len(.N), key(dt)][indx<=3]
    dt = dt[, c("indx", "i.count", "count"):=NULL]
    # .dt = .dt[, prediction := charLevels[prediction]]
    dt[,prediction := str_replace(prediction, "<.s>", "\\.")]
}

# Create the predicting function

## function to convert the character vector into a format that matches the model
### A sentence. Becomes c("<s>", "a", "sentence", "</s>", "<s>", "becomes")
preProcessPredict =  compiler::cmpfun(function(documents, knownChars = wordindx){
    nlength = 4
    res1 = documents%>%
        # remove URLs
        str_replace_all("((http)|(www))\\S+\\b", " ")%>%
        # remove email addresses
        str_replace_all("\\S+@\\S+\\.\\S+\\b", " ")%>%
        # convert paragraphs/documents into vector of sentences
        str_split("(?<!\\w\\.\\w.)(?<![A-Z]\\.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s")%>%
        unlist()%>%
        tolower()%>%
        str_replace_all("[:punct:]|\\d", " ")%>%
        str_squish()
    res1  = paste("<s>", res1, "</s>")
    # sentences to list of word tokens
    res1 = tokenize_fastestword(res1)%>%
        unlist
    res1 = res1[-length(res1)]
    # subset the last 4 words (pad NA before if needed)
    res1 = tail(append(c(NA, NA, NA, NA, NA), res1), nlength)
    # Identify the known/unknown words
    known = is.element(res1, knownChars)
    if(sum(known)!=(nlength)){
        # If any word is unknown, return the Cross Join on the known words + NA
        # Remove the unknown words
        res1 = res1[known]
        # Convert into a list with the appropriate names
        res1 = split(res1, tail(c("V5ngram_1", "V4ngram_1", "V3ngram_1", "V2ngram_1", "V1ngram_1"), nlength)[known])
        # Append NA to the list elements so that 3grams, 2grams etc are also considered
        res1 = lapply(res1, function(x)append(x, NA))
        # create a datatable with all the possible combinations of the words (preserving position)
        res1 = do.call(function(...)CJ(..., unique = T), res1)
        return(res1)
        
    }else{
        # Else return the PASTED Cross join
        # Split into a list
        res1 = split(res1, 1:length(res1))
        # Append NA to the list elements so that 4grams, 3grams etc are also considered
        res1[1:(nlength-1)] = lapply(res1[1:(nlength-1)], function(x)append(x, NA))
        # create a datatable with all the possible combinations of the words (preserving position)
        res1 = do.call(function(...)CJ(..., unique = T), res1)
        # paste the words back into sentences (with NAs for gram1, 2, 3etc)
        res1 = res1[, .(known.=do.call(paste, .SD))]
        return(res1)
    }
})

## Function that performs the prediction
PredictText = compiler::cmpfun(function(string, gramTable = ngrams, chars = wordindx, nPred = 5L){
    # Convert the sentence(s) into a list of last 4 words
    string = preProcessPredict(string, chars)
    
    # # select the V*ngram_1 which are known to the model
    # sel = sapply(string, function(x)sum(!is.na(x)))>0
    # If all words are unknown, return the most common words
    if(sum(is.na(string))==length(string)){
        return(head(c("the", "to", "and", "a", "of", "i", "in", "you", "that", "is"), nPred))
    }else if(length(string)==1){
        # If All the words are know, use the keyed column, otherwise use the names of string
        # If a word was suggested by multiple ngrams, group them up and consider only the max score
        hits  = gramTable[ string, .(score = max(score, na.rm = TRUE)), prediction, nomatch = NULL]
        ## this datatable (ie. string) is used to subset the gramTable with matches
        ## It's grouped by predictions to avoid repeated recommendations
        ## Only the highest score for each prediction is considered when suggesting
    }else{
        # Same as above, but using ON rather than the key
        hits  = gramTable[ string, .(score = max(score, na.rm = TRUE)), on = names(string), prediction, nomatch = NULL]
    }
    head(hits[order(-score), prediction], nPred)

})

