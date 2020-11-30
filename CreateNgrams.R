# All data. Create the ngrams datatable from the raw data

## Loading the required packages and functions
source("functions.R")
library(memuse)

## unziping the data if it doesn't already exist
if(!dir.exists('./Data')){
  dir.create('Data')
  download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
                'Coursera-SwiftKey.zip')
  unzip('Coursera-SwiftKey.zip', exdir = 'Data', junkpaths = TRUE,
        files = c('final/en_US/en_US.twitter.txt',
                  'final/en_US/en_US.news.txt',
                  'final/en_US/en_US.blogs.txt'))
}

## Reading each .txt file to create the ngrams from it. 

dir.create("./ngram")
files = list.files("./Data", "*.txt", full.names = TRUE)

ngroups = 20 # number of groups to split the .txt by (each group is processed individually - RAM) 
j = 1L        # Iterator for the lists (gram2, gram3...)

gram2 = list()
gram3 = list()
gram4 = list()
gram5 = list()

# prepare to parallelize the ngrams calculation
cl = makeCluster(4)
doParallel::registerDoParallel(cl)

for (f in files){
  dat = readr::read_lines(f)
  dat = split(dat, rep(1:ngroups, length.out = length(dat)))
  
  for(i in seq_along(dat)){
    cat("\r created ngrams from ", (i-1)/ngroups*100, "% of ", basename(f), sep = "")
    # Preprocess the data
    d = preProcess(dat[[i]])
    
    # Create ngrams
    grams = foreach(n = 2:5, .packages = c("quanteda", "data.table", "stringr"))%dopar%
      createNgrams(d, n)

    gram2[j] = grams[1]
    gram3[j] = grams[2]
    gram4[j] = grams[3]
    gram5[j] = grams[4]
    j = j+1
    
    # Keep 2GB ram free to help with internals
    if(Sys.meminfo()$freeram@size<2){
      cat("\n Memory full, writing the n grams into directory 'ngram' \n")
      gram2 = rbindlist(gram2)
      gram3 = rbindlist(gram3)
      gram4 = rbindlist(gram4)
      gram5 = rbindlist(gram5)
      # When the memory is almost full, write the ngrams to disk and clear ram
      fwrite(gram2[,.(count = sum(count)), ngram], "./ngram/gram2.csv", append = T)
      fwrite(gram3[,.(count = sum(count)), ngram], "./ngram/gram3.csv", append = T)
      fwrite(gram4[,.(count = sum(count)), ngram], "./ngram/gram4.csv", append = T)
      fwrite(gram5[,.(count = sum(count)), ngram], "./ngram/gram5.csv", append = T)
      j = 1L
      rm(gram2, gram3, gram4, gram5, grams)
      invisible(gc())
      gram2 = list()
      gram3 = list()
      gram4 = list()
      gram5 = list()
    }
  }
  rm(dat)
  invisible(gc())
}
stopCluster(cl)

##  Write the last bit of data
gram2 = rbindlist(gram2)
gram3 = rbindlist(gram3)
gram4 = rbindlist(gram4)
gram5 = rbindlist(gram5)
fwrite(gram2[,.(count = sum(count)), ngram], "./ngram/gram2.csv", append = T)
fwrite(gram3[,.(count = sum(count)), ngram], "./ngram/gram3.csv", append = T)
fwrite(gram4[,.(count = sum(count)), ngram], "./ngram/gram4.csv", append = T)
fwrite(gram5[,.(count = sum(count)), ngram], "./ngram/gram5.csv", append = T)
