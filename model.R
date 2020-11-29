# Convert ngrams data to the fast lookup table

## Loading the required packages and functions
source("functions.R")

## List of ngram.csv available
files = list.files("./ngram", "*.csv", full.names = TRUE)

dir.create("./model")

for(f in files){
  dat = fread(f)[,.(count = sum(as.numeric(count))), ngram]
  n = as.numeric(str_extract(f, "\\d"))
  dat = cleanPreprocess(dat[count>1], n = n, 2)
  fwrite(dat, paste0("./model/", basename(f)))
  rm(dat, n)
  a = gc()
}
