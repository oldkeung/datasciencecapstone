library(data.table)
library(quanteda)
library(dplyr)

loadData <- function() {

  setwd("simple10")
  
  load("ngram_uni.rds")
  assign("uni", ngram, envir = globalenv())
  
  load("ngram_bi.rds")
  assign("bi", ngram, envir = globalenv())
  
  load("ngram_tri.rds")
  assign("tri", ngram, envir = globalenv())
  
  load("ngram_quad.rds")
  assign("quad", ngram, envir = globalenv())
  
  load("ngram_pent.rds")
  assign("pent", ngram, envir = globalenv())
  
}

debug <- function(txt) {
  print(txt)
}

getObs <- function(preTxt, ngramDT, discount = 0) {
  
    #print(head(ngramDT))
    obs <- ngramDT[base == preTxt]
    obsCount <- sum(obs$freq)
    
    obs %>% mutate(prob = (freq - discount) / obsCount)
    
}

#predict <- function(q, uni, bi, tri, quad, pent) {
predict <- function(q) {

  ### Convert question to prediction text
  ngram <- list(uni, bi, tri, quad, pent)
  maxngram <- length(ngram) - 1
  
  toks <- tokens(char_tolower(q), remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)
  nToks <- ntoken(toks)
  
  if (nToks >= maxngram)
    toks <- toks[[1]][(nToks - maxngram + 1):nToks]
  else
    toks <- toks[[1]]
  
  preTxt <- paste(toks, collapse = "_")
  
  nToks <- length(toks)
  debug(preTxt)
  debug(nToks)
  
  ### Get observation
  
  getObs(preTxt, ngram[[nToks + 1]])
  
}