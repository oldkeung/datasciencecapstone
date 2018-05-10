gamma2 <- 0.5  # bigram discount
gamma3 <- 0.5  # trigram discount

## Returns a two column data.frame of observed trigrams that start with the
## bigram prefix (bigPre) in the first column named ngram and
## frequencies/counts in the second column named freq. If no observed trigrams
## that start with bigPre exist, an empty data.frame is returned.
##
## bigPre -  single-element char array of the form w2_w1 which are the first 
##           two words of the trigram we are predicting the tail word of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency/count of each trigram.
getObsTrigs <- function(bigPre, trigrams) {
  trigs.winA <- data.frame(ngrams=vector(mode = 'character', length = 0),
                           freq=vector(mode = 'integer', length = 0))
  regex <- sprintf("%s%s%s", "^", bigPre, "_")
  regex
  trigram_indices <- grep(regex, trigrams$feature)
  trigram_indices
  if(length(trigram_indices) > 0) {
    trigs.winA <- trigrams[trigram_indices, ]
  }
  
  return(trigs.winA)
}

## Returns a two column data.frame of observed trigrams that start with bigram
## prefix bigPre in the first column named ngram and the probabilities
## q_bo(w_i | w_i-2, w_i-1) in the second column named prob calculated from
## eqn 12. If no observed trigrams starting with bigPre exist, NULL is returned.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix bigPre which we are attempting to the predict the next
##            word of in a give phrase. The second column: freq, contains the
##            frequency/count of each trigram.
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency/count of each bigram.
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## triDisc - amount to discount observed trigrams
getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDisc=0.5) {
  if(nrow(obsTrigs) < 1) return(NULL)
  obsCount <- filter(bigrs, ngram==bigPre)$freq[1]
  obsTrigProbs <- mutate(obsTrigs, freq=((freq - triDisc) / obsCount))
  colnames(obsTrigProbs) <- c("ngram", "prob")
  
  return(obsTrigProbs)
}

#obs_trigs <- getObsTrigs(bigPre, trigs)  # get trigrams and counts
# convert counts to probabilities
#qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
#qbo_obs_trigrams