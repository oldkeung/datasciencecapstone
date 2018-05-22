library(dplyr)
library(stringr)
library(quanteda)

##### Parameter
gamma2 <- 0.5  # bigram discount
gamma3 <- 0.5  # trigram discount

##### Step 3. Calculate Probabilities of Words Completing Observed Trigrams

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
  trigram_indices <- grep(regex, trigrams$ngram)
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
## convert counts to probabilities
#qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
#qbo_obs_trigrams

##### Step 4. Calculate Probabilities of Words Completing Unobserved Trigrams

##### Step 4.1 Find unobserved trigram tail words:

## Returns a character vector which are the tail words of unobserved trigrams
## that start with the first two words of obsTrigs (aka the bigram prefix).
## These are the words w in the set B(w_i-2, w_i-1) as defined in the section
## describing the details of equation 17.
##
## obsTrigs - character vector of observed trigrams delimited by _ of the form:
##            w3_w2_w1 where w3_w2 is the bigram prefix
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
getUnobsTrigTails <- function(obsTrigs, unigs) {
  obs_trig_tails <- str_split_fixed(obsTrigs, "_", 3)[, 3]
  unobs_trig_tails <- unigs[!(unigs$ngram %in% obs_trig_tails), ]$ngram
  return(unobs_trig_tails)
}

#unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigs)
#unobs_trig_tails

##### Step 4.2 Calculate discounted probability mass at the bigram level α(wi−1):

## Returns the total probability mass discounted from all observed bigrams
## calculated from equation 14.  This is the amount of probability mass which
## is redistributed to UNOBSERVED bigrams. If no bigrams starting with
## unigram$ngram[1] exist, 0 is returned.
##
## unigram - single row, 2 column frequency table. The first column: ngram,
##           contains the w_i-1 unigram (2nd word of the bigram prefix). The
##           second column: freq, contains the frequency/count of this unigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## bigDisc - amount to discount observed bigrams
getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
  # get all bigrams that start with unigram
  regex <- sprintf("%s%s%s", "^", unigram$ngram[1], "_")
  bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
  if(nrow(bigsThatStartWithUnig) < 1) return(0)
  alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDisc) / unigram$freq)
  
  return(alphaBi)
}

#unig <- str_split(bigPre, "_")[[1]][2]
#unig <- unigs[unigs$ngram == unig,]
#alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
#alpha_big

##### Step 4.3 Calculate backed off probabilities qBO for bigrams

## Returns a character vector of backed off bigrams of the form w2_w1. These 
## are all the (w_i-1, w) bigrams where w_i-1 is the tail word of the bigram
## prefix bigPre and w are the tail words of unobserved bigrams that start with
## w_i-1.
##
## bigPre - single-element char array of the form w2_w1 which are first two
##          words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
getBoBigrams <- function(bigPre, unobsTrigTails) {
  w_i_minus1 <- str_split(bigPre, "_")[[1]][2]
  boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = "_")
  return(boBigrams)
}

## Returns a two column data.frame of backed-off bigrams in the first column
## named ngram and their frequency/counts in the second column named freq.
## 
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency/count of each bigram.
getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
  boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
  obs_bo_bigrams <- bigrs[bigrs$ngram %in% boBigrams, ]
  return(obs_bo_bigrams)
}

## Returns a character vector of backed-off bigrams which are unobserved.
##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
## obsBoBigram - data.frame which contains the observed bigrams in a column
##               named ngram
getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
  boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
  unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$ngram)]
  return(unobs_bigs)
}

## Returns a dataframe of 2 columns: ngram and probs.  Values in the ngram
## column are bigrams of the form: w2_w1 which are observed as the last
## two words in unobserved trigrams.  The values in the prob column are
## q_bo(w1 | w2) calculated from from equation 10.
##
## obsBoBigrams - a dataframe with 2 columns: ngram and freq. The ngram column
##                contains bigrams of the form w1_w2 which are observed bigrams
##                that are the last 2 words of unobserved trigrams (i.e. "backed
##                off" bigrams). The freq column contains integers that are
##                the counts of these observed bigrams in the corpus.
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
## bigDisc - amount to discount observed bigrams
getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
  first_words <- str_split_fixed(obsBoBigrams$ngram, "_", 2)[, 1]
  first_word_freqs <- unigs[unigs$ngram %in% first_words, ]
  obsBigProbs <- (obsBoBigrams$freq - bigDisc) / first_word_freqs$freq
  obsBigProbs <- data.frame(ngram=obsBoBigrams$ngram, prob=obsBigProbs)
  
  return(obsBigProbs)
}

## Returns a dataframe of 2 columns: ngram and prob.  Values in the ngram
## column are unobserved bigrams of the form: w2_w1.  The values in the prob
## column are the backed off probability estimates q_bo(w1 | w2) calculated
## from from equation 16.
##
## unobsBoBigrams - character vector of unobserved backed off bigrams
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
## alphaBig - total discounted probability mass at the bigram level
getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
  # get the unobserved bigram tails
  qboUnobsBigs <- str_split_fixed(unobsBoBigrams, "_", 2)[, 2]
  w_in_Aw_iminus1 <- unigs[!(unigs$ngram %in% qboUnobsBigs), ]
  # convert to data.frame with counts
  qboUnobsBigs <- unigs[unigs$ngram %in% qboUnobsBigs, ]
  denom <- sum(qboUnobsBigs$freq)
  # converts counts to probabilities
  qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                             prob=(alphaBig * qboUnobsBigs$freq / denom))
  
  return(qboUnobsBigs)
}

#bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)  # get backed off bigrams
## separate bigrams which use eqn 10 and those that use 16
#obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
#unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
## unobs_bo_bigrams = c("the_buy", "the_EOS", "the_paint", "the_sell", "the_the")
## calc obs'd bigram prob's from eqn 10
#qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
## calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
#unig <- str_split(bigPre, "_")[[1]][2]
#unig <- unigs[unigs$ngram == unig,]
## distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
#qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
#qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
#qbo_bigrams

##### Checking the Bigram Calculations

#unobs <- qbo_bigrams[-1,]
#sum(unobs$prob)

##### Step 4.4 Calculate discounted probability mass at the trigram level α(wi−2,wi−1)

## Returns the total probability mass discounted from all observed trigrams.
## calculated from equation 14. This is the amount of probability mass which is
## redistributed to UNOBSERVED trigrams. If no trigrams starting with
## bigram$ngram[1] exist, 1 is returned.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix we are attempting to the predict the next word of. The 
##            second column: freq, contains the frequency/count of each trigram.
## bigram - single row frequency table where the first col: ngram, is the bigram
##          which are the first two words of unobserved trigrams we want to
##          estimate probabilities of (same as bigPre in other functions listed
##          prior) delimited with '_'. The second column: freq, is the
##          frequency/count of the bigram listed in the ngram column.
## triDisc - amount to discount observed trigrams
getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
  if(nrow(obsTrigs) < 1) return(1)
  alphaTri <- 1 - sum((obsTrigs$freq - triDisc) / bigram$freq[1])
  
  return(alphaTri)
}

#bigram <- bigrs[bigrs$ngram %in% bigPre, ]
#alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
#alpha_trig

##### Step 4.5 Calculate unobserved trigram probabilities qBO(wi|wi−2,wi−1)

## Returns a dataframe of 2 columns: ngram and prob.  Values in the ngram
## column are unobserved trigrams of the form: w3_w2_w1.  The values in the prob
## column are q_bo(w1 | w3, w2) calculated from equation 17.
##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## qboObsBigrams - 2 column data.frame with the following columns -
##                 ngram: observed bigrams of the form w2_w1
##                 probs: the probability estimate for observed bigrams:
##                        qbo(w1 | w2) calc'd from equation 10.
## qboUnobsBigrams - 2 column data.frame with the following columns -
##                   ngram: unobserved bigrams of the form w2_w1
##                   probs: the probability estimate for unobserved bigrams
##                          qbo(w1 | w2) calc'd from equation 16.
## alphaTrig - total discounted probability mass at the trigram level
getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                             qboUnobsBigrams, alphaTrig) {
  qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
  qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
  sumQboBigs <- sum(qboBigrams$prob)
  first_bigPre_word <- str_split(bigPre, "_")[[1]][1]
  unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$ngram, sep="_")
  unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
  unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
  
  return(unobsTrigDf)
}

#qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
#                                       qbo_unobs_bigrams, alpha_trig)
#qbo_unobs_trigrams

#unobs <- qbo_unobs_trigrams %>% transform(first_term = sub("(.*)_(.*)", "\\1", ngram), second_term = sub("(.*)_(.*)", "\\2", ngram)) %>% select(first_term, second_term, prob) 

##### Step 5. Select wi with the highest qBO(wi|wi−2,wi−1)

getPredictionMsg <- function(qbo_trigs) {
  # pull off tail word of highest prob trigram
  prediction <- str_split(qbo_trigs$ngram[1], "_")[[1]][3]
  result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                    " <<< which has probability = ", qbo_trigs$prob[1])
  return(result)
}

#qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
#qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
#out_msg <- getPredictionMsg(qbo_trigrams)
#out_msg

printSample <- function(data, title = "")
{
  
  if (title != "")
    print(paste("+++", title, "+++", sep = " "))
  
  print(head(data))
}

predict <- function(q, unigs, bigrs, trigs) {
  
  toks <- tokens(char_tolower(q), remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)
  #toks <- tokens_remove(toks, stopwords('en'))
  tok <- unlist(toks)
  bigPre <- paste(tok[length(tok) - 1], tok[length(tok)], sep = "_")
  printSample(bigPre, title = "0 bigPre")
  
  ##### Step 3. Calculate Probabilities of Words Completing Observed Trigrams
  
  obs_trigs <- getObsTrigs(bigPre, trigs)  # get trigrams and counts
  # convert counts to probabilities
  qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
  #qbo_obs_trigrams
  printSample(qbo_obs_trigrams, title = "3 qbo_obs_trigrams")
  
  ##### Step 4. Calculate Probabilities of Words Completing Unobserved Trigrams
  
  ##### Step 4.1 Find unobserved trigram tail words:
  
  unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigs)
  #unobs_trig_tails
  printSample(unobs_trig_tails, title = "4.1 unobs_trig_tails")
  
  ##### Step 4.2 Calculate discounted probability mass at the bigram level α(wi−1):
  
  unig <- str_split(bigPre, "_")[[1]][2]
  unig <- unigs[unigs$ngram == unig,]
  alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
  #alpha_big
  printSample(alpha_big, title = "4.2 alpha_big")
  
  ##### Step 4.3 Calculate backed off probabilities qBO for bigrams
  
  bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)  # get backed off bigrams
  # separate bigrams which use eqn 10 and those that use 16
  obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
  unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
  # unobs_bo_bigrams = c("the_buy", "the_EOS", "the_paint", "the_sell", "the_the")
  # calc obs'd bigram prob's from eqn 10
  qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
  # calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
  unig <- str_split(bigPre, "_")[[1]][2]
  unig <- unigs[unigs$ngram == unig,]
  # distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
  qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
  qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
  #qbo_bigrams
  printSample(qbo_bigrams, title = "4.3 qbo_bigrams")
  
  ##### Checking the Bigram Calculations
  
  unobs <- qbo_bigrams[-1,]
  printSample(sum(unobs$prob), title = "4.3 sum(unobs$prob)")
 
  ##### Step 4.4 Calculate discounted probability mass at the trigram level α(wi−2,wi−1)
  
  bigram <- bigrs[bigrs$ngram %in% bigPre, ]
  alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
  #alpha_trig
  printSample(alpha_trig, title = "4.4 alpha_trig")
  
  ##### Step 4.5 Calculate unobserved trigram probabilities qBO(wi|wi−2,wi−1)
 
  qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                         qbo_unobs_bigrams, alpha_trig)
  #qbo_unobs_trigrams
  printSample(qbo_unobs_trigrams, title = "4.5 qbo_unobs_trigrams")
 
  unobs <- qbo_unobs_trigrams %>% transform(first_term = sub("(.*)_(.*)", "\\1", ngram), second_term = sub("(.*)_(.*)", "\\2", ngram)) %>% select(first_term, second_term, prob) 
 
  ##### Step 5. Select wi with the highest qBO(wi|wi−2,wi−1)
 
  qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
  qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
  printSample(qbo_trigrams, title = "5 qbo_trigrams")
  #out_msg <- getPredictionMsg(qbo_trigrams)
  #out_msg
  
  #q
  #bigPre
  #unobs %>% filter(second_term %in% filterTxt) %>% arrange(desc(prob))
  
}