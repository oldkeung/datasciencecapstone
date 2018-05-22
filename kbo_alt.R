library(data.table)
library(quanteda)
library(dplyr)

options("digits.secs"=6)
lastSysTime <- Sys.time()

loadData <- function() {

  setwd("simple10")
  
  load("ngram_uni.rds")
  setindex(ngram, base)
  assign("uni", ngram, envir = globalenv())
  
  load("ngram_bi.rds")
  setindex(ngram, base)
  assign("bi", ngram, envir = globalenv())
  
  load("ngram_tri.rds")
  setindex(ngram, base)
  assign("tri", ngram, envir = globalenv())
  
  load("ngram_quad.rds")
  setindex(ngram, base)
  assign("quad", ngram, envir = globalenv())
  
  load("ngram_pent.rds")
  setindex(ngram, base)
  assign("pent", ngram, envir = globalenv())
  
  setwd("..")
  
}

loadSampleData <- function() {
  
  txt <- c(
    "sos buy the book eos",
    "sos buy the book eos",
    "sos buy the book eos",
    "sos buy the book eos",
    "sos sell the book eos",
    "sos buy the house eos",
    "sos buy the house eos",
    "sos paint the house eos"
  )
  
  toks <- tokens(txt)

  uni <- data.table(
    textstat_frequency(dfm(tokens_ngrams(toks, n = 1))) %>% 
      transform(base = sub("(.*)_(.*)", "\\1", feature), predict = sub("(.*)_(.*)", "\\2", feature)) %>% 
      select(base, freq = frequency)
  ) 
  
  setindex(uni, base)
  assign("uni", uni, envir = globalenv())
    
  bi <- data.table(
    textstat_frequency(dfm(tokens_ngrams(toks, n = 2))) %>% 
      transform(base = sub("(.*)_(.*)", "\\1", feature), predict = sub("(.*)_(.*)", "\\2", feature)) %>% 
      select(base, predict, freq = frequency)
    ) 
  
  setindex(bi, base)
  assign("bi", bi, envir = globalenv())
  
  tri <- data.table(
    textstat_frequency(dfm(tokens_ngrams(toks, n = 3))) %>% 
      transform(base = sub("(.*)_(.*)", "\\1", feature), predict = sub("(.*)_(.*)", "\\2", feature)) %>% 
      select(base, predict, freq = frequency)
  ) 
  
  setindex(tri, base)
  assign("tri", tri, envir = globalenv())
  
}

debug <- function(data, title = "") {
  
  now <- Sys.time()
  
  if (title != "")
    print(paste("+++", title, now, "(", now - lastSysTime, ")", "+++", sep=" "))
  
  print(head(data))
  
  lastSysTime <- now
  
}

info <- function(title = "") {
  
  now <- Sys.time()
  
  if (title != "")
    print(paste("+++", title, now, "(", now - lastSysTime, ")", "+++", sep=" "))
  
  lastSysTime <- now
  
}

getPreTxt <- function(q, maxngram) {
  
  q <- gsub("_", " ", q)
  
  toks <- tokens(char_tolower(q), remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)
  toks <- toks[[1]]
  nToks <- length(toks)
  
  if (nToks >= maxngram)
    toks <- toks[(nToks - maxngram + 1):nToks]

  nToks <- length(toks)
    
  preTxt <- paste(toks, collapse = "_")
  preTxtBo <- paste(toks[1:(nToks - 1)], collapse = "_")
  preTxtTail <- toks[nToks]
  
  list("corpus" = q, "toks" = toks, "nToks" = nToks, "preTxt" = preTxt, "preTxtBo" = preTxtBo, "preTxtTail" = preTxtTail)
  
}

getObs <- function(preTxt, ngramDT, discount = 0.5) {
  
    #print(head(ngramDT))
    obs <- ngramDT[base == preTxt$preTxt]
    obsCount <- sum(obs$freq)
    
    obs %>% mutate(prob = (freq - discount) / obsCount)
    
}

getUnobsTails <- function(obs, uniDT) {

  uniDT[!(base %in% obs$predict)]
}

getUniTail <- function(preTxt, uniDT) {
  
  uniDT[base == preTxt$preTxtTail]

}

getAlphaBigram <- function(uniTailDT, biDT, discount = 0.5) {
  
  bigsThatStartWithUnig <- biDT[base == uniTailDT$base[1]]
  
  if(nrow(bigsThatStartWithUnig) < 1) 
    0
  else 
    1 - (sum(bigsThatStartWithUnig$freq - discount) / uniTailDT[1]$freq)
  
}

# 4.3

getBoBigrams <- function(preTxt, unObsTail) {
  unObsTail %>% 
    mutate(predict = base) %>%
    mutate(base = preTxt$preTxtTail) %>%
    mutate(preTxt = paste(base, predict, sep = "_")) %>% 
    select(base, predict, preTxt)
}

getObsBoBigrams <- function(boBi, bi) {
  bi[base %in% boBi$base] %>% 
    mutate(preTxt = paste(base, predict, sep = "_")) %>%
    filter(preTxt %in% boBi$preTxt)
}

getUnobsBoBigrams <- function(boBi, obsBoBi) {
  boBi %>% 
    filter(!(preTxt %in% obsBoBi$preTxt))
}

getObsBigProbs <- function(obsBoBigrams, uni, discount = 0.5) {
  
  uni <- uni[base %in% obsBoBigrams$base] %>%
    select(base, uniFreq = freq)
  
  obsBoBigrams %>%
    inner_join(uni, by = "base") %>%
    mutate(prob = (freq - discount) / uniFreq) %>%
    select(base, predict, prob)
    
}

getQboUnobsBigrams <- function(unobsBoBigrams, uni, alpha) {

  # get the unobserved bigram tails
  qboUnobsBigs <- unobsBoBigrams$predict
  w_in_Aw_iminus1 <- uni[!(base %in% qboUnobsBigs)]
  # convert to data.frame with counts
  qboUnobsBigs <- uni[base %in% qboUnobsBigs] %>% select(base, freq)
  
  denom <- sum(qboUnobsBigs$freq)

  unobsBoBigrams %>% 
    inner_join(qboUnobsBigs, by = c("predict" = "base")) %>%
    mutate(prob = alpha * freq / denom) %>% 
    select(base, predict, prob)
  
}

getAlphaTrigram <- function(obsTrigs, bigram, discount=0.5) {
  if(nrow(obsTrigs) < 1) return(1)
  alphaTri <- 1 - sum((obsTrigs$freq - discount) / bigram$freq[1])
  
  return(alphaTri)
}

getUnobsTriProbs <- function(preTxt, qboObsBigrams,
                             qboUnobsBigrams, alphaTrig) {
  #qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
  #qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
  #sumQboBigs <- sum(qboBigrams$prob)
  #first_bigPre_word <- str_split(bigPre, "_")[[1]][1]
  #unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$ngram, sep="_")
  #unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
  #unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
  
  #return(unobsTrigDf)
  
  qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams) %>% arrange(desc(prob))
  #debug(qboBigrams, title = "qboBigrams")
  sumQboBigs <- sum(qboBigrams$prob)
  unobsTrigNgrams <- qboObsBigrams %>% 
    mutate(base = paste(preTxt$preTxtBo, base, sep = "_"), prob = alphaTrig * prob / sumQboBigs) %>%
    select(base, predict, prob)
}

#predict <- function(q, uni, bi, tri, quad, pent) {
predict <- function(q) {

  lastSysTime <- Sys.time()
  info(title = "Start")
  
  ### Convert question to prediction text
  #ngram <- list(uni, bi, tri, quad, pent)
  ngram <- list(uni, bi, tri)
  maxngram <- length(ngram) - 1
  debug(maxngram, title = "maxngram")
  
  preTxt <- getPreTxt(q, maxngram)
  debug(preTxt, title = "preTxt")
  
  ### Get observation
  
  obs <- getObs(preTxt, ngram[[preTxt$nToks + 1]])
  debug(obs, "obs")
  unObsTail <- getUnobsTails(obs, ngram[[1]])
  debug(unObsTail, "unObsTail")
  uniTail <- getUniTail(preTxt, uni)
  debug(uniTail, "uniTail")
  alpha <- getAlphaBigram(uniTail, bi)
  
  debug(alpha, title = "alpha")
  
  boBi <- getBoBigrams(preTxt, unObsTail)
  debug(boBi, title = "boBi")
  
  obs_bo_bi <- getObsBoBigrams(boBi, bi)
  debug(obs_bo_bi, title = "obs_bo_bi")
  
  un_obs_bo_bi <- getUnobsBoBigrams(boBi, obs_bo_bi)
  debug(un_obs_bo_bi, title = "un_obs_bo_bi")
  
  obs_bo_bi_prob <- getObsBigProbs(obs_bo_bi, uni)
  
  debug(obs_bo_bi_prob, "obs_bo_bi_prob")
  un_obs_bo_bi_prob <- getQboUnobsBigrams(un_obs_bo_bi, uni, alpha)
  debug(un_obs_bo_bi_prob, "un_obs_bo_bi_prob")
  
  qbobi <- rbind(obs_bo_bi_prob, un_obs_bo_bi_prob)
  
  debug(qbobi, title = "qbobi")
  debug(sum(qbobi[-1,]$prob), title = "sum(qbobi[-1,]$prob)")
  
  # 4.4
  bigram <- bi[base == preTxt$preTxtBo & predict == preTxt$preTxtTail]
  debug(bigram, "bigram")
  alpha_trig <- getAlphaTrigram(obs, bigram)
  debug(alpha_trig, "alpha_trig")
   
  #4.5
  qbo_unobs_trigrams <- getUnobsTriProbs(preTxt, obs_bo_bi_prob,
                                         un_obs_bo_bi_prob, alpha_trig)
  debug(qbo_unobs_trigrams, title = "qbo_unobs_trigrams")
  
  #5
  obs <- obs %>% select(base, predict, prob)
  qbo_trigrams <- rbind(obs, qbo_unobs_trigrams) %>% arrange(desc(prob))
  #qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
  debug(qbo_trigrams, title = "5 qbo_trigrams")
  
  info(title = "End")
  
}