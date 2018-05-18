library(quanteda)
library(stringi)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(data.table)

## 2. Getting and Cleansing Data

### 2.1 Data Loading

loadData <- function() {
  
  url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  zipfile <- "Coursera-SwiftKey.zip"
  
  if(!file.exists(zipfile)){ 
    download.file(url, destfile = zipfile)
    unzip(zipfile, exdir = ".")
  }
  
  blogConn <- file("final/en_US/en_US.blogs.txt", "r") 
  newsConn <- file("final/en_US/en_US.news.txt", "r") 
  twitterConn <- file("final/en_US/en_US.twitter.txt", "r") 
  
  blog <- readLines(blogConn, skipNul = TRUE, encoding="UTF-8")
  news <- readLines(newsConn, skipNul = TRUE, encoding="UTF-8")
  twitter <- readLines(twitterConn, skipNul = TRUE, encoding="UTF-8")
  
  close(blogConn)
  close(newsConn)
  close(twitterConn)
  
  list("blog" = blog, "news" = news, "twitter" = twitter)
  
}

### 2.2 Sampling

sampling <- function(data, samplePercent = 10) {
  
  set.seed(88888)
  
  blog <- data$blog
  news <- data$news
  twitter <- data$twitter
  
  blogWordCount <- sum(stri_count_words(blog))
  newsWordCount <- sum(stri_count_words(news))
  twitterWordCount <- sum(stri_count_words(twitter))
  
  blogLineCount <- length(blog)
  newsLineCount <- length(news)
  twitterLineCount <- length(twitter)
  
  blog <- blog[rbinom(blogLineCount, 1, samplePercent / 100) == 1]
  news <- news[rbinom(newsLineCount, 1, samplePercent / 100) == 1]
  twitter <- twitter[rbinom(twitterLineCount, 1, samplePercent / 100) == 1]
  
  list("blog" = blog, "news" = news, "twitter" = twitter)
  
}

### 2.3 Invalid Character

removeInvalidChar <- function(data)
{
  
  blog <- iconv(data$blog, from = "latin1", to = "ASCII", sub="")
  news <- iconv(data$news, from = "latin1", to = "ASCII", sub="")
  twitter <- iconv(data$twitter, from = "latin1", to = "ASCII", sub="")
  
  list("blog" = blog, "news" = news, "twitter" = twitter)
  
}

### 2.4 Tokenization 

tokenize <- function(data) {
  
  all <- c(data$blog, data$news, data$twitter)
  toks <- tokens(char_tolower(all), remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)
  #toks <- tokens_wordstem(toks, language = quanteda_options("language_stemmer"))
  
  toks
  
}

### 2.4 Profanity Filtering

#### 2.4.1 Stop Words

removeStopWords <- function(toks) {
  toks <- tokens_remove(toks, stopwords('en'))
}

#### 2.4.2 Bad Words

removeBadWord <- function(toks) {
  
  badWordUrl <- "https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-text-file_2018_03_26_26.zip"
  badWordzipfile <- "full-list-of-bad-words-text-file.zip"
  
  download.file(badWordUrl, destfile = badWordzipfile)
  unzip(badWordzipfile, exdir = ".")
  badWordConn <- file("full-list-of-bad-words-text-file_2018_03_26.txt", "r") 
  
  badWord <- readLines(badWordConn)
  
  close(badWordConn)
  toks <- tokens_remove(toks, badWord, padding = TRUE)
  
}

### 2.5 Generate and save n-grams

splitWords <- function(dfm) {
  
  dt <- data.table(textstat_frequency(dfm) %>% 
    transform(base = sub("(.*)_(.*)", "\\1", feature), predict = sub("(.*)_(.*)", "\\2", feature)) %>% 
    select(base, predict, freq = frequency))

  setkey(dt, base)
  
  dt
  
}

generateNGram <- function(toks, n, fileName) {
  
  ngram <- splitWords(dfm(tokens_ngrams(toks, n = n)))
  save(ngram, file = fileName)
  
}

processData <- function(samplePercent = 10, removeStopWords = TRUE, removeBadWord = TRUE) {
  
  data <- loadData()
  
  if (samplePercent < 100)
    data <- sampling(data, samplePercent)
  
  data <- removeInvalidChar(data)
  toks <- tokenize(data)
  
  if (removeStopWords)
    toks <- removeStopWords(toks)
  
  if (removeBadWord)
    toks <- removeBadWord(toks)
  
  generateNGram(toks, 1, "ngram_uni.rds")
  generateNGram(toks, 2, "ngram_bi.rds")
  generateNGram(toks, 3, "ngram_tri.rds")
  generateNGram(toks, 4, "ngram_quad.rds")
  generateNGram(toks, 5, "ngram_pent.rds")
  
}


