library(tm)
library(quanteda)
library(tidytext)
library(dplyr)
library(tidyr)

### download the data
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
datafile <- "data/Coursera-SwiftKey.zip"

if(!file.exists('data')){
        dir.create('data')
}

if(!file.exists("data/final/en_US")){
        download.file(url, datafile)
        unzip(datafile, exdir = "data")
}

### create datasets
blogFileName <- "data/final/en_US/en_US.blogs.txt"
con <- file(blogFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

newsFileName <- "data/final/en_US/en_US.news.txt"
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

twitterFileName <- "data/final/en_US/en_US.twitter.txt"
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

rm(con)


### select sample dataset

# set seed for reproducability and assign sample size
set.seed(10086)
sampleSize <- 0.1

# sample all three data sets
sampleBlogs <- sample(blogs, length(blogs)* sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news)* sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter)* sampleSize, replace = FALSE)

# remove unneeded file to save space
rm(news, blogs, twitter)

# remove all non-English characters from the sampled data
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

# remove outliers such as very long and very short articles by only including the IQR
removeOutliers <- function(data) {
        first <- quantile(nchar(data), 0.25)
        third <- quantile(nchar(data), 0.75)
        data <- data[nchar(data) > first]
        data <- data[nchar(data) < third]
        return(data)
}

sampleBlogs <- removeOutliers(sampleBlogs)
sampleNews <- removeOutliers(sampleNews)
sampleTwitter <- removeOutliers(sampleTwitter)

# combine all three data sets into a single data set
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)

writeLines(sampleData, con="data/sampledata.txt")


### Clean data

# create profanity words file
badWordsURL <- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
badWordsFile <- "data/profanity.txt"
if (!file.exists('data')) {
        dir.create('data')
}
if (!file.exists(badWordsFile)) {
        download.file(badWordsURL, badWordsFile)
}

profanity <- readLines("data/profanity.txt")
profanity <- iconv(profanity, "latin1", "ASCII", sub = "")

# clean dataset
cleanText <- function(dataset){
        input <- tolower(dataset)
        
        # remove URL, email addresses, Twitter handles and hash tags
        input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove ordinal numbers
        input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove profane words
        input <- removeWords(input, profanity)
        
        # remove punctuation
        input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove punctuation (leaving ')
        input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
        
        # trim leading and trailing whitespace
        input <- gsub("^\\s+|\\s+$", "", input)
        input <- stripWhitespace(input)
        
        return(input)
}

tidySampleData <- cleanText(sampleData)
writeLines(tidySampleData , con="data/tidySampleData.txt")

# remove unneeded file to save space
rm(sampleBlogs,sampleNews,sampleTwitter,sampleData)

## build corpus and n-grams
corpus <- corpus(tidySampleData)
text_df <- tidy(corpus)

## unigrams
gram1 <- text_df %>%  unnest_tokens(w1, text) %>% count(w1, sort = TRUE)
saveRDS(gram1, file = "data/gram1.RData")

gram2 <- text_df %>%  unnest_tokens(words, text, token = "ngrams", n = 2) %>% count(words, sort = TRUE)
gram2 <- gram2 %>%  separate(words, c("w1", "w2"), " ")
saveRDS(gram2, file = "data/gram2.RData")

gc()
gram3 <- text_df %>%  unnest_tokens(words, text, token = "ngrams", n = 3) %>% count(words, sort = TRUE)
gram3 <- gram3 %>%  separate(words, c("w1", "w2", "w3"), " ")
saveRDS(gram3, file = "data/gram3.RData")

gc()
gram4 <- text_df %>%  unnest_tokens(words, text, token = "ngrams", n = 4) %>% count(words, sort = TRUE)
gram4 <- gram4[-2,] #Remove NA row
gram4 <- gram4 %>%  separate(words, c("w1", "w2", "w3", "w4"), " ")
saveRDS(gram4, file = "data/gram4.RData")



