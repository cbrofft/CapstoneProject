library("tm")
library("quanteda")
library("data.table")



# Set WD to project folder
setwd("~/Data Science Capstone/FInalProjectWordPredict")
getwd()


# Check if raw data exists and if it does not download from source location
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                destfile = "Coursera-SwiftKey.zip")
}

# Check if files have been unzipped if not unzip them
if (!file.exists("final/en_US/en_US.blogs.txt")) {
  unzip("Coursera-SwiftKey.zip", exdir = ".")
}


# Load data and analyze basic information around the data

# read in blogs data
conn <- file("final/en_US/en_US.blogs.txt", open = "rb")
blogs_data <- readLines(conn, encoding = "UTF-8")
close(conn)

# read in news data
conn <- file("final/en_US/en_US.news.txt", open = "rb")
news_data <- readLines(conn, encoding = "UTF-8")
close(conn)

# read in twitter data
conn <- file("final/en_US/en_US.twitter.txt", open = "rb")
twitter_data <- readLines(conn, encoding = "UTF-8")
close(conn)





blogs <-as.matrix(blogs_data)
twitter <- as.matrix(twitter_data)
news <- as.matrix(news_data)

Total_data <- do.call("rbind", list(blogs, twitter, news))


Sample_Data <- sample(Total_data, 20000, replace = FALSE, prob = NULL)
Corp_Sample <- Corpus(VectorSource(Sample_Data))




# the puncuations and numbers in the texts were removed as there is no need to predict punctations or numbers
Predict_Tokens <- tokens(
  x = tolower(Corp_Sample),
  remove_punct = TRUE,
  remove_twitter = TRUE,
  remove_numbers = TRUE,
  remove_hyphens = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
)

clean_words <- tokens_wordstem(Predict_Tokens, language = "english")


Two_Gram <- tokens_ngrams(clean_words, n = 2)
Three_Gram <- tokens_ngrams(clean_words, n = 3)

One_dfm <- dfm(clean_words)
Two_dfm <- dfm(Two_Gram)
Three_dfm<- dfm(Three_Gram)


One_dfm <- dfm_trim(One_dfm, 3)
Two_dfm <- dfm_trim(Two_dfm, 3)
Three_dfm <- dfm_trim(Three_dfm, 3)

# Create named vectors with counts of words 
Count_OneWords <- colSums(One_dfm)
Count_TwoWords<- colSums(Two_dfm)
Count_ThreeWords <- colSums(Three_dfm)

# Create data tables with individual words as columns
One_Words <- data.table(word_1 = names(Count_OneWords), count = Count_OneWords)

Two_Words <- data.table(
  word_1 = sapply(strsplit(names(Count_TwoWords), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(Count_TwoWords), "_", fixed = TRUE), '[[', 2),
  count = Count_TwoWords)

Three_Words <- data.table(
  word_1 = sapply(strsplit(names(Count_ThreeWords), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(Count_ThreeWords), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(Count_ThreeWords), "_", fixed = TRUE), '[[', 3),
  count = Count_ThreeWords)


setkey(One_Words, word_1)
setkey(Two_Words, word_1, word_2)
setkey(Three_Words, word_1, word_2, word_3)




discount_value <- 0.75

######## Finding Bi-Gram Probability #################

# Finding number of bi-gram words
numOfTwoGrams <- nrow(Two_Words[by = .(word_1, word_2)])

# Dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
# ( Finding probability for a word given the number of times it was second word of a bigram)
ckn <- Two_Words[, .(Prob = ((.N) / numOfTwoGrams)), by = word_2]
setkey(ckn, word_2)

# Assigning the probabilities as second word of bigram, to unigrams
One_Words[, Prob := ckn[word_1, Prob]]
One_Words <- One_Words[!is.na(One_Words$Prob)]

# Finding number of times word 1 occurred as word 1 of bi-grams
n1wi <- Two_Words[, .(N = .N), by = word_1]
setkey(n1wi, word_1)

# Assigning total times word 1 occured to bigram cn1
Two_Words[, Cn1 := One_Words[word_1, count]]

# Kneser Kney Algorithm
Two_Words[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * n1wi[word_1, N] * One_Words[word_2, Prob])]

######## End of Finding Bi-Gram Probability #################


######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 
Three_Words[, Cn2 := Two_Words[.(word_1, word_2), count]]

# Finding count of word1-word2 combination in trigram
n1w12 <- Three_Words[, .N, by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
Three_Words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * n1w12[.(word_1, word_2), N] *
              Two_Words[.(word_1, word_2), Prob]]

######## End of Finding Tri-Gram Probability #################



# Finding the most frequently used 50 unigrmas
One_Words <- One_Words[order(-Prob)][1:50]


# Prediction App
ThreeWords <- function(w1, w2, n = 5) {
  pwords <- Three_Words[.(w1, w2)][order(-Prob)]
  if (any(is.na(pwords)))
    return(TwoWords(w2, n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_3])
  count <- nrow(pwords)
  Twwords <- TwoWords(w2, n)[1:(n - count)]
  return(c(pwords[, word_3], Twwords))
}

# function to return highly probable previous word given a word
TwoWords <- function(w1, n = 5) {
  pwords <- Three_Words[w1][order(-Prob)]
  if (any(is.na(pwords)))
    return(OneWords(n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_2])
  count <- nrow(pwords)
  OWords <- OneWords(n)[1:(n - count)]
  return(c(pwords[, word_2], OWords))
}

# function to return random words from unigrams
OneWords <- function(n = 5) {  
  return(sample(One_Words[, word_1], size = n))
}

# The prediction app
getWords <- function(str){
  require(quanteda)
  tokens <- tokens(x = char_tolower(str))
  tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
  
  words <- ThreeWords(tokens[1], tokens[2], 5)
  chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")
  
  print(words[1])
}