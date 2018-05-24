#
# Step 1: Collect Data
#
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors=FALSE)
#str(sms_raw)

# Turn type column into factors for "ham" and "spam"
sms_raw$type <- factor(sms_raw$type)
#str(sms_raw$type)

table(sms_raw)

#
# Step 2: Prepare Data
#

# Use TextMining Package to process SMS messages
library(tm)

# Corpus for tm package of all SMS messages
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
# print(sms_corpus)
# inspect(sms_corpus[1:3])

# Clean/Normalize text
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

#inspect(corpus_clean[1:3])

# Tokenize cleaned text
dtm_control <- list(removePunctuation = TRUE, stopwords = TRUE)
sms_dtm <- DocumentTermMatrix(as.vector(corpus_clean), control = dtm_control)

# Build training and test data sets
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4179:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4179:5559,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4179:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Visualize tokens with wordcloud
library(wordcloud)

wordcloud(sms_corpus_train, min.freq=40, random.order=FALSE)

spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam[,2], min.freq=40, scale=c(3, 0.5))
wordcloud(ham[,2], min.freq=40, scale=c(3, 0.5))

# Frequent terms

sms_dict <- list(findFreqTerms(sms_dtm_train, 5))

sms_train <- DocumentTermMatrix(sms_corpus_train) #, control=list(dictionary = sms_dict[[1]]))
sms_test  <- DocumentTermMatrix(sms_corpus_test) #, control=list(dictionary = sms_dict[[1]]))


convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(1,0), labels=c("No", "Yes"))
  return(x)
}

sms_train <- apply(sms_train, MARGIN=2, convert_counts)
sms_test <- apply(sms_test, MARGIN=2, convert_counts)

#
# Step 3: Train
#

# NaiveBayes from install.packages("e1071")
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

#
# Step 4: Evaluate Performance
#

sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)

CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#
# Step 5: Improve Performance
#

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
