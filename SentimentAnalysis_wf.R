#Data Collection
#1. Installing and loading required libraries for data collection from Twitter API
#install.packages("twitteR")
#install.packages("ROAuth")
library(twitteR)
library(ROAuth)

#2. Authentication of Twitter Application
consumerKey <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
consumerSecret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
accessToken <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
accessSecret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

my_oauth <- setup_twitter_oauth(consumer_key = consumerKey,
                                consumer_secret = consumerSecret,
                                access_token = accessToken,
                                access_secret = accessSecret)

#3. Searching tweets by using annotations and transforming it into a well-organized dataframe.
tweet <- searchTwitter("@ktm_berhad", n = 800, lang = "en")
tweet_KTM <- twListToDF(tweet)
tweet <- searchTwitter("@MRTMalaysia", n = 800, lang = "en")
tweet_MRT <- twListToDF(tweet)
tweet <- searchTwitter("@AskRapidKL", n = 800, lang = "en" )
tweet_rapid <- twListToDF(tweet)
tweet <- searchTwitter("@MyRapidKL", n = 800, lang = "en" )
tweet_myrapid <- twListToDF(tweet)
tweet <- searchTwitter("@transitmy", n = 800, lang = "en" )
tweet_transit <- twListToDF(tweet)
data_transit_rapid <- rbind(tweet_KTM, tweet_MRT, tweet_rapid, tweet_myrapid, tweet_transit)

#4. Writting the data collected into a new csv.
write.csv(data_transit_rapid, "sentiment_analysis_data_1.csv")

#Exploratory Data Analysis
#1. Installing and loading required libraries for exploratory data analysis.
#install.packages("dplyr")
#install.packages("lubridate")
library(dplyr)
library(lubridate)

#2. Importing dataset into environment.
data1 <- read.csv("sentiment_analysis_data_1.csv", row.names = 1)
data2 <- read.csv("sentiment_analysis_data_2.csv", row.names = 1)
data3 <- read.csv("sentiment_analysis_data_3.csv", row.names = 1)

#3. Appending all 12 weeks data together into a singe csv file.
data_all  <- rbind(data1, data2, data3)
head(data_all)

#4. Investigating the date of tweets created.
options(max.print = 10000)
data_all$created <- ymd_hms(data_all$created)
str(data_all$created)
data_all$created <- sort(data_all$created)
head(data_all$created, 5)
tail(data_all$created, 5)

#5. Eliminating retweets from dataset.
summary(data_all$isRetweet)
summary(data_all$retweeted)
data_all <- filter(data_all, isRetweet == "FALSE")

#6. Selecting required variables for sentiment analysis and removing all unnecessary variables. In this case, only text variable is needed for sentiment analysis.
data_all <- data_all[1]
head(data_all)

#Data Preprocessing
#1. Importing the prepared dataset into environment.
data <- read.csv('C:/Users/user/Desktop/Research/12weeks_data.csv')

#2. Removing urls, punctuations, whitespaces, hastags, numbers, annotations from tweets.
clean_tweets <- function(data){
  data <- gsub("<[^>]{0,}>|\\r|\\n)", "", data)
  data <- gsub("@\\w+", "", data)  
  data <- gsub("#\\w+", "", data)
  data <- gsub("[[:punct:]]", "", data)
  data <- gsub("http\\w+", "", data)
  data <- gsub("[ \t]{2,}", "", data)
  data <- gsub("^\\s+|\\s+$", "", data)
  data <- gsub("&amp", "", data)
  data <- gsub("â???Tm", "", data)
  data <- gsub("â???Ts", "", data)
  data <- tolower(data)
  return(data)
}
data <- as.data.frame(apply(data, 2, clean_tweets))
colnames(data) <- "text"

#3. Correcting some of the wrong-spelling frequent words in tweets.
data$text <- gsub("pleas", "please", data$text)
data$text <- gsub("pls", "please", data$text)
data$text <- gsub("improv", "improve", data$text)
data$text <- gsub("servic", "service", data$text)
data$text <- gsub("issu", "issue", data$text)
data$text <- gsub("alreadi", "already", data$text)
data$text <- gsub('minut', 'minute', data$text)

#Lexical-based Sentiment Analysis
#1. Installing and loading required packages.
#install.packages("syuzhet")
library(syuzhet)

#2. Using four different dictionary in calculating the polarity of tweets.
sentiment <- function(data){
  syuzhet <- get_sentiment(data, method = "syuzhet")
  bing <- get_sentiment(data, method = "bing")
  afinn <- get_sentiment(data, method = "afinn")
  sentiments <- data.frame(syuzhet, bing, afinn)
  return(sentiments)
}
data <- cbind(data, sentiment(data = data$text))

#Corpus and Document Term Matrix Creation
#1. Installing and loading all required libraries.
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("dplyr")
library(tm)
library(SnowballC)
library(dplyr)

#2. Categorizing each sentiment scores into 3 categories.
data$syuzhet_polarity <- ifelse(data$syuzhet>0, 1, ifelse(data$syuzhet<0, -1, 0))
data$bing_polarity <- ifelse(data$bing>0, 1, ifelse(data$bing<0, -1, 0))
data$afinn_polarity <- ifelse(data$afinn>0, 1, ifelse(data$afinn<0, -1, 0))

#3.Encoding the target feature as factor.
data$syuzhet_polarity <- factor(data$syuzhet_polarity, levels = c(-1,0,1))
data$bing_polarity <- factor(data$bing_polarity, levels = c(-1,0,1))
data$afinn_polarity <- factor(data$afinn_polarity, levels = c(-1,0,1))

#4. Building a corpus.
corpus <- Corpus(VectorSource(data$text))

#5. Preprocessing of corpus.
corpus.clean <- corpus %>% 
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords())%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)

#6. Building a Document Term Matrix (DTM) from the corpus.
dtm <-DocumentTermMatrix(corpus.clean)

#7. Removing sparse term from the Document Term Matrix (DTM).
inspect(dtm)
sparse_dtm <- removeSparseTerms(dtm, 0.995)

#8. Converting the sparse matrix into dataframe so that it can be used for the predictive models.
data_sparse <- as.data.frame(as.matrix(sparse_dtm), stringsAsFactors = FALSE)
colnames(data_sparse) <- make.names(colnames(data_sparse))
data_sparse$syuzhet_polarity <- data$syuzhet_polarity
data_sparse$bing_polarity <- data$bing_polarity
data_sparse$afinn_polarity <- data$afinn_polarity

#Implementation of Machine Learning Algorithms with differenct lexical dictionaries.
#1. Installing and loading all required libraries.
#install.packages("caTools")
#install.packages("e1071")
#install.packages("rpart")
#install.packages("class")
#install.packages("randomForest")
library(caTools)
library(e1071)
library(rpart)
library(class)
library(randomForest)

#2. Splitting data into training set and testing set.
set.seed(123)
split <- sample.split(data$afinn_polarity, SplitRatio = 0.6)
train_sparse <- subset(data_sparse, split == TRUE)
test_sparse <- subset(data_sparse, split == FALSE)

#3. Implementing Decision Tree, Random Forest and Support Vector Machine algorithms with 'syuzhet' lexical.
train_sparse_syuzhet <- subset(train_sparse, select = -c(bing_polarity, afinn_polarity))
test_sparse_syuzhet <- subset(test_sparse, select = -c(bing_polarity, afinn_polarity))
classifier_CART_syuzhet <- rpart(syuzhet_polarity ~., data = train_sparse_syuzhet, method = "class")
predict_CART_syuzhet <- predict(classifier_CART_syuzhet, newdata = test_sparse_syuzhet, type = "class")
confusionmatrix_CART_syuzhet <- table(test_sparse_syuzhet$syuzhet_polarity, predict_CART_syuzhet)
confusionmatrix_CART_syuzhet
classifier_RF_syuzhet <- randomForest(syuzhet_polarity~., data = train_sparse_syuzhet)
predict_RF_syuzhet <- predict(classifier_RF_syuzhet, newdata = test_sparse_syuzhet)
confusionmatrix_RF_syuzhet <- table(test_sparse_syuzhet$syuzhet_polarity, predict_RF_syuzhet)
confusionmatrix_RF_syuzhet
classifier_svm_syuzhet <- svm(train_sparse_syuzhet$syuzhet_polarity~., data = train_sparse_syuzhet, 
                              kernel = "linear", scale = FALSE)
predict_svm_syuzhet <- predict(classifier_svm_syuzhet, newdata = test_sparse_syuzhet)
confusionmatrix_svm_syuzhet <- table(test_sparse_syuzhet$syuzhet_polarity, predict_svm_syuzhet)
confusionmatrix_svm_syuzhet

#4. Implementing Decision Tree, Random Forest and Support Vector Machine algorithms with 'bing' lexical.
train_sparse_bing <- subset(train_sparse, select = -c(syuzhet_polarity, afinn_polarity))
test_sparse_bing <- subset(test_sparse, select = -c(syuzhet_polarity, afinn_polarity))
classifier_CART_bing <- rpart(bing_polarity ~., data = train_sparse_bing, method = "class")
predict_CART_bing <- predict(classifier_CART_bing, newdata = test_sparse_bing, type = "class")
confusionmatrix_CART_bing <- table(test_sparse_bing$bing_polarity, predict_CART_bing)
classifier_RF_bing <- randomForest(bing_polarity~., data = train_sparse_bing)
predict_RF_bing <- predict(classifier_RF_bing, newdata = test_sparse_bing)
confusionmatrix_RF_bing <- table(test_sparse_bing$bing_polarity, predict_RF_bing)
classifier_svm_bing <- svm(train_sparse_bing$bing_polarity~., data = train_sparse_bing, 
                           kernel = "linear", scale = FALSE)
predict_svm_bing <- predict(classifier_svm_bing, newdata = test_sparse_bing)
confusionmatrix_svm_bing <- table(test_sparse_bing$bing_polarity, predict_svm_bing)

#5. Implementing Decision Tree, Random Forest and Support Vector Machine algorithms with 'afinn' lexical.
train_sparse_afinn <- subset(train_sparse, select = -c(syuzhet_polarity, bing_polarity))
test_sparse_afinn <- subset(test_sparse, select = -c(syuzhet_polarity, bing_polarity))
classifier_CART_afinn <- rpart(afinn_polarity ~., data = train_sparse_afinn, method = "class")
predict_CART_afinn <- predict(classifier_CART_afinn, newdata = test_sparse_afinn, type = "class")
confusionmatrix_CART_afinn <- table(test_sparse_afinn$afinn_polarity, predict_CART_afinn)
classifier_RF_afinn <- randomForest(afinn_polarity~., data = train_sparse_afinn)
predict_RF_afinn <- predict(classifier_RF_afinn, newdata = test_sparse_afinn)
confusionmatrix_RF_afinn <- table(test_sparse_afinn$afinn_polarity, predict_RF_afinn)
classifier_svm_afinn <- svm(train_sparse_afinn$afinn_polarity~., data = train_sparse_afinn, 
                            kernel = "linear", scale = FALSE)
predict_svm_afinn <- predict(classifier_svm_afinn, newdata = test_sparse_afinn)
confusionmatrix_svm_afinn <- table(test_sparse_afinn$afinn_polarity, predict_svm_afinn)

#Performance Metrics
#1. Constructing a confusion matrix for each algorithm with each lexical dictionary.
#install.packages('caret')
library(caret)
CART_syuzhet <- confusionMatrix(test_sparse_syuzhet$syuzhet_polarity, predict_CART_syuzhet)
CART_bing <- confusionMatrix(test_sparse_bing$bing_polarity, predict_CART_bing)
CART_afinn <- confusionMatrix(test_sparse_afinn$afinn_polarity, predict_CART_afinn)
RF_syuzhet <- confusionMatrix(test_sparse_syuzhet$syuzhet_polarity, predict_RF_syuzhet)
RF_bing <- confusionMatrix(test_sparse_bing$bing_polarity, predict_RF_bing)
RF_afinn <- confusionMatrix(test_sparse_afinn$afinn_polarity, predict_RF_afinn)
SVM_syuzhet <- confusionMatrix(test_sparse_syuzhet$syuzhet_polarity, predict_svm_syuzhet)
SVM_bing <- confusionMatrix(test_sparse_bing$bing_polarity, predict_svm_bing)
SVM_afinn <- confusionMatrix(test_sparse_afinn$afinn_polarity, predict_svm_afinn)

#3. Investigating the confusion matrix of algorithms with each lexical.
performance <- function(data){
  data_byclass <- data
  data_byclass_avg <- as.data.frame(colMeans(data_byclass))
  return(data_byclass_avg)
}

CART_syuzhet_byclass <- performance(CART_syuzhet$byClass)
colnames(CART_syuzhet_byclass) <- 'CART_syuzhet'

RF_syuzhet_byclass <- performance(RF_syuzhet$byClass)
colnames(RF_syuzhet_byclass) <- 'RF_syuzhet'

SVM_syuzhet_byclass <- performance(SVM_syuzhet$byClass)
colnames(SVM_syuzhet_byclass) <- 'SVM_syuzhet'

CART_bing_byclass <- performance(CART_bing$byClass)
colnames(CART_bing_byclass) <- 'CART_bing'

RF_bing_byclass <- performance(RF_bing$byClass)
colnames(RF_bing_byclass) <- 'RF_bing'

SVM_bing_byclass <- performance(SVM_bing$byClass)
colnames(SVM_bing_byclass) <- 'SVM_bing'

CART_afinn_byclass <- performance(CART_afinn$byClass)
colnames(CART_afinn_byclass) <- 'CART_afinn'

RF_afinn_byclass <- performance(RF_afinn$byClass)
colnames(RF_afinn_byclass) <- 'RF_afinn'

SVM_afinn_byclass <- performance(SVM_afinn$byClass)
colnames(SVM_afinn_byclass) <- 'SVM_afinn'

result <- cbind(CART_syuzhet_byclass, RF_syuzhet_byclass, SVM_syuzhet_byclass,
                CART_bing_byclass, RF_bing_byclass, SVM_bing_byclass,
                CART_afinn_byclass, RF_afinn_byclass, SVM_afinn_byclass)

accuracy <- cbind(CART_syuzhet$overall['Accuracy'], RF_syuzhet$overall['Accuracy'], 
                  SVM_syuzhet$overall['Accuracy'], CART_bing$overall['Accuracy'], 
                  RF_bing$overall['Accuracy'], SVM_bing$overall['Accuracy'],
                  CART_afinn$overall['Accuracy'], RF_afinn$overall['Accuracy'], 
                  SVM_afinn$overall['Accuracy'])
colnames(accuracy) <- c('CART_syuzhet', 'RF_syuzhet', 'SVM_syuzhet',
                        'CART_bing', 'RF_bing', 'SVM_bing',
                        'CART_afinn', 'RF_afinn', 'SVM_afinn')
rbind(accuracy, result)

#4. Visualizing the results of performance metrics.
library(tidyverse)
a <- rbind(accuracy, result)
a_trans <- a %>% gather(Algorithms, Results, CART_syuzhet:SVM_afinn)
a_trans$Measures <- rep(c('Accuracy','Sensitivity','Specificity','Pos Pred Value', 'Neg Pred Value',
                          'Precision','Recall','F1','Prevalence','Detection Rate','Detection Prevalence',
                          'Balanced Accuracy'), 9)
selected_measure <- c('Precision', 'Recall', 'F1', 'Accuracy')
a_trans_selected <- a_trans %>% filter(Measures %in% selected_measure)

library(RColorBrewer)
mycolor <- brewer.pal(4, "Set1")
names(mycolor) <- levels(a_trans_selected$Algorithms)
colscale <- scale_color_manual(name='Algorithms', values = mycolor)

ggplot(a_trans_selected, aes(Algorithms, Results, fill = Algorithms)) + colscale + ggtitle("Performance Metrics Results") + theme(plot.title = element_text(hjust = 0.5)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(.~Measures)

library(stringr)
a_trans_selected_afinn <- a_trans_selected %>% filter(str_detect(Algorithms, 'afinn'))

ggplot(a_trans_selected_afinn, aes(Algorithms, Results, fill = Algorithms)) + colscale + ggtitle("Comparison between Machine Learning Algorithms") + theme(plot.title = element_text(hjust = 0.5)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid((.~Measures))

#Visualizing with word cloud.
#1. Creating a word cloud.
library(wordcloud)
tdm <- TermDocumentMatrix(corpus.clean)
tdm$polarity <- data$afinn_polarity
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
d

#4. Creating a frequency plot for each polarity.
library(tidytext)
data_bigrams <- data %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
data_bigrams_separated <- data_bigrams %>% separate(bigram, c('word1', 'word2'), sep = " ")
data_bigrams_filtered <- data_bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
bigrams_count <- data_bigrams_filtered %>% count(word1, word2, sort = TRUE)
afinn <- get_sentiments('afinn')
c <- bigrams_count %>% inner_join(afinn, by=c(word1='word')) 
d <- bigrams_count %>% inner_join(afinn, by=c(word2='word'))
z <- unique(rbind(c,d))
z <- z %>% unite(bigram, word1, word2, sep = " ")
z$polarity <- ifelse(z$score>0, 1, 0)
bigram_pos <- z %>% filter(polarity==1)
bigram_pos <- bigram_pos %>% arrange(desc(n))
bigram_neg <- z %>% filter(polarity==0)
bigram_neg <- bigram_neg %>% arrange(desc(n))
bigram_pos %>% head(30) %>% ggplot(aes(x = reorder(bigram, n), y = n)) + geom_col(show.legend = FALSE) + xlab("Bigram") + ylab("Frequency") + coord_flip()
bigram_neg %>% head(30) %>% ggplot(aes(x = reorder(bigram, n), y = n)) + geom_col(show.legend = FALSE) + xlab("Bigram") + ylab("Frequency") + coord_flip()

#2. Presenting the word cloud.
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words = 200, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#3. Total number of positive, neutral and negative tweets.
pos_neu_neg <- data %>% group_by(afinn_polarity) %>% tally()
pos_neu_neg
barplot(pos_neu_neg$n, main = 'Number of Positive & Neutral & Negative Tweets', col = 'darkgreen',
        xlab = 'Afinn Polarity', ylab = 'Count', names.arg = c('Negative', 'Neutral', 'Positive'))





