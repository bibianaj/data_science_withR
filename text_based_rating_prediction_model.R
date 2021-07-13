####################################################################################################
# JEONG EUNBI (TP052184)
# MSc. DSBA - Text Based Rating Prediction to assign  Symmetrical Rating for Asymmetric Reviews
# Start Date : 2nd Feb 2021
# End Date : 8th Mar 2021
####################################################################################################
install.packages("stringr", dependencies = TRUE)
library(NLP)
library(tm)
library(stringr)
library(stopwords)
library(sentimentr)
library(SnowballC) # stemming
library(quanteda)
library(spellcheckr) # spelling correction
library(wordcloud)
library(topicmodels)


# Import CSV file
dataset <- read.csv('C:\\Users\\eunvi\\Documents\\APU_Master\\CP\\shopee_reviews.csv\\test_30000.csv', na.strings = c(""), stringsAsFactors = FALSE)
View(dataset)
str(dataset)
summary(dataset)
dim(dataset)

#################################################################
# STEP 1 - TEXT PRE-PROCESSING
#################################################################


# 1-1: Remove null - rating/reviews
is.na(dataset)
table(is.na(dataset))
table(is.na(dataset$label))
table(is.na(dataset$text))


# Prepare my stopwords list
my_stopwords <- head(stopwords::stopwords("en"), 87)
#my_stopwords <- stopwords::stopwords("en")
#quanteda::char_edit(my_stopwords("en", source ="snowball")) #edit stopword list


#Spelling correction
correct("goo")

spelling_checker <- function(text) {
  
  w <- unlist(str_split(text, " "))
  len <- length(w)
  
  result <- ""
  for ( i in 1:len) {
    
    corrected <- correct(w[i])
    #print(corrected)
    result = paste(result, corrected)
    #print(result)
  }
  return(str_trim(result))
}

spelling_checker('very goo materials i bought grey and black will buyore again')


# Text Pre-processing function
clean_text <- function(text) {
  # 1-1: Lowercasing for text
  temp <- tolower(text)
  
  # 1-5: Replace online slang
  temp <- replace_internet_slang(temp)
  temp <- replace_emoticon(temp)
  
  
  # 1-2: Remove special characters
  # 1-4: Remove number
  temp <- str_replace_all(temp, "[^A-Za-z\\s,.]", "")
  temp <- str_replace_all(temp, "  ", " ")
  
  # 1.3: Remove stop words
  temp <- removeWords(temp, my_stopwords)
  
  # 1-6: Word Stemming
  # The Porter algorithm returns incorrect stemmed result.
  # It affected word itself so affected sentiemnt score too.
  # temp <- stemDocument(temp, "english") 
  
  # trim
  temp <- str_trim(temp)
  
  return(temp)
}



replace_emoticon('comfy material, stretchable, but padding abit dented :( ')

# Print cleaned reviews
for (text in dataset$text) {
  result <- clean_text(text)
  print(result)
  #print(sentiment(result))
  #print(emotion(result))
}

new_dataset <- NULL

for (i in 1:dim(dataset)[1]) {
  #print(dataset$label[i])
  #print(dataset$text[i])
  
  cleanedText <- clean_text(dataset$text[i])
  sentiment_result <- sentiment_by(cleanedText)
  
  text = cleanedText
  label = dataset$label[i]
  senti_score = sentiment_result$ave_sentiment
  
  new_dataset = rbind(new_dataset, data.frame(text, label, senti_score))
  
}

View(new_dataset)




#################################################################
# STEP 2 - Detecting asymmetric Rating and Review
#################################################################

# Create new data.frame with cleaned review and sentiment 

# newReviewList ← list of Reviews without asymmetric reviews
# asymmetricReviewList ← list for asymmetric reviews

# Create word cloud to create my_key
texts <- c(dataset$text)
docs = Corpus(VectorSource(texts))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)

matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=300, random.order=FALSE, rot.per=0.2, color=brewer.pal(8, "Dark2"))



# Create my key
set.seed(10)
key <- data.frame(
  words = sample(letters),
  polarity = rnorm(26),
  stringsAsFactors = FALSE
)

is_key(key)

myKey <- as_key(key)
is_key(myKey)


mykey_added <- update_key(myKey, drop =c ("a", "b", "c", "d", "e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"))

mykey_added <- update_key(mykey_added, 
                          x = data.frame(
                            x = c("satisfied", "buy again", "not expensive", "fast", "disappointed", "not bad", "gd", "not too bad", "goof", "not try",
                                  "thnk u", "nicely packed", "dint get", "didnt get", "great", "awkward", "fast delivery", "look nice", "fits well", "received wrong",
                                  "poor", "really bad", "thanks", "looks good", "comfortable", "scratches", "delivery bad", "cheated", "good quality", "worth", "wrong",
                                  "not good", "loove", "good", "well", "nice", "perfectly", "perfect", "as advertised", "very well", "happy",
                                  "nicely", "love", "luv","smiley", "free", "awesome", "responsive", "affordable", "hell", "quickly",
                                  "resonable", "okay", "ok", "dont regret", "reasonable","havent tried", "no bad", "cheap", "recomended",
                                  "no regret", "not too", "have not tried", "not yet", "never fails", "never fail", "havent try", "yet", "havent use", "cant wait",
                                  "no defect", "excellent", "low quality", "not recommend", 'misleading', "dirty", "rubbish quality", "crap", "not worth", "flimsy",
                                  "not receive", "not aesthetically pretty", "faulty", "not like", "slow", "decent", "yet to try", "disappointing",
                                  "delicate", "but not sure", "will deal again", "really good", "doesnt ride up", "worthy", "best", "bad",
                                  "late", "not tried", "as per description", "doesnt disappoint", "safely", "cannot wait to try", "acceptable", "good value", "no complain",
                                  "good condition", "fantastic", "not damaged", "value for money", "quite ok", "understandably", "not bad", "not too bad", "fine",
                                  "havent fix", "second time", "recommended", "not recommended", "looks ok", "looked ok", "nd time ordering", "received in good condition"), 
                            y = c(1, 1, 1, 1, -1, 0.5, 1, 0.5, 1, 0,
                                  1, 1, -0.5, -0.5, 1, -0.5, 1, 0.5, 0.5, -1,
                                  -1, -1, 1, 0.5, 1, -0.5, -0.5, -1, 1, 0.5,
                                  -1, -1, 1, 1, 0.5, 0.5, 1, 1, 0.5,1,
                                  0.5,0.5,1,1,0.5, 0.3, 0.5, 0.5, 1, -0.5,
                                  1, 1, 0.5, 0.5,0.5,1, 0, 0.5, 0.5, 1,
                                  1, 0.5,0,0,0,0,0,0,0,0,
                                  0, 1, -1, -1, -0.5,-1,-1, -1,-1,-1,
                                  -1, -0.5,-1,-0.5, -0.5, 1,0,-0.5,
                                  -0.2,0,0.5,1,0.5, 1, 1, -1,
                                  -0.5,0, 0.5, 1, 0.5, 0.5, 0.5, 0.5, 1,
                                  1,1,0,0.5, 0.5,0.5, 0.5,0.5,1,
                                  0, 1,1,-1, 0.5,0.5,1, 1)
                          ))



sentiment_by('very fast delivery and items also good')
sentiment_by('received.  not tried  but looks decent..', polarity_dt = mykey_added)

#############################################################################
# Sentiment score comparision between cleaend text and normal text
#############################################################################


sentiment_by("See nice, let my friend try ðŸ˜")

clenedText_test <- clean_text("See nice, let my friend try ðŸ˜")
sentiment_by(clenedText_test)


# Return two lists
newReviewList <- NULL
asymmetricReviewList <- NULL


for (i in 1:dim(dataset)[1]) {
  
  cleanedText <- clean_text(dataset$text[i])
  sentimentScore <- sentiment_by(cleanedText, polarity_dt = mykey_added)
  #sentimentScore <- sentiment_by(cleanedText,  polarity_dt = lexicon::hash_sentiment_nrc)
  #sentimentScore <- sentiment_by(cleanedText)
  
  text = cleanedText
  label = dataset$label[i]
  senti_score = sentimentScore$ave_sentiment
  
  print(i)
  
  # asymetric review
  # if sentiment score is negative and rating is 5, consider as asymmetric review
  if (dataset$label[i] == 5 && senti_score < 0) { 
    asymmetricReviewList = rbind(asymmetricReviewList, data.frame(text, label, senti_score))
  } else {
    newReviewList = rbind(newReviewList, data.frame(text, label, senti_score))
  }
  
}

str(asymmetricReviewList)
str(newReviewList)
summary(asymmetricReviewList)
summary(newReviewList)
dim(asymmetricReviewList)
dim(newReviewList)

write.csv(asymmetricReviewList, 'C:\\Users\\eunvi\\Documents\\APU_Master\\CP\\shopee_reviews.csv\\asymeetricReviewList_3.csv', row.names = FALSE)
write.csv(newReviewList, 'C:\\Users\\eunvi\\Documents\\APU_Master\\CP\\shopee_reviews.csv\\newReviewList_3.csv', row.names = FALSE)


#################################################################
# STEP 3 - Rating Prediction
#################################################################

# Factorization of Target Variable
newReviewList$label <- as.factor(newReviewList$label)
asymmetricReviewList$label <- as.factor(asymmetricReviewList$label)

str(asymmetricReviewList)

# newReviewList
set.seed(50)
pred <- sample(2, nrow(newReviewList), replace = TRUE, prob = c(0.7,0.3))

review_train <- newReviewList[pred==1,]
review_test <- newReviewList[pred==2,]

str(review_train)
dim(newReviewList)


#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")
library(caret)
library(randomForest)
library(Metrics) # RMSE

table(review_without5_train[,c('text', 'label', 'senti_score')])

# Randome Forest Model
rf_model1 <- randomForest(label ~ text, data = review_train, ntree = 300, mtry = 2, gamma=0.5, cost=32)
rf_model2 <- randomForest(label ~ senti_score, data= review_train, ntree = 300, mtry = 2, gamma=0.5, cost=32)
rf_model3 <- randomForest(label ~ senti_score, data= review_train, mtry = 2, gamma=0.5, cost=32)
rf_model4 <- randomForest(label ~ senti_score, data= review_train, mtry = 4, gamma=0.5, cost=32)
rf_model5 <- randomForest(label ~ text + senti_score, data = review_train, mtry = 2, gamma=0.5, cost=32)
rf_model6 <- randomForest(label ~ text + senti_score, data = review_train, mtry = 4, gamma=0.5, cost=32)
rf_model7 <- randomForest(label ~ text + senti_score, data = review_train, gamma=0.5, cost=32)
rf_model8 <- randomForest(label ~ text + senti_score, data = review_train, gamma=0.9, cost=32)
rf_model9 <- randomForest(label ~ text + senti_score, data = review_train, gamma=0.9, cost=15)
rf_model10 <- randomForest(label ~ text + senti_score, data = review_train, gamma=2, cost=32)
rf_model11 <- randomForest(label ~ text + senti_score, data = review_train, ntree= 800, gamma=0.5, cost=32)
rf_model12 <- randomForest(label ~ text + senti_score, data = review_train, ntree= 1000, gamma=0.5, cost=32)


plot(rf_model11)
# Present importance of variable
varImpPlot(rf_model11)
importance(rf_model11)
# 랜덤포레스트 내부에서 의사결정 나무가 많아질수록 오류율이 어떻게 변하는지 보여줌줌
dev.off()
par("mar")
par(mar=c(5,5,5,5))
plot(rf_model11$err.rate[, 1], col = "red")


# 모델 시각화
layout(matrix(c(1,2),nrow=1),width=c(4,1)) 
par(mar=c(5,4,4,0)) # 오른쪽 마진 제거 
plot(rf_model7)
par(mar=c(5,0,4,2)) # 왼쪽 마진 제거 
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf_model7$err.rate),col=1:4,cex=0.8,fill=1:4)

#Predict with test_data set.
rf_pred1 <- predict(rf_model1, newdata = review_test) #'predict' - method
rf_pred2 <- predict(rf_model2, newdata = review_test) 
rf_pred3 <- predict(rf_model3, newdata = review_test)
rf_pred4 <- predict(rf_model4, newdata = review_test)
rf_pred5 <- predict(rf_model5, newdata = review_test)
rf_pred6 <- predict(rf_model6, newdata = review_test)
rf_pred7 <- predict(rf_model7, newdata = review_test)
rf_pred8 <- predict(rf_model8, newdata = review_test)
rf_pred9 <- predict(rf_model9, newdata = review_test)
rf_pred10 <- predict(rf_model10, newdata = review_test)

rf_pred11 <- predict(rf_model11, newdata = review_test)
rf_pred12 <- predict(rf_model12, newdata = review_test)


# Accuracy checking
str(review_test)
confus_matrix1 <- confusionMatrix(rf_pred1, review_test$label)
confus_matrix1

confus_matrix2 <- confusionMatrix(rf_pred2, review_test$label)
confus_matrix2

confus_matrix3 <- confusionMatrix(rf_pred3, review_test$label)
confus_matrix3

confus_matrix4 <- confusionMatrix(rf_pred4, review_test$label)
confus_matrix5 <- confusionMatrix(rf_pred5, review_test$label)
confus_matrix6 <- confusionMatrix(rf_pred6, review_test$label)
confus_matrix7 <- confusionMatrix(rf_pred7, review_test$label)
confus_matrix8 <- confusionMatrix(rf_pred8, review_test$label)
confus_matrix9 <- confusionMatrix(rf_pred9, review_test$label)
confus_matrix10 <- confusionMatrix(rf_pred10, review_test$label)
confus_matrix11 <- confusionMatrix(rf_pred11, review_test$label)
confus_matrix12 <- confusionMatrix(rf_pred12, review_test$label)



plot(margin(rf_model, review_test$label))

######################################
# CHECK RMSE & MAE
######################################

# Convert factor into numeric value to calculate RMSE
actual <- as.numeric(review_test$label)
predicted1 <- as.numeric(rf_pred1)
predicted2 <- as.numeric(rf_pred2)
predicted3 <- as.numeric(rf_pred3)
predicted4 <- as.numeric(rf_pred4)
predicted5 <- as.numeric(rf_pred5)
predicted6 <- as.numeric(rf_pred6)
predicted7 <- as.numeric(rf_pred7)
predicted8 <- as.numeric(rf_pred8)
predicted9 <- as.numeric(rf_pred9)
predicted10 <- as.numeric(rf_pred10)
predicted11 <- as.numeric(rf_pred11)
predicted12 <- as.numeric(rf_pred12)

# Calculate RMSE & MAE
predict_rmse1 <- rmse(actual, predicted1)
predict_mae1 <- mae(actual, predicted1)

predict_rmse
predict_mae

predict_rmse2 <- rmse(actual, predicted2)
predict_mae2 <- mae(actual, predicted2)

predict_rmse3 <- rmse(actual, predicted3)
predict_mae3 <- mae(actual, predicted3)

predict_rmse4 <- rmse(actual, predicted4)
predict_mae4 <- mae(actual, predicted4)

predict_rmse5 <- rmse(actual, predicted5)
predict_mae5 <- mae(actual, predicted5)

predict_rmse6 <- rmse(actual, predicted6)
predict_mae6 <- mae(actual, predicted6)

predict_rmse7 <- rmse(actual, predicted7)
predict_mae7 <- mae(actual, predicted7)

predict_rmse8 <- rmse(actual, predicted8)
predict_mae8 <- mae(actual, predicted8)

predict_rmse9 <- rmse(actual, predicted9)
predict_mae9 <- mae(actual, predicted9)

predict_rmse10 <- rmse(actual, predicted10)
predict_mae10 <- mae(actual, predicted10)

predict_rmse11 <- rmse(actual, predicted11)
predict_mae11 <- mae(actual, predicted11)

predict_rmse12 <- rmse(actual, predicted12)
predict_mae12 <- mae(actual, predicted12)

#################################################################
# STEP 3-1 - Rating Prediction for asymmetricReviewList
#################################################################
str(asymmetricReviewList)
asymmetric_pred <- predict(rf_model11, newdata = asymmetricReviewList)


#################################################################
# Prototype of Asymmetric Review Prediction Model
#################################################################

review_rating_checker <- function(text, label) {

  review_frame <- NULL
  predicted_frame <- NULL
  
  text <- clean_text(text)
  sentimentScore <- sentiment_by(cleanedText, polarity_dt = mykey_added)
  senti_score <- sentimentScore$ave_sentiment
  
  print("sentiment score as below :")
  print(senti_score)
  print("=========================")
  
  review_frame = rbind(review_frame, data.frame(text, label, senti_score))
  
  review_frame$label <- as.factor(review_frame$label)
  #str(review_frame)

  if (label == 5 && senti_score < 0) {
    
    predicted <- predict(rf_model11, newdata = review_frame)
    #print("predicted rating score as below :")
    #print(predicted)
    #print("===================================")
    
    predicted_frame = rbind(predicted_frame, data.frame(text, predicted))
    
    print("Rating is not appropriate. Please refer to below the predicted rating information.")
    predicted_frame
  } else {
    print("Assigned rating is appropariate with this review.")
  }
  
}
