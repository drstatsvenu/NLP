getwd()
setwd("C:/Users/Sm/Desktop/St martin/NLP/Text classification")

getwd()

# Loading required libraryies
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)

library(RWeka)
library(reshape2)
library(quanteda)

# Reading data

review=read.csv("reviewdata.csv", stringsAsFactors = FALSE)
names(review)
# Creating corpos from review column
## Make a vector source and a corpus
corpus_review=Corpus(VectorSource(review$Review.Text))
corpus_review
##########################################
#STEP2???-???Text Pre-processing
##########################################

#Convert to lower case
corpus_review=tm_map(corpus_review, tolower)
inspect(corpus_review)
#2. Remove punctuation:
corpus_review=tm_map(corpus_review, removePunctuation)
inspect(corpus_review)
#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
inspect(corpus_review)
# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))
inspect(corpus_review)
## Stem document
corpus_review=tm_map(corpus_review, stemDocument)
##Viewing the corpus content
corpus_review[[1]][1]
inspect(corpus_review)
##########################################
#  Descriptve and Viszvalization of the data
#Frequently used words
# Find the 20 most frequent terms: term_count
##term_count <- freq_terms(corpus_review, 20)
###?freq_terms()
# Plot 20 most frequent terms
##plot(term_count)
##########################################

#STEP4  Create the DTM & TDM from the corpus

##########################################
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

dim(review_dtm)
dim(review_tdm)

#Using the TDM to identify frequent terms

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)

dim(review_m)
write.csv(review_m,"datanew.csv")
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]
###############################################
#STEP5???-???Exploratory text analysis
# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)

#Word clouds
review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 100, colors = "red")

# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 100, colors = c("aquamarine","darkgoldenrod","tomato"))

###############################################
#STEP5???-???Feature extraction by removing sparsity   
## Load the required libraries
library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(igraph)
library(fpc)
library(Rcampdf)

# Tokenize descriptions
reviewtokens=tokens(review$Review.Text,what="word",
                    remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)
# Lowercase the tokens
reviewtokens=tokens_tolower(reviewtokens)

# remove stop words and unnecessary words
rmwords <- c("dress", "etc", "also", "xxs", "xs", "s")
reviewtokens=tokens_select(reviewtokens, stopwords(),selection = "remove")
reviewtokens=tokens_remove(reviewtokens,rmwords)

# Stemming tokens
reviewtokens=tokens_wordstem(reviewtokens,language = "english")
reviewtokens=tokens_ngrams(reviewtokens,n=1:2)
reviewtokens
# Creating a bag of words
reviewtokensdfm=dfm(reviewtokens,tolower = FALSE)

dim(reviewtokensdfm)

# Remove sparsity
reviewSparse <- convert(reviewtokensdfm, "tm")
tm::removeSparseTerms(reviewSparse, 0.7)

# Create the dfm
dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x=dfm_trim(reviewtokensdfm, sparsity = 0.98)
dim(x)

#STEP6???-???Building the Classification Models
## Setup a dataframe with features
 df=convert(x,to="data.frame")
names(df)
write.csv(df,"inputnew.csv")
##Add the Y variable Recommend.IND
#reviewtokensdf=cbind(review$Recommended.IND,df)
#head(reviewtokensdf)
#write.csv(reviewtokensdf,"inputwithrec.csv)


rec=read.csv("inputwithrec.csv")
str(rec)
rec$recommand=as.factor(rec$recommand)
str(rec)

#spilting
train_rows<- sample(1:nrow(rec), size=0.7*nrow(rec))
train_rows
train <- rec[train_rows, ]
test <- rec[-train_rows, ]
dim(rec)
dim(train)
dim(test)
names(train)
names(test)
str(test)

# Model Pipeline
# Run algorithms using 5-fold cross validation
library(caret)
control = trainControl(method="repeatedcv", number=5, repeats=3)


# GLM
library(caret)

set.seed(100)
fit.glm = train(recommand~., data=train, method="glm", metric="Accuracy", trControl=control)
print(fit.glm)


# SVM
set.seed(100)
grid = expand.grid(.sigma=c(0.01,0.05,0.1), .C=c(1))
fit.svm = train(recommand~., data=train, method="svmRadial", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.svm)


# kNN
set.seed(100)
grid = expand.grid(.k=c(1,3,5,7))
fit.knn = train(recommand~., data=train, method="knn", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.knn) 

#random forest
set.seed(100)
fit.rf = train(recommand~., data=train, method="rf", metric="Accuracy", trControl=control)
print(fit.rf)

# Comparing algorithms
M3=resamples(list(SVM=fit.svm, kNN=fit.knn,  RF=fit.rf))
summary(M3)
dotplot(M3)
#validation of train data
pred=predict(fit.rf,data=train)
confusionMatrix(pred,train$recommand)
#validation of test data
pred1=predict(fit.rf,newdata=test)
confusionMatrix(as.factor(pred1),test$recommand)














  







      