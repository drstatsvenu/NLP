
setwd("C:/Users/Sm/Desktop/NLP Projects/stats topic  Modeling")
getwd()

library(tm)
options(scipen=999)
wiki_docs <- Corpus(DirSource("C:/Users/Sm/Desktop/St martin/NLP/Topic Modeling/venu"))

# Func to view corpus
to_char <- function(corpus, start=1, end=NULL){
  if(is.null(end)){
    end=length(corpus)  
  }
  sapply(corpus[start:end], function(x){paste(x$content)})
}


# Text Processing ------------------------------------------------------------------------------
wiki_docs <- tm_map(wiki_docs, removeWords, stopwords('english'))
inspect(wiki_docs)
wiki_docs <- tm_map(wiki_docs, removeWords, c('can', 'must', 'also', 'may', 'will','using','thus','take','called','used','example','either','since','said','known','use'))
inspect(wiki_docs)
wiki_docs <- tm_map(wiki_docs, content_transformer(tolower))
inspect(wiki_docs)
wiki_docs <- tm_map(wiki_docs, removeNumbers)
inspect(wiki_docs)
wiki_docs <- tm_map(wiki_docs, removePunctuation)
inspect(wiki_docs)
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}) )
inspect(wiki_docs)
# Doc Term Matrix
dtm <- DocumentTermMatrix(wiki_docs)
## control = list(removePunctuation=TRUE, removeNumbers=TRUE, stopwords=TRUE))
dtmm=as.matrix(dtm)
dim(dtmm)
View(dtmm)
write.csv(dtmm,"dtmmatrix1.csv")





# LDA ------------------------------------------------------------------------------------------------
library(topicmodels)
# Set parameters
burnin = 4000
iter = 2000
thin = 500
seed = list(2003,5,63,100001,765)
nstart = 5
best = TRUE
# Number of topics
k = 10
# Run LDA
res1 <-LDA(dtm, k, method="Gibbs", control = list(nstart = nstart, seed = seed, 
                                                 best = best, burnin = burnin, 
                                                 iter = iter, thin = thin))
res1
# Number of topics in each document
res1_topics = as.matrix(topics(res1))
print(res1_topics)


# Top 30 terms
res1_terms = as.matrix(terms(res1, 30))
print(res1_terms)


# Show topic probabilities. Rows are documents and Columns are the topics.
res1_topicProbs = as.data.frame(res1@gamma)
print(cbind(rownames(res1_topics), res1_topicProbs))

# Check that each term is allocated to all topics
print(rowSums(res1_topicProbs))

##############
xx=cbind(res1_topics,predict)
View(xx)
write.csv(xx,"cbindmatrix1.csv")

predict1=read.csv("cbindmatrix1.csv")
str(predict1)
predict1$res1_topics=as.factor(predict1$res1_topics)
str(predict1)


# Heat Map with Dendrogram ------------------------------------------------------------------------
# install.packages("d3heatmap")
library(d3heatmap)
library(RColorBrewer)
topic_probs <- data.matrix(res1_topicProbs) 
# convert to matrix

# Set column and row names
colnames(topic_probs) <- c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")
rownames(topic_probs) <- rownames(res1_topics)

# draw heatmap
d3heatmap(topic_probs, colors = brewer.pal(9, "Greens"), scale="column", margins=c(5,5), dendrogram = "row", k_row = 5, cexRow=0.75)


#####################



library(caret)
model=train(res1_topics~.,data=predict1,method="knn")
pred=predict(model,data=predict1)
confusionMatrix(pred,predict1$res1_topics)

pre=predi



