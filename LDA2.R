library(tm)
library(LDAvis)
library(NLP)
library(servr)
library(dplyr)
library(stringi)
require(topicmodels)
require(RColorBrewer)
library(ggplot2)
library(wordcloud)

#library(topicmodels, lib.loc=".")


setwd("D:\\Corpse")
alltweets <- read.csv("output.csv", header = TRUE)
#extract data from the column 'text' so that each tweet is a document
corpus <- iconv(alltweets$text, to = "ASCII", sub = "")
#do a quick check of the loaded data. See the 125th document (tweet) 
corpus[1]
corpus[110]
#writeLines(as.character(corpus[[1]]))
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower)) #converted to lower cases

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
corpus <- tm_map(corpus, toSpace, "-")
corpus <- tm_map(corpus, toSpace, "'")
corpus <- tm_map(corpus, toSpace, '"')

corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus,removeWords,stopwords("english")) #filter stop words
#corpus <- tm_map(corpus,removeWords,c("via", "twitter", "retweet", " ","@\\w+","http.+ |http.+$","amp")) #user-defined stop words to be filtered out
corpus <- tm_map(corpus,removeWords,c("via", "twitter", "retweet"))
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus,stripWhitespace) #remove white space in text
corpus <- tm_map(corpus,stemDocument)

dtm <- DocumentTermMatrix(corpus) 

#the line above will convert corpus to DTM. But we need to run the next four lines to remove empty document from DTM to prevent potential errors. 
#rowTotals<-apply(dtm,1,sum) #running this line takes time
#empty.rows<-dtm[rowTotals==0,]$dimnames[1][[1]] 
#corpus<-corpus[-as.numeric(empty.rows)]
#dtm <- DocumentTermMatrix(corpus)

inspect(dtm[1:5, 1:5]) 
findFreqTerms(dtm, 20)
dtm.mx <- as.matrix(dtm)

#calculate term frequency
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[0:24] 

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5 #find 5 topics

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("isis",k,"DocsToTopics.csv"))


write.table(ldaOut.topics, "TopicForEachTweet.txt", sep = "\t", row.names = FALSE)

ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("isis",k,"TopicsToTerms.csv"))
ldaOut.terms[1:6,]

#probabilities associated with each of the top terms in each topic
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#topicProbabilities[1:5,]

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
    
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

output <- topicmodels_json_ldavis(ldaOut,corpus, dtm) 
serVis(output, out.dir = "isis_vis", open.browser = FALSE) 

freq = frequency

wf=data.frame(term=names(freq),occurrences=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>20), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=10)
wordcloud(names(freq),freq,min.freq=10,colors=brewer.pal(6,"Dark2"))
