##loading packages 
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
##loading the text file into R 
cname <- file.path("~","Desktop","text")
cname   
dir(cname)
library(tm)
docs<-VCorpus(DirSource(cname))
summary(docs)
inspect(docs[2])
writeLines(as.character(docs[2]))
##preprocessing of the data
##remove punctuation
docs<-tm_map(docs,removePunctuation)

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character that did not translate, so it had to be removed.
}
#remove numbers
docs<-tm_map(docs,removeNumbers)
##convert to lowercase
docs<-tm_map(docs,tolower)
docs<-tm_map(docs,PlainTextDocument)
DocsCopy<-docs


##removing stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
##to remove particular words 
docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) 


##combining words that should stay together 



for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)
##to stem words
docs_st <- tm_map(docs, stemDocument)   

docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

##STRIPPing unecesaary white space from the documents
dir(cname)
docs<-tm_map(docs,stripWhitespace)
##THIS TELLS R TO TREAT YOUR PREPROCESSED DOCUMENTS AS TEXT DOCUMENTS
docs<-tm_map(docs,PlainTextDocument)
#staging the data

##creating a document term matrix

dtm<-DocumentTermMatrix(docs_st)
dtm
  
##to create transpose of the matrix
tdm<-TermDocumentMatrix(docs)
tdm
 
##organise terms by their frequency
freq<-colSums(as.matrix(dtm)) ##UNDERSTAND THIS BETTER 
length(freq) 
ord<-order(freq)  
m<-as.matrix(dtm)  
dim(m)  
  
dtms<-removeSparseTerms(dtm,0.2)
dtms

freq<-colSums(as.matrix(dtm))

dtm

head(table(freq),20)
##for a less, fine grained look at the term frequency

freq<-colSums(as.matrix(dtms))
freq
freq<-sort(colSums(as.matrix(dtm)),decreasin=TRUE)
head(freq,14)


wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  
View(wf)

##plot word frequencies

library(ggplot2)
  
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 

findAssocs(dtm, c("country" , "american"), corlimit=0.85)

findAssocs(dtms,"think",corlimit = 0.70)
##plot most frequently occuring words 
library(wordcloud)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)   

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   

##CLUSTERING BY TERM SIMILARITY

dtmss<-removeSparseTerms(dtm,0.15)
dtmss

#hierarchical clustering
library(cluster)
d<-dist(t(dtmss),method="euclidian")
fit<-hclust(d=d,method="complete")
fit
plot(fit,hang=-1)


plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 6 clusters  
##k-means clustering

########################################################################END OF CLUSTERING!!!########################################
###Let us now carry out sentiment analysis for this code

library(tidytext)
get_sentiments("afinn")
##trying to run the sentiment on data frames

nrc_joy<-get_sentiments("nrc") %>%
  filter(sentiment=="joy")
View(dtm)
dtm<-as.data.frame(as.matrix(dtm))
class(dtm)
View(dtm)

dtm %>%
  filter()
 # filter(dtm=="politic") %>%
  inner_join(nrc_joy) %>%
  count(word,sort=TRUE)


##testing on jane austen books
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
class(book)
 class(austen_books())
 View(austen_books())
 View(book)
 book()
  docs
summary(docs)  
  
##converting to and from dtm
terms<-Terms(dtm)
head(terms)

library(dplyr)
library(tidytext)
dtm_tidy<-tidy(dtm)
dtm_tidy

##notice here that we have a tidy three-column tbl_df with variables document term and count
##this tidying operation is similar to the melt() function from the reshape2 package for non-sparse matrices

dtm_sentiments<-dtm_tidy %>%
  inner_join(get_sentiments("bing"),by=c(term="word"))
dtm_sentiments
View(dtm_sentiments)
dtm.to.dfm(dtm)
library(ggplot2)
dtm_sentiments %>%
  count(sentiment,term,wt=count) %>% ##counts the sentiment with the term given 
  ungroup() %>% ##ungroups the groups which are already present, this is like cleaning the dataset
  #filter(n>=200) %>% ##
  mutate(n=ifelse(sentiment=="negative",-n,n)) %>%
  mutate(term=reorder(term,n))%>%
  ggplot(aes(term,n,fill=sentiment))+
  geom_bar(stat="identity")+
  ylab("contribution to sentiment")+
  coord_flip()
dtm_sent<-dtm_sentiments %>%
  group_by(sentiment,term) %>%
  select(sentiment,term,count) %>%
  filter(count>10) %>%
  distinct() %>%
  ungroup()
View(dtm_sent)
dtm_sent<-as.data.frame(dtm_sent)
class(dtm_sent)
pirateplot(formula=count ~term+sentiment, data=dtm_sent, xlab=NULL,ylab="word count",pal="google",
           point.o = .2,avg.line.o = 1,theme=0,point.pch = 16,point.cex=1.5,jitter.val = .1,cex.lab=.9,cex.names = .7)
           
 ##there you go! trump used illegal the most - negative and  great in positive words.



           
View(dtm_sent)  

dtm_sent %>% 
  count(book,in)
           
           
 #################how to build a text mining, machine learning document classification system in r-refer to youtube

#init
libs <- c('tm','plyr','class')
lapply(libs,require.character.only=TRUE)

#SET OPTIONS
options(stringsAsFactors = FALSE)
#set parameters
candidates <- c("romney","obama")
pathname <- "~~~"
#cleaning the text
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus,removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp,tolower)
  corpus.tmp <- tm_map(corpus.tmp,removeWords,stopwords("english"))
  return(corpus.tmp)
  
}
#build term document matrix (quantitative doc to analyse them)
generateTDM <- function(cand,path){
  s.dir <- sprintf("%s%s",path,cand)
  s.cor <- Corpus(DirSource(directory=s.dir,encoding = "ANSI"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm,0.7)
  result <- list(name=cand,tdm=s.tdm)
  
}

tdm <- lapply(candidates,generateTDM,path=pathname)
#attach name to tdm
bindcandidatetotdm <- function(tdm){
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat)
  s.df <- cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}
candTDM <- lapply(tdm,bindcandidatetotdm)

#stack the matrices on top of each other
tdm.stack <- do.call(rbind,fill,candTDM)
tdm.stack[is.na(tdm.stack)] <- 0
head(tdm.stack)


#hold-out

train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)=0.7))
test.idx <- (i:nrow(tdm.stack)) [-train.idx]

#model-knn
tdm.cand <- tdm.stack[,"targetcandidate"]
tdm.stack.n1 <- tdm.stack[,!colname(tdm.stack)%in%"targetcandidate"]

knn.pred <- knn(tdm.stack.n1[train.idx, ],tdm.stack.n1[test.idx, ],tdm.cand[train.idx])




#accuracy
conf.mat <- table("predictions"=knn.pred,Actual=tdm.cand[test.idx])
conf.mat #take a look at the cofusion matrix
##calculate the accuracy
(accuracy <- sum(diag(conf.mat))/length(test.idx)=100)
           
    
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
             