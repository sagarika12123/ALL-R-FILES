##tokenizing by n-gram
library(dplyr)
library(tidytext)
library(janeaustenr)
austen_bigrams<-austen_books() %>%
  unnest_tokens(bigram,text,token="ngrams",n=2)
austen_bigrams
View(austen_bigrams)
##counting and filtering n-grams
austen_bigrams %>%
  count(bigram,sort=TRUE)
##so many repeated patterns which are unwanted so create variable stop word
library(tidyr)
bigrams_seperated<- austen_bigrams %>%
  separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered<-bigrams_seperated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%
  count(word1,word2,sort=TRUE)
bigram_counts
View(bigram_counts)

bigrams_united <-bigrams_filtered %>%
  unite(bigram,word1,word2,sep=" ")
View(bigrams_united)

austen_books() %>%
  unnest_tokens(trigram,text,token='ngrams',n=3) %>%
  separate(trigram,c("word1","word2","word3"),sep=" ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word,!word3 %in% stop_words$word) %>%
  count(word1,word2,word3,sort=TRUE)
##analyzing bigrams
bigrams_filtered %>%
  filter(word2=="street") %>%
  count(book,word1,sort=TRUE)

bigram_tf_idf<-bigrams_united %>%
  count(book,bigram) %>%
  bind_tf_idf(bigram,book,n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf


bigrams_seperated%>%
  filter(word1=="not")%>%
  count(word1,word2,sort=TRUE)

AFINN<-get_sentiments("afinn")
AFINN

not_words<-bigrams_seperated %>%
  filter(word1=="not") %>%
  inner_join(AFINN,by=c(word2="word")) 
  count(word2,value,sort=TRUE)
 
View(not_words) 





