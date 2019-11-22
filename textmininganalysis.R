##text mining

#terms inverse document frequency(idf) 
##Another approach is to look at a term’s 
##inverse document frequency (idf), which 
##decreases the weight for commonly used words
##and increases the weight for words that are 
##not used very much in a collection of documents. 
##This can be combined with term frequency to 
##calculate a term’s tf-idf

##conducting term frequency on jane austens novels
library(dplyr)
library(janeaustenr)
book_words<-austen_books() %>%
  unnest_tokens(word,text) %>%
  count(book,word,sort=TRUE)
View(book_words)

total_words<-book_words %>%
  group_by(book) %>%
  summarize(total=sum(n))
total_words

book_words<-left_join(book_words,total_words)
book_words


library(ggplot2)
ggplot(book_words,aes(n/total,fill=book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA,0.0009)+
facet_wrap(~book,ncol=2,scales="free_y")


##ZIPF's law
freq_by_rank<-book_words %>%
  group_by(book) %>%
  mutate(rank=row_number(),term_frequency=n/total)
freq_by_rank



freq_by_rank %>% 
  ggplot(aes(rank, `term_frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset<-freq_by_rank %>%
  filter(rank <500,
         rank >10)
lm(log10('term_frequency')~log10(rank),data=rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term_frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

##bind_tf_idf

book_words<-book_words %>%
  bind_tf_idf(word,book,n)
book_words
##Notice that idf and thus tf-idf
##are zero for these extremely common words.

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
##here we see all the proper nouns. names that are in fact important in the movies

##let us look at the visualization
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word,levels=rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word,tf_idf,fill=book))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL,y="tf_idf")+
  facet_wrap(~book,ncol=2,scales="free")+
  coord_flip()

##corpus of physics texts

library(gutenbergr)
physics<-gutenberg_download(c(37729,14725,13476,30155),meta_fields = "author")
physics_words<-physics %>%
  unnest_tokens(word,text) %>%
  count(author,word,sort=TRUE)
physics_words
View(physics_words)

library(forcats)
plot_physics<-physics_words %>%
  bind_tf_idf(word,author,n) %>%
  mutate(word=fct_reorder(word,tf_idf)) %>%
  mutate(author=factor(author,levels=c("Galilei, Galileo","Huygens, Christiaan","Tesla, Nikola","Einstein, Albert")))

plot_physics %>%
  group_by(author) %>%
  top_n(15,tf_idf) %>%
  ungroup() %>%
  mutate(word=reorder(word,tf_idf)) %>%
  ggplot(aes(word,tf_idf,fill=author)) +
  geom_col(show.legend=FALSE)+
  labs(x=NULL,y="tf-idf")+
  facet_wrap(~author,ncol=2,scales="free")+
  coord_flip()

library(stringr)
physics %>%
  filter(str_detect(text,"_k_")) %>%
  select(text)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words<-anti_join(physics_words,mystopwords,by='word')
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()




