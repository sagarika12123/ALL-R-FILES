##mining NASA metadata
##metadata is a term thar refers to data that gives information about other data.
##in this case, the metadata informs users about what is in these numerous NASA datasets but does not include the content of the datasets themselves.
library(jsonlite)
metadata<-fromJSON("https://data.nasa.gov/data.json")
hey<-names(metadata$dataset)
View(hey)
class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)
##wrangling and tidying the data
library(dplyr)
nasa_title<-tibble(id=metadata$dataset$'identifier',title=metadata$dataset$title)
View(nasa_title)
nasa_title
nasa_desc<-tibble(id=metadata$dataset$'identifier',desc=metadata$dataset$description)
nasa_desc %>%
  select(desc) %>%
  sample_n(5)
View(nasa_title)
View(nasa_desc)
library(tidyr)
nasa_keyword<-tibble(id=metadata$dataset$'identifier',keyword=metadata$dataset$keyword) %>%
  unnest(keyword)
nasa_keyword
View(nasa_keyword)
library(tidytext)
nasa_title<-nasa_title %>%
  unnest_tokens(word,title) %>%
  anti_join(stop_words)
View(nasa_title)
nasa_desc <-nasa_desc %>%
  unnest_tokens(word,desc) %>%
  anti_join(stop_words)
View(nasa_desc)
nasa_title %>%
  count(word,sort=TRUE)
nasa_desc%>%
  count(word,sort=TRUE)

my_stopwords <- tibble(word = c(as.character(1:10), 
                                "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                "v003", "v004", "v005", "v006", "v7"))
nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

nasa_keyword %>%
  group_by(keyword) %>%
  count(sort=TRUE)

nasa_keyword<-nasa_keyword %>%
  mutate(keyword=toupper(keyword))

library(widyr)
title_word_pairs<-nasa_title %>%
  pairwise_count(word,id,sort=TRUE,upper=FALSE)
title_word_pairs

desc_word_pairs<-nasa_desc %>%
  pairwise_count(word,id,sort=TRUE,upper=FALSE)
desc_word_pairs

##plotting the correlation between these two words. 
library(ggplot2)
library(igraph)
install.packages("ggraph")
library(ggraph)
set.seed(1234)
title_word_pairs %>%
  filter(n>=250) %>%
  graph_from_data_frame() %>%
  ggraph(layout='fr') +
  geom_edge_link(aes(edge_alpha=n,edge_width=n),edge_colour="cyan4")+
  geom_node_point(size=5)+
  geom_node_text(aes(label=name),repel=TRUE,point.padding=unit(0.2,'lines'))+
  theme_void()
##doesnt work because doesnt have value as great as 5000
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

##network of keywords
keyword_pairs<-nasa_keyword %>%
  pairwise_count(keyword,id,sort=TRUE,upper=FALSE)
keyword_pairs
  ##corrrelations between descriptions
set.seed(1234)
keyword_pairs %>%
  filter(n>=700) %>%
  graph_from_data_frame() %>%
  ggraph(layout='fr')+
  geom_edge_link(aes(edge_alpha=n,edge_width=n),edge_colour="royalblue")+
  geom_node_point(size=5)+
  geom_node_text(aes(label=name),repel=TRUE,point.padding=unit(0.2,'lines'))+
  theme_void()
keyword_cors<-nasa_keyword %>%
  group_by(keyword) %>%
  filter(n()>=50) %>%
  pairwise_cor(keyword,id,sort=TRUE,upper=FALSE)
keyword_cors

##visualization of these keywords
set.seed(1234)
keyword_cors %>%
  filter(correlation>0.6) %>%
  graph_from_data_frame() %>%
  ggraph(layout='fr')+
  geom_edge_link(aes(edge_alpha=correlation,edge_width=correlation),edge_colour="green")+
  geom_node_point(size=5)+
  geom_node_text(aes(label=name),repel=TRUE,point.padding=unit(0.2,'lines'))+
  theme_void()

##calculating tf-idf for the description fields
desc_tf_idf<-nasa_desc %>%
  count(id,word,sort=TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word,id,n)

desc_tf_idf %>%
  arrange(-tf_idf)

##connecting description fields to keywords
desc_tf_idf<-full_join(desc_tf_idf,nasa_keyword,by='id')
##lets plot some of the most important words, as meas

desc_tf_idf %>%
  filter(!near(tf,1)) %>%
  filter(keyword %in% )










  
  
  
















