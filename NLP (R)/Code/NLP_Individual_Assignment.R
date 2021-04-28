
# Hult International Business School
# Text Analytics - MsBA2 
# Gabriela Ledesma Valenzuela
# Individual Report 

################################################################################
################################################################################
#Installation packages needed
################################################################################
################################################################################

#install.packages("textreadr")
#install.packages("tm")
#install.packages("reshape2")

################################################################################
################################################################################
#Installation of libraries needed
################################################################################
################################################################################

library(tidyverse)
library(textreadr) 
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)    
library(tm)
library (scales)
library(textdata)
library(ggplot2)
library(wordcloud)    #Sentiment analysis
library(reshape2)     #Sentiment analysis

#####################################################
# Importing all .txt files from one directory 
#####################################################

setwd("/Users/gabyvalenzuela/Documents/31. Personal Portfolio/NLP (R)/TXT Files") 
nm <- list.files(path = "/Users/gabyvalenzuela/Documents/31. Personal Portfolio/NLP (R)/TXT Files")
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
my_txt_text <- data.frame(my_txt_text)

######################################################
# Assigning a location for each document
######################################################

my_txt_text$location <- c("africa","asia","europe")
token_list <- my_txt_text %>%
  unnest_tokens(word, my_txt_text)

#######################################################
#### Tokenizing the text  
#######################################################

token_list %>%    
  count(word, location, sort = TRUE) 
print(token_list)

########################################################
##### Removing the stop words and creating proportions    
########################################################

#Deleting stop words
data(stop_words)
frequencies_tokens_nostop <- token_list%>%
  anti_join(stop_words) %>% 
  count(word, location, sort=TRUE) 

# Code for to create the correlation graphs and proportions
frequency_new <- token_list %>%
  anti_join(stop_words) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(location, word) %>%
  group_by(location) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(location, proportion) %>%
  gather(location, proportion, `africa`,`asia`)

#Creating the correlation plots
ggplot(frequency_new, aes(x=proportion, y=`europe`, 
                      color = abs(`europe`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~location, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "europe", x=NULL)

########################################################
###### Dividing the data by location 
########################################################

words_africa <- frequencies_tokens_nostop %>%
  filter(location == "africa") 

words_asia <- frequencies_tokens_nostop %>%
  filter(location == "asia")

words_europe <- frequencies_tokens_nostop %>%   
  filter(location == "europe")

########################################################
####### Generating the graphs with the main top 10 words
########################################################

### Plot Africa ###
freq_hist <- words_africa %>%
  top_n(10) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

### Plot Asia ###
freq_hist <- words_asia %>%
  top_n(10) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

### Plot Europe ###
freq_hist <- words_europe %>%
  top_n(10) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

################################################################################
################################################################################
# Sentiment analysis by wordcloud
################################################################################
################################################################################

# Creating the nrc variable
nrc <- get_sentiments("nrc")        

########################################################
####### Sentiments Africa
########################################################
 
words_africa %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=50, scale = c(1,0.01))   

########################################################
####### Sentiments Asia
########################################################

words_asia %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                    
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=50, scale = c(1,0.1))  

########################################################
####### Sentiments Europe
########################################################

words_europe %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                      
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),         
                   max.words=50, scale = c(1,0.1))  

################################################################################
################################################################################
####### Sentiments comparison by bar graphs
################################################################################
################################################################################

########################################################
####### Sentiments Africa (bar)
########################################################
sentiments_africa <-words_africa %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = T) %>%
  ggplot(aes(sentiment, n, fill = sentiment, order = TRUE)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")+
  ylab("Contribution to sentiment Africa") +
  xlab("Sentiment")+
  ggtitle("Reports Sentiment Africa")+
  coord_flip()
sentiments_africa

########################################################
####### Sentiments Asia (bar)
########################################################

sentiments_asia <-words_asia %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = T) %>%
  ggplot(aes(sentiment, n, fill = sentiment, order = TRUE)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")+
  ylab("Contribution to sentiment Asia") +
  xlab("Sentiment")+
  ggtitle("Reports Sentiment Asia")+
  coord_flip()
sentiments_asia

########################################################
####### Sentiments Europe (bar)
########################################################

sentiments_europe <-words_europe %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = T) %>%
  ggplot(aes(sentiment, n, fill = sentiment, order = TRUE)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")+
  ylab("Contribution to sentiment Europe") +
  xlab("Sentiment")+
  ggtitle("Reports Sentiment Europe")+
  coord_flip()
sentiments_europe

#################################################################################
#################################################################################

#################################################################################
#################################################################################

