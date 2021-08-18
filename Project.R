install.packages("tidytext")

library(tidytext)
sentiments

#Negative Positive Categorisation we use Bing
#AFINN for -5 to +5
#Bing for Negative Positive
#loughran Analysis on shareholders reports

get_sentiments("bing")

#Textual Data in the forms of books we use janeaustenr (Jane austen)
install.packages("janeaustenr")

install.packages("stringr")

library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)

#Text of book into tidy format single row single word

tidy_data <- austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% ungroup() %>% unnest_tokens(word, text)

#Using Bing ob each word

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

#Segregation of Words

install.packages("tidyr")
library(tidyr)

bing <- get_sentiments("bing")

Emma_sentiment <- tidy_data %>% inner_join(bing) %>% count(book = "Emma" , index = linenumber %/% 80, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)


#vISUALISATION OF ALL THE WORDS

library(ggplot2)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) + geom_bar(stat = "identity", show.legend = TRUE) +facet_wrap(~book, ncol = 2, scales = "free_x")

#MOST POSITICE AND NEGATIVE WORDS

counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

head(counting_words)

#PLOT OF MOST POSITIVE AND NEGATIVE
  
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

#WORDCLOUD OF WORDS

install.packages("reshape")
install.packages("wordcloud")
library(reshape2)
library(wordcloud)

tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)