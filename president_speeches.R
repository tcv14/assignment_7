library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)

# load data and convert to tibble
bush <- read.delim("./h_data/bush41 first state of the union.txt", header=FALSE)
carter <- read.delim("./h_data/carter first state of the union.txt", header = FALSE)

tidy_bush <- data_frame(paragraph = 1:61, text = as.character(bush$V1)) %>% 
  unnest_tokens(word, text)

tidy_carter <- data_frame(paragraph = 1:112, text= as.character(carter$V1)) %>% 
  unnest_tokens(word, text)

# bush speeches
bush_sentiment <- tidy_bush %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 2) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)

ggplot(bush_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)  # positive/negative sentiments in bush's speech


# carter speeches
carter_sentiment <- tidy_carter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 3) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)

ggplot(carter_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) # positive/negative sentiments in carter's speech


bush_word_counts <- tidy_bush %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

bush_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle("Bush Speech") +
  coord_flip()

carter_word_counts <- tidy_carter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

carter_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle("Carter Speech") +
  coord_flip()
