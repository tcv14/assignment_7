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

# for sentiment analysis with nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")
nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

# bush sentiment analysis with nrc
bush_joy <- tidy_bush %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n))
bush_anger <- tidy_bush %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n))
bush_anticipation <- tidy_bush %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n))

gridExtra::grid.arrange(
ggplot(bush_joy, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Bush Joy"),
ggplot(bush_anger, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Bush Anger"),
ggplot(bush_anticipation, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Bush Anticipation"),
nrow=1
)

# carter sentiment analysis with nrc
carter_joy <- tidy_carter %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n))
carter_anger <- tidy_carter %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n))
carter_anticipation <- tidy_carter %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n))

gridExtra::grid.arrange(
ggplot(carter_joy, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Carter Joy"),
ggplot(carter_anger, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Carter Anger"),
ggplot(carter_anticipation, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Carter Anticipation"),
nrow=1
)
