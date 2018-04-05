library(dplyr)
library(tidytext)

# load data and convert to tibble
bush <- read.delim("./h_data/bush41 first state of the union.txt", header=FALSE)
carter <- read.delim("./h_data/carter first state of the union.txt", header = FALSE)

tidy_bush <- data_frame(paragraph = 1:61, text = as.character(bush$V1)) %>% 
  unnest_tokens(word, text)

tidy_carter <- data_frame(paragraph = 1:112, text= as.character(carter$V1)) %>% 
  unnest_tokens(word, text)

