# Load necessary libraries
library(janeaustenr)  # for text data (Jane Austen's books)
library(tidytext)      # for text processing
library(dplyr)         # for data manipulation
library(ggplot2)       # for visualization
library(stringr)       # for string manipulation
library(tidyr)         # for data reshaping
library(wordcloud)     # for word cloud visualization

# Load the sentiment lexicons
bing_sentiment <- get_sentiments("bing")  # positive and negative sentiment words

# Load Jane Austen's books from the janeaustenr package
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)  # Tokenizing text into individual words

# Performing sentiment analysis by joining the tidy_data with the Bing lexicon
positive_senti <- bing_sentiment %>% 
  filter(sentiment == "positive")

# Extract positive sentiment words from the book "Emma"
positive_words <- tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

# View the positive sentiment words
print(positive_words)

# Perform sentiment analysis by calculating sentiment score: positive - negative
emma_sentiment <- tidy_data %>%
  inner_join(bing_sentiment) %>%
  count(book = "Emma", index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Visualizing the sentiment scores using ggplot
ggplot(emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  labs(title = "Sentiment Analysis of Emma")

# Counting the most common positive and negative words in the book
counting_words <- tidy_data %>%
  inner_join(bing_sentiment) %>%
  count(word, sentiment, sort = TRUE)

# View the top counted words
print(head(counting_words))

# Visualization of sentiment scores for the most common words
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score") +
  labs(title = "Sentiment Score of Common Words")

# Creating a wordcloud for the most frequent words in positive and negative sentiments
tidy_data %>%
  inner_join(bing_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"), max.words = 100)
