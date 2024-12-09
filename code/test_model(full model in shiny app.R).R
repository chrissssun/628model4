library(dplyr)
library(tm)
library(ggplot2)
library(cluster)
library(factoextra)
library(readr)
library(stringr)
library(tidyr)
library(textdata)
library(ggplot2)

# Read the data
data <- read_csv("C:/Users/HP/Desktop/Master/628/P4/github/test_data.csv", col_types = cols(name = col_character(), description = col_character()))

# Text cleaning
corpus <- Corpus(VectorSource(data$description))
corpus_clean <- tm_map(corpus, content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus_clean)
tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(tfidf)

# PCA for dimension reduction
pca_result <- prcomp(tfidf_matrix, scale. = TRUE)
pca_matrix <- pca_result$x[, 1:10]

# Sentiment analysis
sentiment_dict <- get_sentiments("bing")
sentiment_scores <- data %>%
  unnest_tokens(word, description) %>%
  inner_join(sentiment_dict, by = "word") %>%
  group_by(name) %>%
  summarise(Sentiment_Polarity = sum(ifelse(sentiment == "positive", 1, -1), na.rm = TRUE)) %>%
  ungroup()

# Novelty score calculation
similarity_matrix <- sim2(tfidf_matrix, method = "cosine")
novelty_scores <- apply(similarity_matrix, 1, function(row) mean(1 - row))

# Text complexity metrics
data <- data %>%
  mutate(
    Avg_Word_Length = str_length(description) / str_count(description, "\\S+"),
    Sentence_Count = pmax(str_count(description, "\\.|\\!|\\?"), 1),
    Sentence_Length = str_count(description, "\\S+") / Sentence_Count,
    Flesch_Score = 206.835 - (1.015 * (str_count(description, "\\S+") / Sentence_Count)) -
      (84.6 * (str_count(description, "[aeiouAEIOU]") / str_count(description, "\\S+")))
  )

# Combine all metrics
data <- data %>%
  mutate(Novelty_Score = novelty_scores) %>%
  left_join(sentiment_scores, by = "name") %>%
  mutate(Sentiment_Polarity = replace_na(Sentiment_Polarity, 0))

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(data[, c(4:8)], centers = 3)
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualization of clustering
fviz_cluster <- fviz_cluster(kmeans_result, data = data[, c(4:8)], geom = "point", ellipse.type = "convex", stand = FALSE)
print(fviz_cluster)

# Save the plot
ggsave("clustering_plot.png", fviz_cluster, width = 10, height = 8, dpi = 300)

