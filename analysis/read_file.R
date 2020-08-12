library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(scales)


# FMP most reused parts ---------------------------------------------------

# load FMP file (only reused parts)
FMP_reused <- read_csv("../FMP_reused.csv") %>%
  drop_na(chapter)

# count reuses
ch_count <- FMP_reused %>%
  group_by(chapter) %>%
  count(chapter)

# plot
ch_count %>%
  ggplot(aes(x = chapter, y = n)) +
  geom_step(fill = "#CD534CFF") +
  labs(subtitle = "Number of tweets reusing a chapter") +
  labs(title = "#MattiaTW", x = "Chapter", y = "Count")  +
  ggsave("ch_reuse.pdf", width = 6, height = 6)


# prepare tweets ------------------------------------------------------
  
# load all tweets
all_tweets <- read_tsv("all_tweets_clean.tsv")

#load tweets which are text reuse
tweet_reuse <- read_csv("reuse.csv")

# combine data about tweets and reuse
mattia_df <- all_tweets %>%
  left_join(tweet_reuse)

mattia_df <- mattia_df %>% 
  select(-Null, -Node, -Length) %>% 
  mutate(Reuse = ifelse(is.na(Reuse), FALSE, TRUE))

# select only tweets which are reuse
tweet_reuse <- mattia_df %>% 
  filter(Reuse == TRUE) %>% 
  drop_na(Text) %>% 
  mutate(Blast_text_length = nchar(Blast_text))

# select only tweets which are NOT reuse
tweet_noReuse <- mattia_df %>%
  select(-Cluster, -Blast_text) %>%
  filter(Reuse == FALSE)%>% 
  write_csv("tweet_noReuse.csv")
  
# remove punctuation
tweet_noReuse$Text <- gsub('[[:punct:] ]+',' ', tweet_noReuse$Text)

# tidy format
tidy_tweet_noReuse <- tweet_noReuse %>% 
  unnest_tokens(Words, Text) %>% 
  # mutate(Words = char_wordstem(Words, language = "italian")) %>%  # stemming
  arrange(Words)

tidy_tweet_noReuse <- tidy_tweet_noReuse[-(1:354), ] %>%  # remove numbers and weird words
  drop_na(Words)
  
tweet_noReuse <- tidy_tweet_noReuse %>% 
  group_by(ID, Group) %>% 
  summarise(Text = paste(Words, collapse = " ")) %>% 
  ungroup()


# Frequency analysis ------------------------------------------------------

# TF-IDF
group_words <- tidy_tweet_noReuse %>% 
  count(Group, Words, sort = TRUE)

total_words <- group_words %>% 
  group_by(Group) %>% 
  summarise(Total = sum(n))

group_words <- left_join(group_words, total_words)

group_words <- group_words %>% 
  bind_tf_idf(Words, Group, n) %>% 
  arrange(desc(tf))
  
my_stopwords <- tibble(stopwords("italian")) %>%
  rename(Words = "stopwords(\"italian\")") %>% 
  add_row(Words = c("ml", "aj", "mb", "dri", "g", "amv", "bj", "etta", "diosa", "sel", "siasegnale", "mm", "ross", "francescaau", "driano", "d"))

top10_tfidf <- group_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(Words = factor(Words, levels = rev(unique(Words)))) %>% 
  anti_join(my_stopwords) %>% 
  group_by(Group) %>% 
  top_n(10, tf_idf) %>% 
  ungroup()

ggplot(top10_tfidf, aes(Words, tf_idf, fill = Group)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  geom_text(aes(label= paste("(", n, ")", sep = "")), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  coord_flip() +
  ggsave("mattia_noReuse_tf-idf.pdf")
  
top10_tf <- group_words %>% 
    arrange(desc(tf)) %>%
    anti_join(my_stopwords) %>% 
    group_by(Group) %>% 
    top_n(10, tf) %>% 
    ungroup() %>% 
    mutate(Group = as.factor(Group),
           Words = reorder_within(Words, tf, Group))
  
ggplot(top10_tf, aes(Words, tf, fill = Group)) +
    geom_bar(stat = "identity",show.legend = FALSE) +
    labs(x = NULL, y = "tf") +
    facet_wrap(~Group, ncol = 2, scales = "free") +
    scale_x_reordered() +
    coord_flip() +
    ggsave("mattia_tf.pdf")


# Bigrams -----------------------------------------------------------------

tidy_bigrams <- tweet_noReuse %>% 
  unnest_tokens(Bigram, Text, token = "ngrams", n = 2)

bigrams_separated <- tidy_bigrams %>%
  separate(Bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% my_stopwords$Words) %>%
  filter(!word2 %in% my_stopwords$Words) %>% 
  drop_na()

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(Bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(Group, Bigram) %>%
  bind_tf_idf(Bigram, Group, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(Bigram = factor(Bigram, levels = rev(unique(Bigram)))) %>% 
  group_by(Group) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(Bigram, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label= paste("(", n, ")", sep = "")), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  coord_flip()

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# explore specific bigrams

target_words <- c("vita") # "libertà" "uomo", "essere" "morte"

bigrams_united_target <- bigrams_filtered %>%
  filter(word2 %in% target_words | word1 %in% target_words) %>%
  unite(Bigram, word1, word2, sep = " ")

bigram_tf_idf_target <- bigrams_united_target %>%
  count(Group, Bigram) %>%
  bind_tf_idf(Bigram, Group, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_target %>%
  arrange(desc(tf_idf)) %>%
  mutate(Bigram = factor(Bigram, levels = rev(unique(Bigram)))) %>% 
  group_by(Group) %>% 
  top_n(10) %>% 
  ungroup() %>%
  mutate(Group = as.factor(Group),
         Bigram = reorder_within(Bigram, tf_idf, Group)) %>% 
  ggplot(aes(Bigram, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip()

# Topic modeling ----------------------------------------------------------

my_stopwords <- tibble(stopwords("italian")) %>%
  rename(Words = "stopwords(\"italian\")") %>% 
  add_row(Words = c("s", "toc", "tac", "the", "tac", "ciò", "nè", "quel", "può"))

tweet_words <- tidy_tweet_noReuse %>% 
  anti_join(my_stopwords) %>% 
  count(Group, Words)
  
tweet_dtm <- tweet_words %>%
  cast_dtm(ID, Words, n)

tweet_dtm

mattia_lda <- LDA(noReuse_stemMatr, k = 5, control = list(seed = 1234))

mattia_topics <- tidy(mattia_lda, matrix = "beta")

top_terms <- mattia_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# Corpus analysis ---------------------------------------------------------

# create the corpus
noReuse_corpus <- corpus(tweet_noReuse, docid_field = "ID", text_field = "Text")

# create stemmed matrix
noReuse_stemMatr <- dfm(noReuse_corpus, groups = "Group", remove = c(stopwords("italian"), "mattia", "pascal", "adriano"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

noReuse_stemMatr <- dfm(noReuse_corpus, remove = c(stopwords("italian"), "mattia", "pascal", "adriano"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

topfeatures(noReuse_stemMatr, 20)
dfm_sort(noReuse_stemMatr)

# lexical diversity
ld <- textstat_lexdiv(noReuse_stemMatr, measure = "all")

# calculate text similarity
group_simil <- textstat_simil(noReuse_stemMatr, margin = "documents", method = "cosine")

mattia_trimmed <- dfm_trim(noReuse_stemMatr, min_termfreq = 5, min_docfreq = 3)

# hierarchical clustering - get distances on normalized dfm
mattia_dist_mat <- as.dist(textstat_dist(dfm_weight(mattia_trimmed, "prop"), margin = "documents"))

mattia_dist_mat_df <- as.data.frame(textstat_dist(dfm_weight(mattia_trimmed, "prop"), margin = "documents"))
sum(is.na(mattia_dist_mat))

mattia_dist_mat <- pivot_to_numeric_matrix(mattia_dist_mat_df, document1, document2, euclidean)

# hiarchical clustering the distance object
mattia_cluster <- fastcluster::hclust.vector(mattia_dist_mat)

list_cluster <- (cutree(mattia_cluster, k = 10))

clust_df <- as_tibble(list_cluster, rownames = "ID") %>% 
  rename(Hclust_out = value) %>%
  mutate_at("ID", as.numeric)

clust_df
tweet_noReuse
sum(is.na(clust_df))

tweet_noReuse_cl <- tweet_noReuse %>% 
  left_join(clust_df)

# plot as a dendrogram
plot(list_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

# topic modeling
trim_noReuse <- dfm_trim(noReuse_stemMatr, min_termfreq = 10)

set.seed(100)
if (require(stm)) {
  my_lda_fit20 <- stm(trim_noReuse, K = 5, verbose = FALSE)
  ggplot (my_lda_fit20)    
}

tidy_lda <- tidy(my_lda_fit20) %>% # tidy the stm output to use it with ggplot
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  ggsave("tidy_lda.pdf", width = 9, height = 9)
  
# topic modeling 2
dtm <- convert(noReuse_stemMatr, to = "topicmodels")
lda <- LDA(dtm, k = 2)

terms(lda, 10)

dtm$Topic <- topics(lda)
list_Topic <- as_tibble(head(topics(lda), 6598), rownames = "ID") %>% 
  rename(Topic = value)%>%
  mutate_at("ID", as.numeric)

tweet_noReuse_tm <- tweet_noReuse %>% 
  left_join(list_Topic) %>% 
  drop_na()

tweet_noReuse_tm %>% 
  count(Group, Topic) %>% 
  ggplot(aes(x = Group, y = n, fill = Topic)) +
    geom_bar(position = "fill", stat = "identity")


# Machine learning --------------------------------------------------------

# function for evaluation
performance <- function(mytable, verbose = TRUE) {
  truePositives <- mytable[1, 1]
  trueNegatives <- sum(diag(mytable)[-1])
  falsePositives <- sum(mytable[1, ]) - truePositives
  falseNegatives <- sum(mytable[, 1]) - truePositives
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / (truePositives + falseNegatives)
  accuracy <- sum(diag(mytable)) / sum(mytable)
  tnr <- trueNegatives / (trueNegatives + falsePositives)
  balanced_accuracy <- sum(c(precision, tnr), na.rm = TRUE) / 2
  if (verbose) {
    print(mytable)
    cat(
      "\n    precision =", round(precision, 2),
      "\n       recall =", round(recall, 2),
      "\n     accuracy =", round(accuracy, 2),
      "\n    bal. acc. =", round(balanced_accuracy, 2),
      "\n"
    )
  }
  invisible(c(precision, recall))
}

# sample tweet to be manually classified
set.seed(99)
sample_tweets <- sample_n(tweet_noReuse, 100) %>% 
  write_csv("sample_tweets2.csv")

set.seed(98)
sample_tweets <- sample_n(tweet_noReuse, 100) %>% 
  write_csv("sample_tweets3.csv")

# tweets manually categorized
sample_cat <- read_csv("sample_tweets_cat.csv")
sample_cat2 <- read_csv("sample_tweets2_cat.csv")
sample_cat3 <- read_csv("sample_tweets3_cat.csv")

sample_cat_all <- bind_rows(sample_cat, sample_cat2, sample_cat3)
sample_cat_13 <- bind_rows(sample_cat, sample_cat3) %>% write_csv("sample_tweets200.csv")

cat_summary <- sample_cat_all %>% 
  group_by(Category) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# define training and test set
dfmat_training <- corpus(sample_cat_13, docid_field = "ID", text_field = "Text") %>% 
  dfm(remove = c(stopwords("italian"), "mattia", "pascal", "adriano", "meis"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
topfeatures(dfmat_training, 20)

dfmat_test <- corpus(sample_cat2, docid_field = "ID", text_field = "Text") %>% 
  dfm(remove = c(stopwords("italian"), "mattia", "pascal", "adriano", "meis"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
topfeatures(dfmat_test, 20) 

# dfmat_predict <- corpus(tweet_noReuse, !tweet_noReuse$ID %in% sample_cat$ID, docid_field = "ID", text_field = "Text") %>% 
#  dfm(remove = c(stopwords("italian"), "mattia", "pascal", "adriano", "meis"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
# topfeatures(dfmat_test, 20) 
  
## train naive bayes
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$Category)
summary(tmod_nb)


# check results
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$Category
predicted_nb <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_nb)
tab_class

# assess test performance
confusionMatrix(tab_class, mode = "everything")

## train SVM
tmod_svm <- textmodel_svm(dfmat_training, y = docvars(dfmat_training, "Category"))
summary(tmod_nb)

# classify tweets
predicted_svm <- predict(tmod_svm, newdata = dfmat_test, type = "class")
tab_class <- table(predicted_svm, dfmat_test$Category) %>% performance()

## train CNN
toks_train <- corpus(sample_cat_13, docid_field = "ID", text_field = "Text") %>% 
    tokens() %>%
    tokens_remove(c(stopwords("italian"), "mattia", "pascal", "adriano", "meis"), valuetype = "fixed", padding = TRUE)

summary(toks_train)  

tmod_cnn <- textmodel_cnnlstmemb(toks_train, 
                                 y = docvars(toks_train, "Category"), 
                                 epochs = 5, verbose = 1)

sample_cat2_corpus <- corpus(sample_cat2, docid_field = "ID", text_field = "Text")

toks_predict <- corpus(sample_cat2, docid_field = "ID", text_field = "Text") %>% 
  tokens() %>%
  tokens_remove(c(stopwords("italian"), "mattia", "pascal", "adriano", "meis"), valuetype = "fixed", padding = TRUE)

pred_cnn <- predict(tmod_cnn, newdata = toks_predict)
table(pred_cnn)
tail(texts(sample_cat2_corpus)[pred_cnn == "Category"], 10)


