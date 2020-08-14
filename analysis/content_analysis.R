library(tidyverse)
library(tidytext)
library(quanteda)
library(scales)

# prepare tweets ------------------------------------------------------
  
# load all tweets
tweets_only <- read_tsv("data/all_tweets_clean.tsv")

#load tweets which are text reuse (identified with BLAST https://github.com/avjves/textreuse-blast)
tweet_reuse <- read_csv("data/reuse.csv")

# combine data about tweets and reuse
mattia_df <- tweets_only %>%
  left_join(tweet_reuse) %>% 
  select(-Null, -Node, -Length) %>% 
  mutate(Reuse = ifelse(is.na(Reuse), FALSE, TRUE),
         Group = str_replace(Group, "groupA", "A"),
         Group = str_replace(Group, "groupB", "B"),
         Group = str_replace(Group, "groupO2", "NR"),
         Group = str_replace(Group, "groupO1", "OtherReg"))

# select only tweets which are reuse
tweet_reuse <- mattia_df %>% 
  filter(Reuse == TRUE) %>% 
  drop_na(Text) %>% 
  mutate(Blast_text_length = nchar(Blast_text))

# select only tweets which are NOT reuse
tweet_noReuse_raw <- mattia_df %>%
  select(-Cluster, -Blast_text) %>%
  filter(Reuse == FALSE) %>% 
  drop_na(Text)
  
# remove punctuation
tweet_noReuse_raw$Text <- gsub('[[:punct:] ]+',' ', tweet_noReuse_raw$Text)

# tidy format
tidy_tweet_noReuse <- tweet_noReuse_raw %>% 
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

group_words <- left_join(group_words, total_words) %>% 
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
  geom_text(aes(label= paste("(", n, ")", sep = "")), color = "white", size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#9AD93CFF", "#1FA188FF", "#375B8DFF", "#440154FF")) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  ggsave("plots/mattia_noReuse_tf-idf.pdf")
  
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
  geom_text(aes(label= paste("(", n, ")", sep = "")), color = "white", size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#9AD93CFF", "#1FA188FF", "#375B8DFF", "#440154FF")) +
    labs(x = NULL, y = "tf") +
    facet_wrap(~Group, ncol = 2, scales = "free") +
    scale_x_reordered() +
    coord_flip() +
    theme_minimal() +
    ggsave("plots/mattia_tf.pdf")


# Lexical diversity ---------------------------------------------------------

# create the corpus
noReuse_corpus <- corpus(tweet_noReuse_raw, docid_field = "ID", text_field = "Text")

# create stemmed matrix
noReuse_stemMatr <- dfm(noReuse_corpus, groups = "Group", stem = TRUE)

# calculate lexical diversity
ld <- textstat_lexdiv(noReuse_stemMatr, measure = "all")


# Tweets classification ---------------------------------------------------

# sample tweet to be manually classified
set.seed(99)
sample_tweets <- sample_n(tweet_noReuse, 100) %>% 
  write_csv("sample_tweets2.csv")

set.seed(98)
sample_tweets <- sample_n(tweet_noReuse, 100) %>% 
  write_csv("sample_tweets3.csv")

# tweets manually categorized
sample_cat <- read_csv("data/sample_tweets_cat.csv")
sample_cat2 <- read_csv("data/sample_tweets2_cat.csv")
sample_cat3 <- read_csv("data/sample_tweets3_cat.csv")

sample_cat_all <- bind_rows(sample_cat, sample_cat2, sample_cat3)

cat_summary <- sample_cat_all %>% 
  group_by(Category) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
