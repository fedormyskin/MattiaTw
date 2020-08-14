library(tidyverse)
library(tidytext)
library(treemapify)
library(digest)


# data preparation --------------------------------------------------------

# load full database
mattia_anon <- read_csv("data/mattia_db_completo_aug2020_anon.csv")

# load database with manually checked chapters mentions
mattia_full_ch_only <- read_csv("data/mattia_full_tidy_ch_only_anon.csv")

# merge datasets
pre_merge <- mattia_full_ch_only %>% 
  select(index, group_fix)

full_author_fix <- mattia_anon %>% 
  left_join(pre_merge, by = "index") %>% 
  mutate(group_fix = ifelse(is.na(group_fix), "NR", group_fix),
         group_fix = ifelse(author %in% target_authors, "organizers", group_fix)) %>% 
  select(-group)

# tw, rt, and engagement per chapter --------------------------------------

tot_authors <- full_author_fix %>% 
  group_by(group_fix) %>% 
  summarise(tot_authors = n_distinct(author))

months_count <- full_author_fix %>% 
  group_by(month, group_fix) %>% 
  summarise(tw_month = n_distinct(index[retweet=="tweet"]),
            rt_month = n_distinct(index[retweet=="retweet"]),
            tot_month = tw_month + rt_month)

# n_distinct(full_author_fix$index[full_author_fix$retweet=="tweet"])

mattia_full_count <- mattia_full_ch_only %>% 
  left_join(full_author_fix %>% select(index, `total engagement`)) %>% 
  group_by(chapter, group_fix, retweet) %>% 
  summarise(n_tw_ch = n_distinct(index[retweet=="tweet"]),
            n_rt_ch = n_distinct(index[retweet=="retweet"]),
            tot_tw_rt = n_distinct(index),
            tot_eng = sum(`total engagement`))

# plot
mattia_full_count %>% 
  group_by(group_fix) %>% 
  ggplot(aes(x = chapter, color = group_fix, shape = retweet, linetype = retweet)) +
  geom_point(aes(y = n_tw_ch), size = 1.3) + geom_line(aes(y = n_tw_ch), size = 1.1) +
  geom_point(aes(y = n_rt_ch), size = 1.3) + geom_line(aes(y = n_rt_ch), size = 1.1) +
  geom_point(aes(y = tot_eng), size = 0.8) +
  geom_line(aes(y = tot_eng), size = 0.3) +
  scale_shape_manual(values = c(21, 16)) + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_x_continuous(breaks = 1:20) +
  scale_color_manual(values = c("#9AD93CFF", "#1FA188FF", "#375B8DFF", "orange", "#440154FF")) + # from viridis palette
  labs(x = "Chapter", y = "Count", color = "Group", linetype = "Type", shape = "Type") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  ggsave("plots/per_chapter.png", width = 8, height = 5)

# average counts ----------------------------------------------------------

mattia_full_avg <- mattia_full_ch_only %>%
  filter(!author %in% target_authors,
         between(chapter, 1, 18)) %>% # chapters 19 and 20 are outliers (epilogues)
  mutate(phase = ifelse(between(chapter, 1, 10), 1, 2)) %>% 
  group_by(phase, chapter, group_fix, author) %>% 
  summarise(tw = n_distinct(index[retweet=="tweet"]),
            rt = n_distinct(index[retweet=="retweet"])) %>%
  group_by(phase, chapter, group_fix) %>% 
  summarise(tw_ch = sum(tw),
            rt_ch = sum(rt)) %>% 
  left_join(tot_authors) %>% 
  mutate(tw_usr_ch = tw_ch/tot_authors,
         rt_usr_ch = rt_ch/tot_authors)

phases_summary <- mattia_full_avg %>% 
  group_by(phase, group_fix) %>% 
  summarise(tw_tot = sum(tw_ch),
            rt_tot = sum(rt_ch),
            avg_tw_ch = sum(tw_ch)/n_distinct(chapter),
            avg_rt_ch = sum(rt_ch)/n_distinct(chapter),
            tw_usr = sum(tw_ch)/mean(tot_authors),
            rt_usr = sum(rt_ch)/mean(tot_authors),
            tw_usr_ch = tw_usr/n_distinct(chapter),
            rt_usr_ch = rt_usr/n_distinct(chapter)) %>% 
  pivot_longer(tw_tot:rt_usr_ch, names_to = "metric", values_to = "count") %>%
  group_by(group_fix, metric) %>% 
  mutate(variation = ifelse(group_fix != "A", (count - lag(count))/lag(count)*100, NA)) # calculate variation only for groups who participated in both phases


# treemap of authors' activity --------------------------------------------

author_count <- full_author_fix %>% 
  group_by(group_fix, author) %>% 
  summarise(Tweets = n_distinct(index[retweet == "tweet"]),
            Retweets = n_distinct(index[retweet == "retweet"])) %>% 
  pivot_longer(Tweets:Retweets, names_to = "metric", values_to = "count") %>% 
  mutate(organizers = ifelse(author %in% target_authors, author, NA))

author_count %>%
  ggplot(aes(area = count, fill = group_fix, label = count, subgroup = group_fix)) +
  geom_treemap(start = "topleft") +
  geom_treemap_subgroup_border(start = "topleft") +
  geom_treemap_text(start = "topleft", place = "topleft", size = 10, color = "white") +
  scale_fill_manual(values = c("#9AD93CFF", "#1FA188FF", "#375B8DFF", "orange", "#440154FF")) +
  facet_wrap(~metric, nrow = 2) +
  labs(fill = "Group") +
  theme_minimal() +
  ggsave("plots/treemap.png", width = 8, height = 5)


# Users mentions ----------------------------------------------------------------
# this section needs some tweaking in order to be replicated, because the authors have been anonymized

mentions_df <- full_author_fix %>% 
  select(index, author_mentioned) %>% 
  unnest_tokens(mentions, author_mentioned, to_lower = FALSE) %>% 
  filter(mentions != "mattiatw") # %>% 
  # write_csv("data/mentions.csv")

# manually checked usernames  
mentions_ok <- read_csv("data/mentions_checked.csv") %>% 
  group_by(index) %>% 
  summarise(Mentions = paste(mentions, collapse = " ")) %>% 
  ungroup()

# add mentions info to dataset
authors <- unique(full_author_fix$author)

mattia_mentions <- full_author_fix %>% 
  select(-author_mentioned) %>% 
  left_join(mentions_ok) %>% 
  unnest_tokens(mentions, Mentions, to_lower = FALSE) %>% 
  filter(mentions %in% author)

n_distinct(mattia_mentions$mentions) # 329 users

# network analysis
library(tidygraph)
library(ggraph)

nodes <- mattia_mentions %>% 
  filter(retweet == "tweet") %>% 
  select(author, group_fix) %>% 
  distinct(author, .keep_all = TRUE)

edges <- mattia_mentions %>% 
  filter(retweet == "tweet") %>% 
  select(author, mentions) %>%
  filter(mentions %in% nodes$author) %>% 
  dplyr::rename(from = author, to = mentions)

# edges %>% write_csv("data/edges.csv")
# nodes %>% write_csv("data/nodes.csv")

net <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>% 
  activate(nodes) %>%
  mutate(Centrality = centrality_degree(), # change method to calculate centrality, if you wish
         org_label = ifelse(author %in% target_authors, paste(author, "(", Centrality, ")", sep = ""), NA))

net %>% 
  activate(nodes) %>%
  activate(edges) %>%
  ggraph(layout = 'kk') +
  geom_edge_link(color="gray50", width=0.2) +
  geom_node_point(aes(colour = group_fix, size = Centrality)) + 
  geom_node_text(aes(label = org_label), check_overlap = TRUE, vjust = "top") +
  scale_color_manual(values = c("#9AD93CFF", "#1FA188FF", "#375B8DFF", "orange", "#440154FF")) + 
  theme_graph() +
  ggsave("plots/network_labels.png", width = 15, height = 10)
