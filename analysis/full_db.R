library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(scales)
library(viridis)
library(treemapify)


# FMP most reused parts ---------------------------------------------------

# load full database
mattia_full <- read_csv2("mattia_db_completo_aug2020.csv")

# tidy
mattia_full_tidy <- mattia_full %>% 
  select(author, retweet, month, content, `media type`, `total engagement`, group, hashtag_mentioned) %>% 
  rowid_to_column("index") %>% 
  mutate(group = str_replace(group, "O2", "NR"),
         group = str_replace(group, "O1", "OtherReg"),
         retweet = str_replace(retweet, "0", "tweet"),
         retweet = str_replace(retweet, "1", "retweet"),
         `total engagement` = na_if(`total engagement`, "N/A"),
         `total engagement` = as.double(replace_na(`total engagement`, 0)))

# mattia_full_ch_only <- mattia_full_tidy %>% 
#   mutate(chapter = str_extract(hashtag_mentioned, "\\d+")) %>% 
#   select(-hashtag_mentioned) %>% 
#   drop_na(chapter) %>% 
#   arrange(chapter)

reg_authors <- read_csv("database_classi.csv") %>% 
  pivot_longer(everything(), names_to = "group_fix", values_to = "author") %>% 
  drop_na()

reg_authors[duplicated(reg_authors$author), ]

count_reg_authors <- reg_authors %>% 
  group_by(group_fix) %>% 
  summarise(n_authors = n_distinct(author))

# mattia_full_tidy <- mattia_full_tidy %>% 
#   left_join(reg_authors, by = "author") %>% 
#   mutate(group_fix = ifelse(is.na(group_fix), "NR", group_fix))

# check <- authors_fix %>% 
#   filter(is.na(authors_fix$group_fix) & authors_fix$group != "NR") %>% 
#   distinct(author, group)

# manually check chapter numbers
# write_csv(mattia_full_ch_only, "mattia_full_tidy.csv")
mattia_full_ch_only <- read_csv("mattia_full_tidy.csv") %>%
  left_join(reg_authors, by = "author") %>% 
  mutate(group_fix = ifelse(is.na(group_fix), "NR", group_fix)) %>% 
  mutate(retweet = str_replace(retweet, "0", "tweet"),
         retweet = str_replace(retweet, "1", "retweet"))

mattia_full_ch_only[duplicated(mattia_full_ch_only$index), ]

# merge datasets
pre_merge <- mattia_full_ch_only %>% 
  select(index, group_fix)

full_author_fix <- mattia_full_tidy %>% 
  left_join(pre_merge, by = "index") %>% 
  mutate(group_fix = ifelse(is.na(group_fix), "NR", group_fix))

full_author_fix[duplicated(full_author_fix$index), ]

tot_authors <- full_author_fix %>% 
  group_by(group_fix) %>% 
  summarise(tot_authors = n_distinct(author))

months_count <- full_author_fix %>% 
  group_by(month, group_fix) %>% 
  summarise(tw_month = n_distinct(index[retweet=="tweet"]),
            rt_month = n_distinct(index[retweet=="retweet"]),
            tot_month = tw_month + rt_month)

# tw, rt, and engagement per chapter
mattia_full_count <- mattia_full_ch_only %>% 
  left_join(full_author_fix %>% select(index, `total engagement`)) %>% 
  group_by(chapter, group_fix, retweet) %>% 
  summarise(n_tw_ch = n_distinct(index[retweet=="tweet"]),
            n_rt_ch = n_distinct(index[retweet=="retweet"]),
            tot_tw_rt = n_distinct(index),
            tot_eng = sum(`total engagement`))

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
  scale_color_viridis(begin = 0, end = .85, discrete = TRUE, direction = -1) +
  labs(x = "Chapter", y = "count") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  ggsave("plots/per_chapter.pdf", width = 8, height = 5)

# average counts
target_authors <- c("TwLetteratura", "MattiaPascalTw", "AdrianoMeisTw")

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

# treemap
author_count <- full_author_fix %>% 
  group_by(group_fix, author) %>% 
  summarise(Retweets = n_distinct(index[retweet == "tweet"]),
            Tweets = n_distinct(index[retweet == "retweet"])) %>% 
  pivot_longer(Retweets:Tweets, names_to = "metric", values_to = "count") %>% 
  mutate(organizers = ifelse(author %in% target_authors, TRUE, FALSE))

author_count %>%
  ggplot(aes(area = count, fill = group_fix, label = count, subgroup = group_fix)) +
  geom_treemap(start = "topleft") +
  geom_treemap_subgroup_border(start = "topleft") +
  geom_treemap_text(start = "topleft", place = "topleft", size = 10, color = "white") +
  scale_fill_viridis(begin = 0, end = .85, discrete = TRUE, direction = -1) +
  facet_wrap(~metric, nrow = 2) +
  theme_minimal() +
  ggsave("plots/treemap.pdf", width = 8, height = 5)
