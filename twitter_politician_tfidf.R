# Twitter politician tf-idf analysis
# 10.27.19

# install required packages
install.package('rtweet')
install.packages('tidyverse')
install.packages('tidytext')

# load libraries ----
library(rtweet)
library(tidyverse)
library(tidytext)


# authenticate for Twitter API access ---- 
# first establish authentication. Replace the dummy values below with your own
# if you forgot to set up a Twitter app or don't know how to get these values see the link where you downloaded this script
create_token(
  app = "my_twitter_research_app",
  consumer_key = "XYznzPFOFZR2a39FwWKN1Jp41",
  consumer_secret = "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD",
  access_token = "9551451262-wK2EmA942kxZYIwa5LMKZoQA4Xc2uyIiEwu2YXL",
  access_secret = "9vpiSGKg1fIPQtxc5d5ESiFlZQpfbknEN1f1m2xe5byw7")



# 1 - collect some tweets to analyze ----
#or jump to step 2 if you've downloaded the "politician_timelines_10.27.19" data directly

# get Trump, Warren and Bernie's most recent 1000 tweets
trump <- get_timelines("realdonaldtrump", n = 1000)
warren <- get_timelines("SenWarren", n = 1000)
bernie<- get_timelines("SenSanders", n = 1000)

# combine data
politicians <- rbind(trump, warren, bernie)

# select only columns we need for tf-idf analysis
politicians_clean <- politicians %>% select(screen_name, text)


# 2 - preprocessing ----

# put data into tidy text format - note we use 'token = 'tweets'' for twitter-specific text preprocessing
politicians_tidy <- politicians_clean %>% 
  unnest_tokens(word, text, token = "tweets")



# 3 - term frequency analyses ----

# check out distribution of words across politicians
tweet_words <- politicians_tidy %>% count(screen_name, word, sort = TRUE)

# get total words to look at term frequency distributions
total_words <- tweet_words %>% 
  group_by(screen_name) %>% 
  summarize(total = sum(n))

tweet_words <- left_join(tweet_words, total_words)

# show distribution of words in politicians tweets -- typically see skewed frequency distri described by Zipf's law. 
# Applies to Twitter as well. But Trump's distribution looks a bit fragmented...
ggplot(tweet_words, aes(n/total, fill = screen_name)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~screen_name, scales = "free_y") +
  labs(x = "Count", y = "Term Frequency (N / Total)") + 
  theme_bw()

# compute tf-idf to find each politicians' most "important" words in recent tweets
tweet_words <- tweet_words %>%
  bind_tf_idf(word, screen_name, n)

# visualize to check out top 10 important words for each politician
tweet_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(screen_name) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, scales = "free") +
  coord_flip()

# save hi-res version
ggsave("Politician_Twitter_tfidf.png")



# 4 extra analyses ----

#I notice a weird token."ma" is Warren's most "important" word. Unclear what that is. Let's investigate:
warren %>% 
  filter(str_detect(text, "ma")) %>% 
  select(text)

#nothing looks weird above ("ma" is not being used separately from a quick visual inspection) 
#so its happening in the tidying step somewhere? 
#This will take some more investigation
#What I'm doing is identifiying full tweets that contain "ma" as produced by the tidy function
warren_clean <- warren %>% mutate(idx = row_number()) %>% select(idx,text)
warren_tidy <- warren_clean %>%  unnest_tokens(word, text, token = "tweets")
check <- warren_tidy %>% filter(word == "ma") %>% mutate(check = 1) %>% select(idx, check)
warren_check <- warren_clean %>% left_join(check, by = "idx") %>% filter(check == 1)
#By investigating the warren_check dataset we actually see it was her use of the state abbreviation for Massachusettes ("MA")
#So actually MA was indeed being used separately. Oops. "ma" is meaningful afterall because it represents her referring to Massachusettes

#however, if you wanted to remove unmeaningful words you find in your own data just use this method then re-plot:
stopwords <- tibble(word = c("yourword1", "yourword2", "yourword3"))

tweet_words <- anti_join(tweet_words, stopwords, 
                           by = "word")






