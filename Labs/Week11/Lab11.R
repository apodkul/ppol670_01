### Loading Data 

#install.packages("tidytext")
library(tidytext)
library(dplyr)
library(ggplot2)

## Project 1
### Example Data 
tweets <- read.csv("https://raw.githubusercontent.com/apodkul/ppol670_01/main/Labs/Week11/Climate_tweets.csv")

tweets %>% 
  glimpse()

### Prep
tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text')

stop_words %>%
  head()

tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words)


### Word Cloud 
tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

#install.packages("ggwordcloud")
#Vignette: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
library(ggwordcloud)
tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 50) %>%
  ggplot() + 
  geom_text_wordcloud(aes(label = word, size = n))

#using a different token type?
tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_ngrams(output = 'word', input = 'text', n = 2) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

### Sentiment Analysis 
bing_words <- get_sentiments('bing')

bing_words %>% 
  glimpse()

tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words) %>%
  inner_join(bing_words)


tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words) %>%
  inner_join(bing_words) %>%
  count(tweet_id, sentiment)


nrc_words <- get_sentiments(lexicon = 'nrc')

tweets %>% 
  mutate(tweet_id = 1:nrow(tweets)) %>%
  dplyr::select(tweet_id, text) %>%
  unnest_tokens(output = 'word', input = 'text') %>%
  anti_join(stop_words) %>%
  inner_join(nrc_words) %>%
  count(sentiment)


## Project 2
### Get Data 
library(rvest)

output_data <- c()
j <- 1
for(i in 2:5){
  
  list_of_links <- read_html(sprintf('https://ocasio-cortez.house.gov/media/press-releases?page=%s', 
                                     i)
                             ) %>%
    html_nodes('a') %>%
    html_attr('href')
  
  list_of_links <- list_of_links[stringr::str_detect(list_of_links, 'press-releases/')]
  list_of_links <- list_of_links[!duplicated(list_of_links) & !is.na(list_of_links)]
  
  for(ii in 1:length(list_of_links)){
    tmp_var <- read_html(stringr::str_c('https://ocasio-cortez.house.gov', 
                                                 list_of_links[ii])
                                  ) %>%
      html_nodes('.evo-press-release--full > div') %>%
      html_text2()
    output_data <- c(output_data, tmp_var)
    j <- j+1
  }
}

press_releases <- data.frame(id = 1:length(output_data), 
                             text = output_data)

View(press_releases)

### LDA 

# Prep dataset
### Convert to tidy 
press_releases %>% 
  unnest_tokens(output = 'word', input = 'text') %>% 
  anti_join(stop_words, by = c('word' = 'word')) %>%
  count(id, word, name = 'count') 

press_releases <- press_releases %>% 
  unnest_tokens(output = 'word', input = 'text') %>% 
  anti_join(stop_words, by = c('word' = 'word')) %>%
  count(id, word, name = 'count') 


pr_input <- press_releases %>%
  cast_dtm(id, word, count)

pr_input



# Estimate model 
pr_lda <- LDA(pr_input, k = 2, 
              control = list(seed = 1789))




# Get Beta Terms 
pr_topics <- tidy(pr_lda, matrix = 'beta')

pr_topics


pr_top_terms <- pr_topics %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta))

ggplot(pr_top_terms) + 
  geom_bar(aes(x = beta, y = term, fill = as.factor(topic)), 
           stat = 'identity') + 
  facet_wrap(~topic, scales = 'free')  + 
  theme(legend.position = 'none')


gg <- pr_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  tidyr::pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .005 | topic2 > .005) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(log_ratio) %>%
  mutate(term = reorder(term, log_ratio))

ggplot(gg) + 
  geom_bar(aes(x = log_ratio, y = term), 
           stat = 'identity')


# Get Gamma Terms 
gammas <- tidy(pr_lda, matrix = 'gamma')

#gammas$document <- factor(gammas$document, levels = 1:26)
ggplot(gammas) + 
  geom_bar(aes(x = topic, y = gamma), stat = 'identity') + 
  facet_wrap(~document)

