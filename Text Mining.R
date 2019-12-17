### Text Mining

library(tidytext)
library(dplyr)
library(stringr)

## read stored data form API
new_york <- read.csv("./data/ny.csv")
sf <- read.csv("./data/sf.csv")
dallas <- read.csv("./data/dallas.csv")

ny_high_review_r <- read.csv("./data/ny_high_review.csv",stringsAsFactors = FALSE)
sf_high_review_r <- read.csv("./data/sf_high_review.csv",stringsAsFactors = FALSE)
dl_high_review_r <- read.csv("./data/dl_high_review.csv",stringsAsFactors = FALSE)


## Select ratings >= 4.5 in NY and SF
ny_high <- dplyr::filter(new_york,rating>=4.5)
sf_high <- dplyr::filter(sf,rating>=4.5)
dl_high <- dplyr::filter(dallas,rating>=4.5)

#### Prepare tidy test data 

### NY
tidy_ny <- ny_high_review_r %>% unnest_tokens(word, text)

tidy_ny <- tidy_ny %>%
    anti_join(stop_words)


tidy_ny %>%
    count(word, sort = TRUE) 

### SF
tidy_sf <- sf_high_review_r %>% unnest_tokens(word, text)

tidy_sf <- tidy_sf %>%
    anti_join(stop_words)


tidy_sf %>%
    count(word, sort = TRUE) 


### Dallas
tidy_dl <- dl_high_review_r %>% unnest_tokens(word, text)

tidy_dl <- tidy_dl %>%
    anti_join(stop_words)


tidy_dl %>%
    count(word, sort = TRUE) 



#### plot most frequent word for ny and sf high rating restaurants' reveiws
library(ggplot2)

tidy_ny %>%
    count(word, sort = TRUE) %>%
    filter(n > 50) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


tidy_sf %>%
    count(word, sort = TRUE) %>%
    filter(n > 20) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


tidy_dl %>%
    count(word, sort = TRUE) %>%
    filter(n > 20) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


#### Word frequencies


library(tidyr)

frequency <- bind_rows(mutate(tidy_ny, city = "NYC"),
                       mutate(tidy_sf, city = "SF"),
                       mutate(tidy_dl, city = "Dallas")) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(city, word) %>%
    group_by(city) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    spread(city, proportion) %>% 
    gather(city, proportion, `NYC`:`SF`)


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Dallas`, color = abs(`Dallas` - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~city, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Dallas", x = NULL)

### Let’s quantify how similar and different these sets of word frequencies are using a correlation test. 
### How correlated are the review words frequencies between NYC and SF in high restaurant?

cor.test(data = frequency[frequency$city == "SF",],
         ~ proportion + `Dallas`)

cor.test(data = frequency[frequency$city == "NYC",],
         ~ proportion + `Dallas`)


# Pearson's product-moment correlation
# 
# data:  proportion and NYC
# t = 65.43, df = 860, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9006398 0.9230629
# sample estimates:
#       cor 
# 0.9125345 

## 


#### sentiments analysis

### Most common positive and negative words

bing_word_counts_ny<- tidy_ny %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts_sf<- tidy_sf %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()


bing_word_counts_dl<- tidy_dl %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()


bing_word_counts_ny %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()


bing_word_counts_sf %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()


bing_word_counts_dl %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()


### Wordclouds

library(wordcloud)



custom_stop_words <- bind_rows(tibble(word = c("food","restaurant"), 
                                      lexicon = c("custom")), 
                               stop_words)


tidy_ny %>%
    anti_join(custom_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))



tidy_sf %>%
    anti_join(custom_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))


tidy_dl %>%
    anti_join(custom_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))


### We can use this visualization to see the most important positive and negative words

library(reshape2)

tidy_ny %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)



tidy_sf %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)


tidy_dl %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)





