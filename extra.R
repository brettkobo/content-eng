subreddit <- function(start_date, end_date, subreddit, limit, sort, sort_type) {
  api_url <- paste0(base_url,
                    "submission/search/?",
                    "subreddit=", subreddit,
                    "&after=", start_date,
                    "&before=", end_date,
                    "&limit=", limit,
                    "&sort", sort,
                    "&sort_type", sort_type,
                    "&unique=TRUE")  
  data <- fromJSON(api_url)  
}

#using google nlp analysis
text_nlp <- gl_nlp(string = full_body_post$comment_text, type = 'PLAIN_TEXT', nlp_type = c('classifyText'))
text_nlp <- gl_nlp(string = full_body_post$comment_text, nlp_type = c('classifyText'), encodingType = c("UTF8"))

topics %>% arrange(-score) %>% View()


loopable_subreddit <- function(date) {
  results <- subreddit(subreddit = "lifeprotips", start_date = date, end_date = date + 1, sort = "asc", limit = 1000, sort_type = "score")
}

cond_add_cols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

dates <- seq.Date(as.Date("2016-01-01"), as.Date("2019-03-05"), 1)

reddit_data_list <- map(dates, loopable_subreddit)

#extracting list of coloums names and then reducing it down to a single list of shared coloum names
shared_names <- map(reddit_data_list, colnames) %>% reduce(intersect)

#helps build the data.frames out to the full size based on the unique coloums
just_list_vars <- reddit_data_list %>% map(select_if, is.list) %>% map(colnames) %>% unlist %>% unique
all_names <- map(reddit_data_list, colnames) %>% unlist %>% unique

#selecting data with shared coloums
#reddit_data <- map_df(reddit_data_list, ~ cond_add_cols(., cname = all_names))
reddit_data <- reddit_data_list %>% 
  map(cond_add_cols, cname = all_names) %>%
  map(select, -just_list_vars) %>%
  bind_rows()

write_feather(reddit_data, path = "data/lifeprotips.feather")


#explore some of the data

reddit_data %>%
  filter(score > 500) %>% nrow()
ggplot() +
  geom_histogram(aes(score))

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


filter_data <- reddit_data %>% 
  select(created_utc, title, score, num_comments)

ngram <- function(input_data, num_of_words){ #, limit) {
  input_data %>%
    mutate(title = tolower(title) %>% gsub("lpt", "", title)) %>%
    unnest_tokens(ngram, title, token = "ngrams", n = num_of_words) %>%
    count(ngram, sort = TRUE) #%>%
  #anti_join(stop_words) %>%
  #head(limit) %>%
  #ggplot() +
  #geom_bar(aes(ngram, n), stat = "identity") +
  #coord_flip()
}

bigram <- reddit_data %>%
  filter(score > 200) %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         word1 != "lpt") %>%
  count(word1, word2, sort = TRUE)





bigram %>%
  filter(n > 3, !str_detect(word1, "\\d"), !str_detect(word2, "\\d")) %>%
  graph_from_data_frame() %>%
  visualize_bigrams()


ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
