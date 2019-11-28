library(jsonlite)
library(tidyverse) 
library(tidytext)
library(RedditExtractoR)
library(httr)
library(rlist)
library(glue)
library(blastula)
library(patchwork)
library(config)

texrazor_api_key <- config::get("textrazor", file = "creds/textrazor.yml")$key

subreddit <- function(start_date, end_date, subreddit, limit, sort, sort_type) {
  #reddit_fields <- "author,domain,full_link,num_comments,score,title,selftext,subreddit,subreddit_subscribers,url,thumbnail,created_utc"
  reddit_fields <- "author,domain,full_link,num_comments,title,subreddit,url,created_utc"
  base_url <- "https://api.pushshift.io/reddit/"
  api_url <- paste0(base_url,
                    "submission/search/?",
                    "subreddit=", subreddit,
                    "&after=", start_date,
                    "&before=", end_date,
                    "&limit=", limit,
                    "&sort=", sort,
                    "&sort_type=", sort_type,
                    "&fields=", reddit_fields,
                    "&unique=TRUE")  
  fetch <- fromJSON(api_url) 
  data <-  try(fetch$data)
  
  if(isTRUE(class(data)=="try-error"))
  { next}  
  else 
  { data } 
}

tr_topics <- function(api_key, text, url, text_type, extractors) {
  extractors <- paste0(extractors, collapse = ",")
  if(text_type == 'url') {
    body = list(url = url, extractors = extractors)
  }
  
  if(text_type == 'text') {
    body = list(text = text, extractors = extractors)
  }
  
  response <- POST(url = 'https://api.textrazor.com/',
                   add_headers("x-textrazor-key" = api_key),
                   body = body,
                   encode = "form")
  if(response$status_code == 200) {
    results <- content(response)
    return(results)
  } else {
    return(response$status_code)
  }
}

today <- Sys.Date()
yesterday <- today - 1

results <- subreddit(subreddit = "dataisbeautiful", 
                     start_date = yesterday, 
                     end_date = today, 
                     sort = "desc", 
                     limit = 100, 
                     sort_type = "num_comments")
n <- 5
top_n_posts <- head(results, n)
comments <- reddit_content(top_n_posts$full_link) %>% mutate(full_link = str_replace(URL, '\\?ref=search_posts', ''))

#comments %>% filter(author == user) %>% View()
post_scores <- comments %>% select(full_link, post_score) %>% unique()
post_and_score <- top_n_posts %>% left_join(post_scores, by = c('full_link' = 'full_link'))

flat_comments <- comments %>% 
  mutate(comment = gsub("[^\x20-\x7E]", "", comment)) %>%
  #mutate(comment = stri_escape_unicode(stri_enc_toutf8(comment))) %>%
  #mutate(type = stri_enc_detect(comment))
  group_by(full_link) %>% 
  summarise(comment_text = paste0(comment, collapse = "")) %>% mutate(comment_str_length = str_length(comment_text))

full_body_post <- post_and_score %>% left_join(flat_comments)

comment_topic_data <- list()

for(i in 1:5) {
  comment_class <- tr_topics(api_key = texrazor_api_key,
                    text = full_body_post$comment_text[i], 
                    text_type = 'text',
                    extractors  = c("topics"))
                    #extractors  = c("topics","entities"))
  
  comment_data_list <- list(
      reddit_url = full_body_post$full_link[i],
      coarse_topics = comment_class$response$coarseTopics %>% lapply(as.data.frame) %>% bind_rows(),
      topics = comment_class$response$topics %>% lapply(as.data.frame) %>% bind_rows()#,
      #entries = comment_class$response$entities %>% lapply(as.data.frame) %>% bind_rows()
    )
  
  comment_topic_data[[i]] <- comment_data_list
  print(i)
}

build_text_summary_charts <- function(comment_data, filter_reddit_url) {
  
  thread_data <- list.filter(comment_data, reddit_url == filter_reddit_url)
  
  print('Thread Data read in')
  
  major_themes <- thread_data[[1]]$coarse_topics %>%
    ggplot() + 
    geom_bar(aes(label, score), stat = 'identity', fill = 'blue') + 
    coord_flip() +
    labs(title = 'Major Themes from Comments')
  
  print('Major theme built')
  
  minor_themes <- thread_data[[1]]$topics %>% 
    arrange(-score) %>%
    head(20) %>% 
    ggplot() + 
    geom_bar(aes(label, score), stat = 'identity', fill = "red") +
    coord_flip() +
    labs(title = 'Minor Themes from Comments')
  
  print('Minor theme built')
  
  charts <- list(major = major_themes, minor = minor_themes)
  
  return(charts)

}

build_email_block <- function(i) {
  comment_charts <- build_text_summary_charts(comment_data = comment_topic_data, filter_reddit_url = full_body_post$full_link[i])
  
  post_title <- full_body_post$title[i]
  post_url = full_body_post$full_link[i]
  post_image <- full_body_post$url[i]
  post_combine_plot <- comment_charts$major + comment_charts$minor +  plot_layout(ncol = 1, heights = c(1, 3))
  
  email_block <- c(
    glue("
    ### Title: [{ post_title }]({ post_url })
    
    Post Image:
    ![post-image]({ post_image })
    "),
    add_ggplot(post_combine_plot),
    '- - -'
  )
  return(email_block)
}

full_email_blocks <- c()
for(i in 1:5) {
  results <- build_email_block(i)
  full_email_blocks <- append(full_email_blocks, results)
  print(i)
}  
  


email <- compose_email(header = "Welcome to the Gun Show!",
                       body = md(full_email_blocks),
                       footer = "This will be the final countdown.") 

email %>%
  smtp_send(
    from = "me@brettkobold.com",
    to = "brettkobo@gmail.com",
    subject = paste(Sys.Date(), "| Daily Visualization Summary"),
    credentials = creds_file(file = "creds/aws_creds")
  )


