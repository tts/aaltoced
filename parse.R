library(crevents)
library(tidyverse)
library(readxl)

aalto <- read.csv("DOI_q-08_02_2019.csv", stringsAsFactors = FALSE)
names(aalto) <- c("unit", "parent", "doi", "title", "subtitle", "journal", "year", "isbn", "issn", "isbne")

withdois <- aalto %>% 
  filter(doi != '') %>% 
  distinct(doi, .keep_all = TRUE) %>% 
  select(-isbn, -isbne)

dois <- as.list(withdois[, "doi"])

tweeted <- function(doi, from, to){
  print(paste0("Querying ", doi))
  x <- crev_query(obj_id = doi, source = "twitter", # with default row limit 1000
                  from_occurred_date = from, until_occurred_date = to)
  return(x$message$events)
}

res <- map_dfr(dois, ~ tweeted(.x, "2017-01-01", "2019-02-17"))

write.csv(res, "events.csv", row.names = FALSE)

res <- res %>% 
  mutate(doi = substr(obj_id, 17, length(obj_id))) 

alldata <- left_join(withdois, res)

write.csv(alldata, "alldata.csv", row.names = FALSE)

# Note that the format of Tweet and author IDs changed in January 2019. 
# They are now non-resolvable URIs. 
# "To read a Tweet in your browser, you can visit the URL: http://twitter.com/statuses/«ID»"
newtweets <- alldata %>% 
  filter(grepl("twitter://", subj.pid)) %>% 
  mutate(tweet_id = str_extract(subj.pid, "[0-9]+")) %>% 
  mutate(tweet = paste0("http://twitter.com/statuses/", tweet_id)) %>% 
  select(-subj.pid, -tweet_id)

oldtweets <- alldata %>% 
  filter(!grepl("twitter://", subj.pid)) %>% 
  rename(tweet = subj.pid)

alltweets <- rbind(newtweets, oldtweets) 

data2app <- alltweets %>% 
  filter(unit != 'Not published at Aalto University') %>% 
  mutate(year = str_sub(year, -4)) %>% 
  select(unit, parent, doi, obj_id, title, year, occurred_at, tweet)

#-------------------------
#
# Join with org data
#
#------------------------

org <- read_excel("org_hierarchy.xls", sheet = "Sheet2")

# The managing unit in the Pure record can be from any level (rg, dept, school, univ)
data_org_rg <- inner_join(org, data2app, by = c("Research group"="unit"))
data_org_dept <- inner_join(org, data2app, by = c("Department or research area"="unit"))
data_org_school <- inner_join(org, data2app, by = c("School"="unit"))
data_org_univ <- inner_join(org, data2app, by = c("University or research org"="unit"))

data_org_raw <- rbind(data_org_rg, data_org_dept, data_org_school, data_org_univ)

data_org <- data_org_raw %>% 
  distinct(doi, title, occurred_at, .keep_all = TRUE)

dataforapp <- data_org %>% 
  rename(Date = occurred_at,
         Tweet = tweet,
         Year = year) %>% 
  mutate(Tweet = ifelse(!is.na(Tweet), paste0("<a target='blank' href='", Tweet, "'>Tweet</a>"), ""),
         Article = paste0("<a target='blank' href='https://doi.org/", doi, "'>", title, "</a>")) %>% 
  select(School, `Department or research area`, `Research group`, Year, Article, title, Tweet, Date) %>% 
  arrange(School, `Department or research area`, `Research group`, Year, Article, Date)


#------------------------
# 
# Twitter stats 
#
#------------------------

stats_raw <- dataforapp %>% 
  group_by(School) %>% 
  mutate(Articles_by_school = n(),
         Tweets_by_school = sum(Tweet != ""),
         Tweets_article_ratio_school = paste0(round(Tweets_by_school/Articles_by_school,1),"%")) %>% 
  arrange(desc(Tweets_by_school)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`) %>% 
  mutate(Articles_by_dept = n(),
         Tweets_by_dept = sum(Tweet != ""),
         Tweets_article_ratio_dept = paste0(round(Tweets_by_dept/Articles_by_dept,1),"%")) %>% 
  arrange(desc(Tweets_by_dept)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`,`Research group`) %>% 
  mutate(Articles_by_rg = n(),
         Tweets_by_rg = sum(Tweet != ""),
         Tweets_article_ratio_rg = paste0(round(Tweets_by_rg/Articles_by_rg,1),"%")) %>% 
  arrange(desc(Tweets_by_rg)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`, `Research group`, Article) %>% 
  mutate(Tweets_by_article = sum(Tweet != "")) %>% 
  ungroup() 

dataforapp_w_stats <- left_join(dataforapp, stats_raw)

saveRDS(dataforapp_w_stats, "dataforapp_w_stats.rds")







