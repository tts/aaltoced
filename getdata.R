library(crevents)
library(tidyverse)
library(readxl)
library(rtweet)

aalto <- read_excel("DOI_q-05_04_2019.xls")

aalto <- aalto %>% 
  rename(unit = `Organisational unit name-0`,
         parent = `Parent organisational units-1`,
         doi = `Electronic version(s) of this work. (By uploading the full text file authors accept the terms of electronic publishing))-2`,
         title = `Title of the contribution in original language-3`,
         year = `Publication statuses and dates > Date > Year-6`) %>% 
  select(unit, parent, doi, title, year)

withdois <- aalto %>% 
  mutate(uniquedoi = substring(doi, regexpr(",", doi) + 1),
         uniquedoi = substring(uniquedoi, regexpr(":", uniquedoi) + 1),
         uniquedoi = gsub("https://doi.org/", "", uniquedoi),
         uniquedoi = gsub("https://dx.doi.org/", "", uniquedoi),
         uniquedoi = gsub("//dx.doi.org/", "", uniquedoi),
         uniquedoi = gsub("//doi.org/", "", uniquedoi),
         uniquedoi = gsub("\\s", "", uniquedoi)) %>% 
  filter(uniquedoi != '') %>% 
  rename(olddoi = doi,
         doi = uniquedoi) %>% 
  distinct(doi, .keep_all = TRUE) 

withdois$doi[withdois$doi == "0.3389/fpls.2014.00271"] <- "10.3389/fpls.2014.00271"

dois <- as.vector(unlist(as.list(withdois[, "doi"])))

tweeted <- function(doi, from, to){
  print(paste0("Querying ", doi))
  x <- crev_query(obj_id = doi, source = "twitter",
                  from_occurred_date = from, until_occurred_date = to)
  return(x$message$events)
}

res <- map_dfr(dois, ~ tweeted(.x, "2017-01-01", "2019-04-05"))

write.csv(res, "events.csv", row.names = FALSE)

res <- res %>% 
  mutate(doi = substr(obj_id, 17, length(obj_id))) 

alldata <- left_join(withdois, res)

write.csv(alldata, "alldata_20190408.csv", row.names = FALSE)

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

#------------------------
#
# Tweets
#
#------------------------

appname <- "rtweet_tokens_ttso"
key <- "yourkeyhere"
secret <- "yoursecrethere"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

alltweets <- alltweets %>% 
  mutate(tweet_status = str_extract(tweet, "[0-9]+$")) 

tweet_statuses_fetched <- rtweet::lookup_statuses(alltweets$tweet_status)

saveRDS(tweet_statuses_fetched, file = "tweet_status_fetched_20190408.RDS")

tweet_statuses_fetched_selection <- tweet_statuses_fetched %>% 
  select(status_id, screen_name, description, followers_count, text, is_retweet, location,
         retweet_status_id, retweet_screen_name, retweet_description, retweet_followers_count, retweet_text)

tweets_combined <- inner_join(alltweets, tweet_statuses_fetched_selection, by = c("tweet_status" = "status_id"))

data2app <- tweets_combined %>% 
  filter(unit != 'Not published at Aalto University') %>% 
  mutate(year = str_sub(year, -4)) %>% 
  select(unit, parent, doi, obj_id, title, year, occurred_at, tweet, screen_name, description, text, is_retweet, location)

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
         Link = tweet,
         Tweet = text,
         Retweet = is_retweet,
         `Screen name of (re)tweeter` = screen_name,
         Description = description,
         #Followers = folcount,
         Location = location,
         Year = year) %>% 
  mutate(Link = ifelse(!is.na(Link), paste0("<a target='blank' href='", Link, "'>Link to tweet</a>"), ""),
         Article = paste0("<a target='blank' href='https://doi.org/", doi, "'>", title, "</a>")) %>% 
  select(School, `Department or research area`, `Research group`, Year, Article, title, Tweet, Link, `Screen name of (re)tweeter`, Description, Location, Date, Retweet) %>% 
  arrange(School, `Department or research area`, `Research group`, Year, Article, Date)


#------------------------
# 
# Twitter stats 
#
#------------------------

stats_raw <- dataforapp %>% 
  group_by(School) %>% 
  mutate(Articles_by_school = n(),
         #Tweets_by_school = sum(Tweet != ""),
         Tweets_by_school = sum(!is.na(Tweet)),
         Tweets_article_ratio_school = paste0(round(Tweets_by_school/Articles_by_school,1),"%")) %>% 
  arrange(desc(Tweets_by_school)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`) %>% 
  mutate(Articles_by_dept = n(),
         #Tweets_by_dept = sum(Tweet != ""),
         Tweets_by_dept = sum(!is.na(Tweet)),
         Tweets_article_ratio_dept = paste0(round(Tweets_by_dept/Articles_by_dept,1),"%")) %>% 
  arrange(desc(Tweets_by_dept)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`,`Research group`) %>% 
  mutate(Articles_by_rg = n(),
         #Tweets_by_rg = sum(Tweet != ""),
         Tweets_by_rg = sum(!is.na(Tweet)),
         Tweets_article_ratio_rg = paste0(round(Tweets_by_rg/Articles_by_rg,1),"%")) %>% 
  arrange(desc(Tweets_by_rg)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`, `Research group`, Article) %>% 
  mutate(Tweets_by_article = sum(Tweet != "")) %>% 
  #mutate(Tweets_by_article = sum(!is.na(Tweet))) %>% 
  ungroup() 

dataforapp_w_stats <- left_join(dataforapp, stats_raw)

saveRDS(dataforapp_w_stats, "dataforapp_w_stats.rds")






