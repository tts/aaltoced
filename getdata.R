library(crevents)
library(tidyverse)
library(readxl)
library(rtweet)
library(lubridate)

# Had to first save this to xslx via OO, see https://github.com/tidyverse/readxl/issues/496#issuecomment-436355364
aalto <- read_excel("DOI_q-08_04_2019.xlsx")

aalto <- aalto %>% 
  rename(unit = `Organisational unit name-0`,
         parent = `Parent organisational units-1`,
         doi = `Electronic version(s) of this work. (By uploading the full text file authors accept the terms of electronic publishing))-2`,
         title = `Title of the contribution in original language-3`,
         year = `Publication statuses and dates > Date > Year-6`,
         id = `UUID-15`) %>% 
  select(unit, parent, doi, title, year, id)

percentage_of_dois <- (nrow(aalto[!is.na(aalto$doi),]) / nrow(aalto)) * 100

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

res <- map_dfr(dois, ~ tweeted(.x, "2017-01-01", "2019-04-08"))

write.csv(res, "events_20190408.csv", row.names = FALSE)

res <- res %>% 
  mutate(doi = substr(obj_id, 17, length(obj_id))) 

alldata <- left_join(withdois, res, by = "doi")

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

# Leaving out non-tweeted rows
tweets_combined <- inner_join(alltweets, tweet_statuses_fetched_selection, by = c("tweet_status" = "status_id"))

data2app <- tweets_combined %>% 
  filter(unit != 'Not published at Aalto University') %>% 
  mutate(year = str_sub(year, -4)) %>% 
  select(unit, parent, doi, obj_id, title, id.x, year, occurred_at, tweet, screen_name, description, followers_count, text, is_retweet, location)

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
         Followers = followers_count,
         Location = location,
         Year = year,
         Id = id.x) %>% 
  group_by(Id) %>%
  mutate(Elapsed = max(ymd_hms(Date)) - min(ymd_hms(Date)),
         Elapsed_in_hours = Elapsed / 3600,
         Elapsed_in_days = Elapsed / 86400,
         `Life span (days)` = as.numeric(round(Elapsed_in_days, 1))) %>% # Time diff in hours between the first and last/latest tweet
  ungroup() %>% 
  mutate(Link = ifelse(!is.na(Link), paste0("<a target='blank' href='", Link, "'>Link to tweet</a>"), ""),
         Article = paste0("<a target='blank' href='https://research.aalto.fi/en/publications/id(", Id, ").html'>", title, "</a>")) %>% 
  select(School, `Department or research area`, `Research group`, Year, Article, title, Tweet, Link, `Screen name of (re)tweeter`, Description, Location, Followers, Date, Retweet, `Life span (days)`) %>% 
  arrange(School, `Department or research area`, `Research group`, Year, Article, Date)


#------------------------
# 
# Twitter stats 
#
#------------------------

stats_raw <- dataforapp %>% 
  group_by(School) %>% 
  mutate(Articles_by_school = n(),
         Tweets_by_school = sum(!is.na(Tweet)),
         Tweets_article_ratio_school = paste0(round(Tweets_by_school/Articles_by_school,1),"%")) %>% 
  arrange(desc(Tweets_by_school)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`) %>% 
  mutate(Articles_by_dept = n(),
         Tweets_by_dept = sum(!is.na(Tweet)),
         Tweets_article_ratio_dept = paste0(round(Tweets_by_dept/Articles_by_dept,1),"%")) %>% 
  arrange(desc(Tweets_by_dept)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`,`Research group`) %>% 
  mutate(Articles_by_rg = n(),
         Tweets_by_rg = sum(!is.na(Tweet)),
         Tweets_article_ratio_rg = paste0(round(Tweets_by_rg/Articles_by_rg,1),"%")) %>% 
  arrange(desc(Tweets_by_rg)) %>% 
  ungroup() %>% 
  group_by(School, `Department or research area`, `Research group`, Article) %>% 
  mutate(Tweets_by_article = sum(Tweet != "")) %>% 
  ungroup() 

dataforapp_w_stats <- left_join(dataforapp, stats_raw)

saveRDS(dataforapp_w_stats, "dataforapp_w_stats.rds")







