load("swap2and3.RData")
library(tidyverse)
library(binom)
data1 <- data[data$timestamp < '20160426000000',]
data1 <- data1 %>% filter(event_subTest == 'swap2and3' | is.na(event_subTest))

# Verify with counts on https://phabricator.wikimedia.org/T136017
data1 %>% group_by(event_subTest) %>% distinct(event_searchSessionId) %>% tally()
data1 %>% filter(event_action == 'visitPage' & !is.na(event_position) & event_position %in% c(2,3)) %>% group_by(event_subTest, event_position) %>% tally()
data1 %>% filter(event_action == 'click' & !is.na(event_position) & event_position %in% c(2,3)) %>% group_by(event_subTest, event_position) %>% tally()

# Clean-up:
data1 <- data1 %>% filter(
  (event_action == 'searchResultPage' & !is.na(event_hitsReturned)) |
  (event_action == 'click' & !is.na(event_position) & event_position > -1) |
  (event_action == 'visitPage' & !is.na(event_pageViewId)) |
  (event_action == 'checkin' & !is.na(event_checkin) & !is.na(event_pageViewId))
)
data1$event_subTest[is.na(data1$event_subTest)] <- "Control"
data1 <- data1[!duplicated(data1$id), ]
data1$date <- lubridate::ymd(substr(data1$timestamp, 1,8))
data1 <- data1 %>%
  group_by(date, event_subTest, event_mwSessionId, event_searchSessionId) %>%
  filter("searchResultPage" %in% event_action) %>%
  ungroup %>% as.data.frame() # remove search_id without SERP asscociated
## 6474 out of 301603 search sessions fall into both control and test group... Delete those sessions
temp <- data1 %>% group_by(event_searchSessionId) %>% summarise(count=length(unique(event_subTest)))
data1 <- data1[data1$event_searchSessionId %in% temp$event_searchSessionId[temp$count==1], ]; rm(temp)

data_summary <- data1 %>%
  group_by(`Test group` = event_subTest) %>%
  summarize(`Search sessions` = length(unique(event_searchSessionId)), `Events recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Events recorded` = sum(.$`Events recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Events recorded` = prettyNum(`Events recorded`, big.mark = ","))
knitr::kable(data_summary, format = "markdown", align = c("l", "l", "r", "r"))

# De-duplication
temp <- data1 %>%
  filter(event_action == "searchResultPage") %>%
  group_by(event_mwSessionId, event_searchSessionId, event_query) %>%
  mutate(new_page_id = min(event_pageViewId)) %>%
  ungroup %>%
  select(c(event_pageViewId, new_page_id)) %>%
  distinct
data1 <- left_join(data1, temp, by = "event_pageViewId"); rm(temp)
data1$new_page_id[is.na(data1$new_page_id)] <- data1$event_pageViewId[is.na(data1$new_page_id)] 
temp <- data1 %>%
  filter(event_action == "searchResultPage") %>%
  arrange(new_page_id, timestamp) %>%
  mutate(dupe = duplicated(new_page_id, fromLast = FALSE)) %>%
  select(c(id, dupe))
data1 <- left_join(data1, temp, by = "id"); rm(temp)
data1$dupe[data1$event_action != "searchResultPage"] <- FALSE
data1 <- data1[!data1$dupe & !is.na(data1$new_page_id), ] %>%
  select(-c(event_pageViewId, dupe)) %>%
  rename(page_id = new_page_id) %>%
  arrange(date, event_mwSessionId, event_searchSessionId, page_id, desc(event_action), timestamp)
# Summarize on a page-by-page basis for each SERP:
searches <- data1 %>%
  group_by(`test group` = event_subTest, event_mwSessionId, event_searchSessionId, page_id) %>%
  filter("searchResultPage" %in% event_action) %>% # keep only searchResultPage and click
  summarize(timestamp = timestamp[1], 
            results = ifelse(event_hitsReturned[1] > 0, "some", "zero"),
            clickthrough = "click" %in% event_action,
            `no. results clicked` = length(unique(event_position))-1,
            `first clicked result's position` = ifelse(clickthrough, na.omit(event_position)[1][1], NA), #There are some search with click, but position is NA
            `Clicked on position 2` = 2 %in% event_position,
            `Clicked on position 3` = 3 %in% event_position
            ) %>%
  arrange(timestamp)

# Summary Table
events_summary2 <- data1 %>%
  group_by(`Test group` = event_subTest) %>%
  summarize(`Search sessions` = length(unique(event_searchSessionId)), `Events recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Events recorded` = sum(.$`Events recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Events recorded` = prettyNum(`Events recorded`, big.mark = ","))
searches_summary <- searches %>%
  group_by(`Test group` = `test group`) %>%
  summarize(`Search sessions` = length(unique(event_searchSessionId)), `Searches recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Searches recorded` = sum(.$`Searches recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Searches recorded` = prettyNum(`Searches recorded`, big.mark = ","))
knitr::kable(inner_join(searches_summary, events_summary2, by=c("Test group", "Search sessions")), 
             format = "markdown", align = c("l", "r", "r", "r")) 

# Summarize on a page-by-page basis for each visitPage:
clickedResults <- data1 %>%
  group_by(test_group = event_subTest, event_mwSessionId, event_searchSessionId, page_id) %>%
  filter("visitPage" %in% event_action) %>% #only checkin and visitPage action
  summarize(timestamp = timestamp[1], 
            position = na.omit(event_position)[1][1],
            dwell_time=ifelse("checkin" %in% event_action, max(event_checkin, na.rm=T), 0),
            scroll=sum(event_scroll)>0) %>%
  arrange(timestamp)
clickedResults$dwell_time[is.na(clickedResults$dwell_time)] <- 0
# 74148 clickedResults
clickedResults$status <- ifelse(clickedResults$dwell_time=="420", 1, 2)

clickedResults_summary <- clickedResults %>%
  group_by(`Test group` = test_group) %>%
  summarize(`Visited pages` = length(unique(page_id))) %>% ungroup %>%
  {
    rbind(., tibble(
      `Test group` = "Total",
      `Visited pages` = sum(.$`Visited pages`)
    ))
  } %>%
  mutate(`Visited pages` = prettyNum(`Visited pages`, big.mark = ","))
knitr::kable(clickedResults_summary, format = "markdown", align = c("l", "r", "r"))

