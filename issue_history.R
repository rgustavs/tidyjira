

res$issues %>% map_chr(. , "key")
b <- res$issues %>% map(., "changelog")

b %>% map_int(., length())


one_issue <- res$issues[c(1)] 

one_changelog <- one_issue[[1]]$changelog
rm(one_history, one_history_item)
one_history <- o
one_history$author <- NULL
one_history_item <- one_history$items[[1]]

one_history_item 

one_issue %>% as_data_frame

one_history %>% as_tibble %>% mutate(created = ymd_hms(created) %>% floor_date("day")) 

show(one_history)

%>% 
  mutate(issue_info = one_history %>% map_df())
one_history %>% as_tibble %>% mutate(n_hi = map_int(items, length))


one_history_item %>% as_tibble %>% select(field, fromString, toString)

a <- structure(
  list(
    id = "67289", 
    created = "2017-11-08", 
  items = list(
        structure(
          list(field = "status", fromString = "Backlog", toString = "In Progress"), .Names = c("field", 
      "fieldtype", "fieldId", "from", "fromString", "to", "toString"
    )))), .Names = c("id", "created", "items"))



hist1 <- list(field="type", from_string ="issue", to_string="bug")
hist2 <- list(field="status", from_string ="open", to_string="closed")
hist3 <- list(field="type", from_string ="bug", to_string="issue")

issue1 <- list(id="123", created = "2017-11-08", issue_history = list(hist1, hist2))
issue2 <- list(id="124", created = "2017-11-10", issue_history = list(hist1, hist3))
issue <- list(issue1, issue2)


map_df(one_history, as_tibble) %>% 
  mutate(histories = map(histories, as_tibble)) %>% 
  unnest()


target results

#-----
library(tidyverse)

map(issue, as.tibble) %>%
  map_df(~ rowwise(.) %>%
      mutate(issue_history = list(bind_rows(issue_history))) %>%
      unnest() )

#---------
map_df(issue, as_tibble) %>% 
  mutate(issue_history = map(issue_history, as_tibble)) %>% 
  unnest()

#-----------
map_df(one_history, as_tibble) %>% 
  mutate(items = map(., ~x$items, as_tibble)) %>% 
  unnest()

map(one_history$items %>% flatten, as_tibble)

str(issue, max.level = 1)
str(one_issue, max.level = 1)
#dput(one_issue)

str(one_history)
