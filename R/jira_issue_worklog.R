#' iterates through issues and returns issue worklog items
#'
#' @param con - a validated jira connection
#' @param issue - uses issue$key to identify issues to iterate through
#'
#' @return a data frame containing issue_worklog items
#' @export
#'
#' @examples
#' issue_worklog <- jira_issue_worklog(con, issue)
jira_issue_worklog <- function(con, issue){
  
  i <- issue %>% filter(as.numeric(timespent)>0)
  r <- map(i$issue_key, ~ jira_issue_worklog_element(con,.x))
  a <- bind_rows(r)
  
  #post-processing
  a <- a %>% 
    mutate(time_spent_hours = as.numeric(time_spent_seconds)/3600) %>%
    mutate(started_day = floor_date(wl_started, unit="day"))
  #extending dimensions
  a <- left_join(a, 
    issue %>% select(id, issue_key, issuetype_name, project_key, issue_creator_email, issue_assignee_email), 
    by=c("issue_id"="id"))
  
  
  #return 
  a
}

jira_issue_worklog_element <- function(con, key){
  
  #Check input - expecting 3 arguments
  if(length(con) != 3 && length(con) != 4 )
    stop('Malformed connection')
  
  #
  print(paste0("processing id ", key))
  
  #Build query and execute
  url <- file.path(host, paste0("rest/api/2/issue/", key , "/worklog"))
  res <- jira_get(con = con, url = url)
  
  # Transform output
  issue_worklog_raw <- res$worklogs
  
  # using pattern from https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html
  issue_worklog <-  issue_worklog_raw %>% {
    tibble(
      #Identifiers
      self = map_chr(., "self"),
      id = map_chr(., "id"),
      issue_id = map_chr(., "issueId"),
      wl_comment = map_chr(., "comment"),
      created = map_chr(., "created") %>% ymd_hms(),
      updated = map_chr(., "updated") %>% ymd_hms(),
      wl_started = map_chr(., "started") %>% ymd_hms(),
      wl_assignee_email = map_chr_hack(., ~ .x[["author"]][["emailAddress"]]),
      wl_updater_email = map_chr_hack(., ~ .x[["updateAuthor"]][["emailAddress"]]),
      time_spent = map_chr(., "timeSpent"),
      time_spent_seconds = map_chr(., "timeSpentSeconds") %>% as.numeric()
    )
  }   
  
  #return
  issue_worklog
  
}