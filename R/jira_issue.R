#' Returns a list of jira issues
#' 
#' input can either be a a jira connection or a file with raw data
#' 
#' @param con - a jira connection object
#' @param raw_issue_file - instead of alternative to online query. 
#'
#' @return a data frame containing issues
#' @export
#'
#' @examples
#' 
#' issue <- jira_issue(con)
#' issue <- jira_issue(issue_raw_file = "./issue_raw.RData")
jira_issue <- function(con = NULL, issue_raw_file = NULL){
  
  if(is.null(issue_raw_file)){
    #
    # online query
    #
    #Check input - expecting 3 arguments
    if(length(con) != 3)
      stop('Malformed connection')
    
    #Build query and execute
    #url <- file.path(con$host, "rest/api/latest/search?project=TO&maxResults=1000")
    #url <- file.path(con$host, "rest/api/latest/search?jql=assignee=marcgide&maxResults=1000")
    #url <- file.path(con$host, "rest/api/2/TO/search?")
    position <- 0
    increment <- 100
    issue <- NULL
  
    repeat{ 
      url <- file.path(con$host, paste0('rest/api/latest/search?jql=project%20%3D%20"TO"%20&startAt=', position, '&maxResults=', increment))
      #url <- file.path(con$host, paste0('rest/api/latest/search?&startAt=', position, '&maxResults=', increment))
      url
      res <- jira_get(url = url, user = con$user, password = con$password, verbose = TRUE)
      issue_element <- jira_issue_raw_2_issue(res$issues)
      if(is.null(issue)){ issue <- issue_element } else {issue <- bind_rows(issue, issue_element)}
      if( position + length(res$issues) > res$total){ break }
      print(paste0("position: ", position, "/", res$total))
      position <- position + increment
    }
  } else {
    #
    # file based input
    #
    load(issue_raw_file)
    if( !(exists("issue_raw")) )
      stop('Malformed raw file')
    issue <- jira_issue_raw_2_issue(issue_raw)
  }

  #return
  issue
  
}    

jira_issue_raw_2_issue <- function(issue_raw){
  # Transform output
  # using pattern from https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html
  issue <-  issue_raw %>% {
    tibble(
      #Identifiers
      id = map_chr(., "id"),
      key = map_chr(., "key"),
      summary = map_chr_hack(., ~ .x[["fields"]][["summary"]]),
      
      #Category
      issuetype_id = map_chr_hack(., ~ .x[["fields"]][["issuetype"]][["id"]]),
      issuetype_name = map_chr_hack(., ~ .x[["fields"]][["issuetype"]][["name"]]),
      
      #Effort - How much
      timespent = map_chr_hack(., ~ .x[["fields"]][["timespent"]]) %>% as.numeric(),
      
      #Who - creators,
      #resolution = map_chr_hack(., ~ .x[["fields"]][["resolution"]]),
      creator_key = map_chr_hack(., ~ .x[["fields"]][["creator"]][["key"]]),
      creator_name = map_chr_hack(., ~ .x[["fields"]][["creator"]][["name"]]),
      assignee_key = map_chr_hack(., ~ .x[["fields"]][["assignee"]][["key"]]),
      assignee_name = map_chr_hack(., ~ .x[["fields"]][["assignee"]][["name"]]),
      
      #Dates - when
      created_date = map_chr_hack(., ~ .x[["fields"]][["created"]]) %>% ymd_hms(),
      resolution_date = map_chr_hack(., ~ .x[["fields"]][["resolutiondate"]]) %>% ymd_hms(),
      last_viewed_date = map_chr_hack(., ~ .x[["fields"]][["lastViewed"]]) %>% ymd_hms()
    )
  }
  #return
  issue
  
}