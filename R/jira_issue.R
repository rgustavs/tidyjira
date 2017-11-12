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
    if(length(con) != 3 && length(con) != 4)
      stop('Malformed connection')
    
    #Build query and execute
    position <- 0
    increment <- 100
    issue <- NULL

    print("Getting issues:")  
    repeat{ 
      if(!is.null(con$project)){
        url <- file.path(con$host, paste0('rest/api/latest/search?jql=project%20%3D%20"', project, '"%20&startAt=', position, '&maxResults=', increment))
      } else {
        url <- file.path(con$host, paste0('rest/api/latest/search?&startAt=', position, '&maxResults=', increment))
      }
      url
      res <- jira_get(url = url, user = con$user, password = con$password, verbose = TRUE)
      issue_element <- jira_issue_raw_2_issue(res$issues)
      if(is.null(issue)){ issue <- issue_element } else {issue <- bind_rows(issue, issue_element)}
      if( position + length(res$issues) > res$total){ break }
      print(paste0("- position: ", position, "/", res$total))
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
  print(paste0("Fetched all issues (",res$total ,"/",res$total ,")" ))

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
      issue_key = map_chr(., "key"),
      summary = map_chr_hack(., ~ .x[["fields"]][["summary"]]),
      
      #What project and categories
      project_key = map_chr_hack(., ~ .x[["fields"]][["project"]][["key"]]),
      project_id = map_chr_hack(., ~ .x[["fields"]][["project"]][["id"]]),
      issuetype_id = map_chr_hack(., ~ .x[["fields"]][["issuetype"]][["id"]]),
      issuetype_name = map_chr_hack(., ~ .x[["fields"]][["issuetype"]][["name"]]),
      
      #Effort - How much
      timespent = map_chr_hack(., ~ .x[["fields"]][["timespent"]]) %>% as.numeric(),
      
      #Who - creators,
      #resolution = map_chr_hack(., ~ .x[["fields"]][["resolution"]]),
      #creator_key = map_chr_hack(., ~ .x[["fields"]][["creator"]][["key"]]),
      issue_creator_email = map_chr_hack(., ~ .x[["fields"]][["creator"]][["emailAddress"]]),
      #assignee_key = map_chr_hack(., ~ .x[["fields"]][["assignee"]][["key"]]),
      issue_assignee_email = map_chr_hack(., ~ .x[["fields"]][["assignee"]][["emailAddress"]]),
      
      #Dates - when
      created_date = map_chr_hack(., ~ .x[["fields"]][["created"]]) %>% ymd_hms(),
      resolution_date = map_chr_hack(., ~ .x[["fields"]][["resolutiondate"]]) %>% ymd_hms(),
      last_viewed_date = map_chr_hack(., ~ .x[["fields"]][["lastViewed"]]) %>% ymd_hms()
    )
  }
  #return
  issue
  
}