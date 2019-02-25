#' Returns a list of jira issues
#' 
#' input can either be a a jira connection or a file with raw data
#' 
#' @param con - a jira connection object
#'
#' @return a data frame containing issues
#' @export
#'
#' @examples
#' 
#' issue <- jira_issue(con)
jira_issue <- function(con = NULL, issue_raw_file = NULL, db_export = FALSE){
  
  if(is.null(issue_raw_file)){
    #
    # online query
    #
    #Check input - expecting 3 arguments
    if(length(con) != 3 && length(con) != 4)
      stop('Malformed connection')
    
    #Build query and execute
    position <- 0
    increment <- 50
    issue <- NULL
    issue_history <- NULL

    print("Getting issues:")  
    repeat{ 
      if(!is.null(con$project)){
        url <- file.path(con$host, paste0('rest/api/latest/search?jql=project%20%3D%20"', con$project, '"%20&expand=changelog&startAt=', position, '&maxResults=', increment))
      } else {
        url <- file.path(con$host, paste0('rest/api/latest/search?&startAt=', position, '&maxResults=', increment))
      }
      r <- jira_get(con = con, url = url) %>% content('text', encoding = 'UTF-8') %>% fromJSON()
      
      #work on issues
      issue_element <- r %>% select_issues()
      if(is.null(issue)){ issue <- issue_element } else {issue <- bind_rows(issue, issue_element)}
      
      #wrap up
      if( position + nrow(r$issues) >= r$total){ break }
      print(paste0("- position: ", position, "/", r$total))
      position <- position + increment
    }
    print(paste0("Fetched all issues (",r$total ,"/",r$total ,")" ))
  }
  
  #return
  issue 

  
}    

select_issues <- function(r){
  # Transform output
  a <- r$issues %>% select(id,key)
  b <- r$issues$fields %>% select(summary, created, resolutiondate, lastViewed, timespent, customfield_10018) %>% rename(epic_key = customfield_10018) %>% rename(created_date = created, resolution_date = resolutiondate, last_viewed_date = lastViewed) %>% mutate(created_date = ymd(substr(created_date,1,10)), resolution_date = ymd(substr(resolution_date,1,10)), last_viewed_date = ymd(substr(last_viewed_date,1,10)), summary = substr(summary,1,60))
  c <- r$issues$fields$project %>% select(id, key) %>% rename(project_id = id, project_key = key)
  d <- r$issues$fields$issuetype %>% select(id, name) %>% rename(issue_type_id = id, issue_type = name)
  e <- r$issues$fields$assignee %>% select(displayName, emailAddress) %>% rename(assignee_name = displayName, assignee_email = emailAddress)
  f <- r$issues$fields$reporter %>% select(displayName, emailAddress) %>% rename(reporter_name = displayName, reporter_email = emailAddress)
  
  o <- bind_cols(a,b,c,d,e,f)
  o <- o %>% mutate(
    
  )
  
  #return
  o
  
}