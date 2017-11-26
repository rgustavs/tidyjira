#' Returns a list of jira related data frames 
#' 
#' @param con - a jira connection object
#'
#' @return a list of data frames
#' @export
#'
#' @examples
#' 
#' jira_db <- jira_issue(con)
jira_db <- function(con = NULL, issue_raw_file = NULL, db_export = FALSE){
  jira_db <- jira_issue(con, db_export = TRUE)
  jira_db$issue_worklog <- jira_issue_worklog(con, jira_db$issue)
  
  #return
  jira_db
}
  

  