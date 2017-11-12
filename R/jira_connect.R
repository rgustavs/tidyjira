
#' Validate the connection to jira by logging in an performing a search
#'
#' @param host 
#' @param user 
#' @param password 
#' @param project 
#'
#' @return
#' @export
#'
#' @examples
jira_connect <- function(host, user, password, project = NULL) {
  if(is.null(host))
    stop('host is NULL.')
  
  if(is.null(user))
    stop("user is NULL")
  
  if(is.null(password))
    stop("password is NULL")
  
  con <- list(user=user, password=password, host=host)
  url <- file.path(host, "rest/api/latest/search?&maxResults=1")
  res <- jira_get(con = con, url = url)
  res <- res$issues
  
  #Check if we got an response
  if(length(res) != 1) {
    stop(paste0("Cannot connect to host:", host))
  }
  
  
  #Project parameter
  if(!is.null(project)) {
    url <- file.path(host, paste0('rest/api/latest/search?jql=project%20%3D%20"', project, '"%20&&maxResults=1'))
    res <- jira_get(con = con, url = url)
    res <- res$issues
    
    if(length(res) == 1){
      #working project
      con$project <- project
    } else {
      #failed to connect to project
      stop(paste0("No issues returned for project:", project))
    }
  }
  
  
  #Return
  con
  
}
