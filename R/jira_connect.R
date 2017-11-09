
#' Validate the connection to jira by logging in an performing a search
#'
#' @param host 
#' @param user 
#' @param password 
#'
#' @return
#' @export
#'
#' @examples
jira_connect <- function(host, user, password) {
  if(is.null(host))
    stop('host is NULL.')
  
  if(is.null(user))
    stop("user is NULL")
  
  if(is.null(password))
    stop("password is NULL")
  
  url <- file.path(host, "rest/api/latest/search?&maxResults=1")
  res <- jira_get(url = url, user = user, password = password)
  res <- res$issues
  
  #Check if we got an response
  if(length(res) != 1) {
    stop(paste0("Cannot connect to host:", host))
  }
  
  #Return
  con <- list(user=user, password=password, host=host)
  con
  
}
