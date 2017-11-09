#---------------------------
#file container helper functions
#

#
# GET data from jira
#
#' Runs a rest GET call towards jira 
#'
#' @param url 
#' @param user 
#' @param password 
#' @param verbose 
#'
#' @return a parsed rest document (in the shape of a list)
#' @export
#'
#' @examples
#' 
#' res <- jira_get(url = url, user = user, password = password)

jira_get <- function(url = url, user = user, password = password, verbose = verbose){
  
  verbose <- TRUE
  res <- GET(url = url,
    authenticate(user = user, password = password, "basic"),
    add_headers("Content-Type" = "application/json")
    #verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  res <- content(res, as = "parsed")
  
  res$startAt 
  return(res)
}


#
#' to avoid issues with map_chr and NULL
#' @param .x 
#' @param .f 
#' @param ... 
#'
#' @return a character vector
#' @export
#'
#' @examples
#' A helper function from https://github.com/jennybc/analyze-github-stuff-with-r#readme
#' same arguments as map_chr
#' 
#'   issue <-  res$issues %>% {
#'      tibble(issuetype_id = map_chr_hack(., ~ .x[["fields"]][["issuetype"]][["id"]]))} 
map_chr_hack <- function(.x, .f, ...) {
  map(.x, .f, ...) %>%
    map_if(is.null, ~ NA_character_) %>%
    flatten_chr()
}

