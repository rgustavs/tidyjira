#' Extraction issue history tree from issue
#'
#' @param con 
#' @param issue_raw 
#'
#' @return issue_history
#' @export
#'
#' @examples
jira_issue_history <- function(issue_raw){
  
  issue_raw %>% content(type = "text") %>%  as.tbl_json() %>% 
    #Drill down to elements
    enter_object("issues") %>% 
    gather_array() %>% 
    spread_values("key" = jstring("key")) %>% 
    spread_values("id" = jstring("id")) %>% 
    enter_object("changelog") %>%
    enter_object("histories") %>% 
    gather_array() %>% 
    spread_values("hist_id" = jstring("id")) %>%
    spread_values("created" = jstring("created")) %>%
    enter_object("items") %>% 
    gather_array() %>% 
    spread_values(
      fieldId = jstring("fieldId"),
      fromString = jstring("fromString"),
      toString = jstring("toString")
    ) %>% 
    #Select and process
    filter(fieldId == "status") %>%
    select(-document.id, - array.index) %>%
    mutate( created = created %>% ymd_hms())
}
