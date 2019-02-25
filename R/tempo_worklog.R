tempo_connect <- function(jira_host, tempo_appname, tempo_key, tempo_secret) {

  tempo_endpoint <- oauth_endpoint(
    authorize = paste0(jira_host, "/plugins/servlet/ac/io.tempo.jira/oauth-authorize"),
    access = "https://api.tempo.io/oauth/token/")
  tempo_app <- oauth_app(
    appname = tempo_appname, 
    key = tempo_key,
    secret =tempo_secret)
  tempo_token <- oauth2.0_token(tempo_endpoint, tempo_app, 
    query_authorize_extra = list(access_type = 'tenant_user', response_type = NULL, state = NULL))
  
  #return
  tempo_token

}

tempo_worklog <- function(token){
  select_worklog <- function(worklog){
    bind_cols(
      worklog %>% select(tempoWorklogId, jiraWorklogId, timeSpentSeconds, startDate, description) %>% 
        mutate(time_spent_hours = timeSpentSeconds/3600,
          worked_year_mon = floor_date(ymd(startDate), unit = 'month')),
      worklog$author %>% select(accountId, displayName),
      worklog$issue     
    )
  }
  block_size <- 1000
  fetch_size <- 1000
  i <- 0
  worklog <- NULL
  while(fetch_size == block_size){
    
    req <- GET("https://api.tempo.io/core/3/worklogs/",
      config(token = tempo_token),
      query = list(limit=block_size, offset=i*block_size)
    )
    r <- content(req, 'text', encoding = 'UTF-8') %>% fromJSON()
    if(is.null(worklog)){
      worklog <- r$results %>% select_worklog()
    } else {
      worklog <- bind_rows(worklog, r$results %>% select_worklog())
    }
    fetch_size <- r$results %>% nrow()
    i <- i + 1
    print(i*block_size)
  }
  
  #return
  worklog
  
}
