library(tidyverse)
library(httr)
library(lubridate)
library(jsonlite)

con <- jira_connect(
  host = Sys.getenv("jira_host"), 
  user = Sys.getenv("jira_user"),
  password = Sys.getenv("jira_password"), 
  project = "BM")
con$project <- "BM"
issue1 <- jira_issue(con)
con$project <- "SOPS"
issue2 <- jira_issue(con)
con$project <- "ET"
issue3 <- jira_issue(con)
con$project <- "INTD"
issue4 <- jira_issue(con)
con$project <- "CUF"
issue5 <- jira_issue(con)
con$project <- "AB"
issue6 <- jira_issue(con)

issue <- bind_rows(issue1, issue2, issue3, issue4, issue5, issue6)

tempo_token <- tempo_connect(
  jira_host = Sys.getenv("jira_host"),
  tempo_appname = Sys.getenv("tempo_appname"), 
  tempo_key = Sys.getenv("tempo_key"), 
  tempo_secret = Sys.getenv("tempo_secret")
)
worklog <- tempo_worklog(tempo_token)

#setting epics names 
epics <- issue %>% select(summary, key) %>% filter(key %in% unique(issue$epic_key)) %>% 
  mutate(epic_name = substr(summary, 1, 20)) %>% rename(epic_key=key) %>% select(-summary)

worklog_ext <- left_join(worklog, issue, by = "key") %>% left_join(epics, by ="epic_key", copy = TRUE)

w


worklog_ext %>% filter(key == "BM-174")

#Epics
worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  filter(displayName %in% c('Aleksandrs Bugajs', 'Edgars Alksnis', 'Nauris Upitis', 'Maksims Riks', 'Deniss Ivanovs')) %>% 
#  filter(!is.na(epic_key)) %>%
  group_by(project_key, epic_name, worked_year_mon) %>% 
  summarise(time_spent_hours = sum(time_spent_hours)) %>% 
  spread(worked_year_mon, time_spent_hours, fill = "-")





worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  filter(displayName %in% c('Aleksandrs Bugajs', 'Edgars Alksnis', 'Nauris Upitis', 'Maksims Riks', 'Deniss Ivanovs')) %>% 
  group_by(displayName, worked_year_mon) %>% 
  summarise(time_spent_hours = sum(time_spent_hours)) %>% 
  spread(worked_year_mon, time_spent_hours, fill = "-")

worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  filter(displayName %in% c('Aleksandrs Bugajs', 'Edgars Alksnis', 'Nauris Upitis', 'Maksims Riks', 'Deniss Ivanovs')) %>% 
  filter(project_key == 'BM') %>%
  group_by(epic_key, epic_name, worked_year_mon) %>% 
  summarise(time_spent_hours = sum(time_spent_hours)) %>% 
  spread(worked_year_mon, time_spent_hours, fill = "-")




worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  filter(displayName %in% c('Aleksandrs Bugajs', 'Edgars Alksnis', 'Nauris Upitis', 'Maksims Riks', 'Deniss Ivanovs')) %>% 
  filter(is.na(project_key)) %>%
  group_by(project_key, epic_name, worked_year_mon, key) %>% 
  summarise(time_spent_hours = sum(time_spent_hours)) %>% 
  spread(worked_year_mon, time_spent_hours, fill = "-")


worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  filter(displayName %in% c('Aleksandrs Bugajs', 'Edgars Alksnis', 'Nauris Upitis', 'Maksims Riks', 'Deniss Ivanovs')) %>% 
  group_by(epic_name, displayName, worked_year_mon) %>% 
  summarise(time_spent_hours = sum(time_spent_hours)) %>% 
  spread(worked_year_mon, time_spent_hours, fill = "-") %>% as.data.frame()



worklog_ext %>% filter(year(worked_year_mon) == 2019) %>% 
  group_by(displayName) %>% 
  summarise(time_spent_hours = sum(time_spent_hours))%>% as.data.frame()


#summarize per indivual and epis and week

issue_worklog %>% mutate(start_mon = month(wl_started)) %>% 
  group_by(start_mon, wl_updater_email) %>% 
  summarize(effort_hours = round(sum(time_spent_hours)), digits = 1)  %>%
  spread(start_mon, effort_hours, fill = "-")

issue_worklog %>% mutate(start_mon = month(wl_started)) %>% 
  group_by(wl_updater_email, project_key, start_mon) %>% 
  summarize(effort_hours = round(sum(time_spent_hours)), digits = 1)  %>%
  spread(start_mon, effort_hours, fill = "-") %>% as.data.frame()
