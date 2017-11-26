# tidyjira
tidy formatatted exports of jira issues and issue worklog items

Prepare
```
library(purrr)
library(dplyr)
library(readr)
library(httr)
library(tidyr)
library(lubridate)
library(tidyjson)
library(tidyJira)
user <- "a_name"
password <- "a_password"
host <- "https://a_site.atlassian.net/"
project <- "APROJ"
```

Run 
```
con <- jira_connect(host, user, password, project)
issue <- jira_issue(con)
issue_worklog <- jira_issue_worklog(con, issue)
```

Investigate
```
issue_worklog %>% mutate(start_mon = month(wl_started)) %>% 
 group_by(start_mon, wl_updater_email) %>% 
 summarize(effort_hours = round(sum(time_spent_hours)), digits = 1)  %>%
 spread(start_mon, effort_hours, fill = "-")

issue_worklog %>% mutate(start_mon = month(wl_started)) %>% 
  group_by(wl_updater_email, project_key, start_mon) %>% 
  summarize(effort_hours = round(sum(time_spent_hours)), digits = 1)  %>%
  spread(start_mon, effort_hours, fill = "-") %>% as.data.frame()

```


