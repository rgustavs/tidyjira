% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jira_issue_worklog.R
\name{jira_issue_worklog}
\alias{jira_issue_worklog}
\title{iterates through issues and returns issue worklog items}
\usage{
jira_issue_worklog(con, issue)
}
\arguments{
\item{con}{- a validated jira connection}

\item{issue}{- uses issue$key to identify issues to iterate through}
}
\value{
a data frame containing issue_worklog items
}
\description{
iterates through issues and returns issue worklog items
}
\examples{
issue_worklog <- jira_issue_worklog(con, issue)
}
