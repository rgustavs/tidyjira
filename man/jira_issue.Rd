% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jira_issue.R
\name{jira_issue}
\alias{jira_issue}
\title{Returns a list of jira issues}
\usage{
jira_issue(con = NULL, issue_raw_file = NULL)
}
\arguments{
\item{con}{- a jira connection object}

\item{raw_issue_file}{- instead of alternative to online query.}
}
\value{
a data frame containing issues
}
\description{
input can either be a a jira connection or a file with raw data
}
\examples{

issue <- jira_issue(con)
issue <- jira_issue(issue_raw_file = "./issue_raw.RData")
}
