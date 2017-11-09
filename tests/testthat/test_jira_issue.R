context("Jira issue")
issue <- jira_issue(con, issue_raw_file =  "./test_jira_issues_raw.RData")

test_that("package test is working", {
  expect_equal(10, nrow(issue))
  expect_equal(13, ncol(issue))
  expect_equal("WOW-10", issue[1,]$key)
  expect_equal("13201", issue[1,]$id)
})

