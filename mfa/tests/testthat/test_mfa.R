
data_raw<-read.csv("../../data/wines.csv")
data<-data_raw[,2:54]

context("mfa arguments")

test_that("mfa with wrong sets",{
  expect_error(mfa(data,sets = TRUE))
})
