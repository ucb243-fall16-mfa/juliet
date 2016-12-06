#Read the data from file(You may change the path)
data_raw<-read.csv("../../../data/wines.csv")

#Pick the useful part
data<-data_raw[,2:54]

#Make a list store the block division
pick1<-list(seq(1,6),seq(7,12),seq(13,18),seq(19,23),seq(24,29),
            seq(30,34),seq(35,38),seq(39,44),seq(45,49),seq(50,53))

col_name<-names(data)
pick2<-list(col_name[1:6],col_name[7:12],col_name[13:18],col_name[19:23],col_name[24:29],
            col_name[30:34],col_name[35:38],col_name[39:44],col_name[45:49],col_name[50:53])
#Construct the object
review<-mfa(data,pick1)

# test
test_that('functionality', print.mfa(review, n_assessor = 1))
test_that("plot.r with wrong n_assessor inputs",{
  expect_error(print.mfa(review, n_assessor = -1))
})
test_that("plot.r with wrong dataset x inputs",{
  expect_error(print.mfa(1:12, n_assessor = -1))
})

test_that("summary_eigen with wrong mfa x inputs",{
  expect_error(summary_eigen(1))
})
test_that("contribution with wrong mfa x inputs",{
  expect_error(contribution(1))
})
test_that("contribution with wrong type inputs",{
  expect_error(contribution(review,"wrong_type"))
})
test_that("tr with wrong matrix inputs",{
  expect_error(tr(matrix(c(1,2,3,4,5,6),nrow=2)))
})
test_that("RV wiht wrong matrix inputs",{
  expect_error(RV(matrix(c(1,2,3,4,5,6),nrow=2),matrix(c("1","2","3","4","5","6"),nrow=2)))
})
test_that("RV_table with wrong data frame dataset inputs",{
  expect_error(RV_table(c(1,2,3),sets=NULL))
})
test_that("RV_table with wrong list sets inputs",{
  expect_error(RV_table(review,sets=c(1,2,3)))
})
test_that("LG wiht wrong matrix inputs",{
  expect_error(LG(matrix(c(1,2,3,4,5,6),nrow=2),matrix(c("1","2","3","4","5","6"),nrow=2)))
})
