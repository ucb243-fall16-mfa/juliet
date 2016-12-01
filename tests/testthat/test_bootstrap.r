#Read the data from file(You may change the path)
data_raw<-read.csv("../../data/wines.csv")

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
# test bootstrap input
set.seed(1)
test_that('functionality', bootstrap.mfa(review, 10))
bootstrap_ratio = bootstrap.mfa(review, 10)
ref_ratio = -23.80556
test_that('bootstrap ratio values',
          {all.equal(bootstrap_ratio$FSB[1,1], ref_ratio)})
# test bootstrap.mfa's wrong inputs
test_that("bootstrap.mfa with wrong dataset X inputs",{
  expect_error(bootstrap.mfa(1:12, 10))
})
test_that("bootstrap.mfa with wrong L inputs",{
  expect_error(bootstrap.mfa(review, -2))
})

# test plot.bootstrap's wrong inputs
test_that("plot.bootstrap with wrong dataset X inputs",{
  expect_error(plot.bootstrap(1:12, 10))
})
test_that("plot.bootstrap with wrong L inputs",{
  expect_error(plot.bootstrap(review, -2))
})
