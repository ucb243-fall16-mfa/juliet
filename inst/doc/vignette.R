## ------------------------------------------------------------------------
source("../R/mfa.r")
source('../R/plot.r')
source('../R/method.r')
source('../R/print.r')
source('../R/bootstrap.r')

## ------------------------------------------------------------------------
data_raw<-read.csv("../data/wines.csv")
#Pick the useful part
data<-data_raw[,2:54]
#Make a list store the block division
pick1<-list(seq(1,6),seq(7,12),seq(13,18),seq(19,23),seq(24,29),
            seq(30,34),seq(35,38),seq(39,44),seq(45,49),seq(50,53))
col_name<-names(data)
pick2<-list(col_name[1:6],col_name[7:12],col_name[13:18],col_name[19:23],col_name[24:29],
            col_name[30:34],col_name[35:38],col_name[39:44],col_name[45:49],col_name[50:53])

## ------------------------------------------------------------------------
review<-mfa(data,pick1)

## ------------------------------------------------------------------------
print.mfa(review)

## ------------------------------------------------------------------------
#plot.mfa(review, dim_plot=c(1,2), cex=0.8)

## ------------------------------------------------------------------------
test1<-summary_eigen(review)

## ------------------------------------------------------------------------
#test2<-contribution(review,type="observation")
#test3<-contribution(review,type="variable")
#test4<-contribution(review,type="table")

## ------------------------------------------------------------------------
num_block<-length(review$weight)
test5<-RV(review$scale_x[,seq(review$divide[1],review$divide[2]-1)],
          review$scale_x[,seq(review$divide[num_block-1],review$divide[num_block]-1)])

## ------------------------------------------------------------------------
test6<-LG(review$scale_x[,seq(review$divide[1],review$divide[2]-1)],
          review$scale_x[,seq(review$divide[num_block-1],review$divide[num_block]-1)])

## ------------------------------------------------------------------------
bootstrap_ratio = bootstrap.mfa(review, 10)

