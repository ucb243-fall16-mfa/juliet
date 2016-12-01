##load libraries
library(shiny)
library(ggplot2)
library(ellipse)

##Read in data
dir<-"C:/Users/yangq_000/Google Drive/Berkeley/Courses/STATS 243 Statistical Computing/Final Project"
source(paste(dir,"Work/mfa.R",sep="/"))

#Read the data from file(You may change the path)
data_raw<-read.csv(paste(dir,"/Data/wines.csv",sep="/"))

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


################ PLOT eigenvalues bar chart ##################
plot_eigen<-function(x){
  data<-data.frame("Component"=1:11,"Eigenvalue"=x$eigen)
  
  x<-review
  ggplot(data,aes(x=Component,y=Eigenvalue))+geom_bar(stat="identity",color="black",fill="blue")+labs(title
                ="Eigenvalue Bar-chart")
}

#plot_eigen(review)

###############PLOT common factor scores ####################
plot_common_factor <- function(x, dim_plot = c(1,2), cex = 0.8, ...) {

  # check plotted dimensions
  for (i in 1:2) {
    if (dim_plot[i] < 0 || dim_plot[i] > 11) {
      warning("wrong dimesions!\nSet dimesions as default: 1 & 2")
    }
  }
  ##################set labels for factor plots###############################
  factor_text <- NULL
  size_subsample <- dim(x$common_factor_scores)[1] / 3
  for (i in 1: dim(x$common_factor_scores)[1]) {
    if (i <= size_subsample) factor_text = c(factor_text, paste('NZ',i))
    if (i > size_subsample && i <= 2 * size_subsample) factor_text = c(factor_text, paste('FR',i - 4))
    if (i > 2 * size_subsample) factor_text = c(factor_text, paste('CA',i - 8))
  }
  
  ################## PROCESS PARTIAL SCORES #####################
  # extract partial scores for each assessor
  partial_factor_scores <- attributes(x$partial_factor_scores)
  partial_factor_scores[[1]] <- NULL
  
  ################## PROCESS LOADINGS #####################
  # re-scale loadings to have a variance of 1st 2 eigen values
  # re-scale to 1
  Loadings_std <- sweep(x$loadings, 2, apply(x$loadings, 2, sd), "/")
  # re-scale to eigen-value
  loadings <- sweep(Loadings_std, 2, sqrt(x$eigen), "*")
  # seperate different assessors
  loadings_list <- list(
    "1" = loadings[1:6,],
    "2" = loadings[7:12,],
    "3" = loadings[13:18,],
    "4" = loadings[19:23,],
    "5" = loadings[24:29,],
    "6" = loadings[30:34,],
    "7" = loadings[35:38,],
    "8" = loadings[39:44,],
    "9" = loadings[45:49,],
    "10" = loadings[50:53,]
  )
  # name loadings
  loadings_text <- list(
    "1"<- c("V1", "V2", "V3", "V4", "V5", "V6"),
    "2"<- c("V1", "V2", "V3", "V4", "V7", "V8"),
    "3"<- c("V1", "V2", "V3", "V4", "V9", "V10"),
    "4"<- c("V1", "V2", "V3", "V4", "V8"),
    "5"<- c("V1", "V2", "V3", "V4", "V11", "V12"),
    "6"<- c("V1", "V2", "V3", "V4", "V13"),
    "7"<- c("V1", "V2", "V3", "V4"),
    "8"<- c("V1", "V2", "V3", "V4", "V14", "V15"),
    "9"<- c("V1", "V2", "V3", "V4", "V15"),
    "10"<- c("V1", "V2", "V3", "V4")
  )

  

  plot(x$common_factor_scores[,1], x$common_factor_scores[,2], main = "Common Factor Scores",
       xlab = "1", ylab = "2", bty = "l", type = "n", las = 1,
       xlim = c(min(x$common_factor_scores[,1])-0.5, max(x$common_factor_scores[,1])+0.5),
       ylim = c(min(x$common_factor_scores[,2]), max(x$common_factor_scores[,2])))
  text(x$common_factor_scores[,1], x$common_factor_scores[,2], labels = factor_text, cex = cex, col = "blue")
  abline(h=0, v=0)
  
  
#plot_common_factor(review)

}
  
##############PLOT partial factor scores ONLY #####################

plot_partial_factor <- function(x, dim_plot = c(1,2), cex = 0.8, ...) {
  
  # check plotted dimensions
  for (i in 1:2) {
    if (dim_plot[i] < 0 || dim_plot[i] > 11) {
      warning("wrong dimesions!\nSet dimesions as default: 1 & 2")
    }
  }
  ##################set labels for factor plots###############################
  factor_text <- NULL
  size_subsample <- dim(x$common_factor_scores)[1] / 3
  for (i in 1: dim(x$common_factor_scores)[1]) {
    if (i <= size_subsample) factor_text = c(factor_text, paste('NZ',i))
    if (i > size_subsample && i <= 2 * size_subsample) factor_text = c(factor_text, paste('FR',i - 4))
    if (i > 2 * size_subsample) factor_text = c(factor_text, paste('CA',i - 8))
  }
  
  ################## PROCESS PARTIAL SCORES #####################
  # extract partial scores for each assessor
  partial_factor_scores <- attributes(x$partial_factor_scores)
  partial_factor_scores[[1]] <- NULL
  
  ################## PROCESS LOADINGS #####################
  # re-scale loadings to have a variance of 1st 2 eigen values
  # re-scale to 1
  Loadings_std <- sweep(x$loadings, 2, apply(x$loadings, 2, sd), "/")
  # re-scale to eigen-value
  loadings <- sweep(Loadings_std, 2, sqrt(x$eigen), "*")
  # seperate different assessors
  loadings_list <- list(
    "1" = loadings[1:6,],
    "2" = loadings[7:12,],
    "3" = loadings[13:18,],
    "4" = loadings[19:23,],
    "5" = loadings[24:29,],
    "6" = loadings[30:34,],
    "7" = loadings[35:38,],
    "8" = loadings[39:44,],
    "9" = loadings[45:49,],
    "10" = loadings[50:53,]
  )
  # name loadings
  loadings_text <- list(
    "1"<- c("V1", "V2", "V3", "V4", "V5", "V6"),
    "2"<- c("V1", "V2", "V3", "V4", "V7", "V8"),
    "3"<- c("V1", "V2", "V3", "V4", "V9", "V10"),
    "4"<- c("V1", "V2", "V3", "V4", "V8"),
    "5"<- c("V1", "V2", "V3", "V4", "V11", "V12"),
    "6"<- c("V1", "V2", "V3", "V4", "V13"),
    "7"<- c("V1", "V2", "V3", "V4"),
    "8"<- c("V1", "V2", "V3", "V4", "V14", "V15"),
    "9"<- c("V1", "V2", "V3", "V4", "V15"),
    "10"<- c("V1", "V2", "V3", "V4")
  )
  
  # plot 10 graphs in one doc.
  multi_graph <- par(mfrow=c(2,5))
  # plot partial factor scores
  for (i in 1: length(partial_factor_scores)) {    # index = order of assessor + 1 
    plot(partial_factor_scores[[i]][,dim_plot[1]], partial_factor_scores[[i]][,dim_plot[2]], main = "Partial Factor Scores",
         xlab = paste0("dim", dim_plot[1]), ylab = paste0("dim", dim_plot[2]), bty = "l", type = "n", las = 1,
         xlim = c(min(partial_factor_scores[[i]][,dim_plot[1]])-0.5, max(partial_factor_scores[[i]][,dim_plot[1]])+0.5),
         ylim = c(min(partial_factor_scores[[i]][,dim_plot[2]]), max(partial_factor_scores[[i]][,dim_plot[2]])))
    # plot partial factor scores
    text(partial_factor_scores[[i]][,dim_plot[1]], partial_factor_scores[[i]][,dim_plot[2]], cex = cex, 
         labels = factor_text, col = "blue")
    mtext(side=1,text=paste0("Assessor", i), cex = cex)
    abline(h = 0, v = 0)
  }
  par(multi_graph)
}
  
#plot_partial_factor(review,dim_plot=c(4,5))

##############PLOT partial factor scores && Loading TOGETHER#####################
plot_partial_factor_loading <- function(x, dim_plot = c(1,2), cex = 0.8, ...) {
  
  # check plotted dimensions
  for (i in 1:2) {
    if (dim_plot[i] < 0 || dim_plot[i] > 11) {
      warning("wrong dimesions!\nSet dimesions as default: 1 & 2")
    }
  }
  ##################set labels for factor plots###############################
  factor_text <- NULL
  size_subsample <- dim(x$common_factor_scores)[1] / 3
  for (i in 1: dim(x$common_factor_scores)[1]) {
    if (i <= size_subsample) factor_text = c(factor_text, paste('NZ',i))
    if (i > size_subsample && i <= 2 * size_subsample) factor_text = c(factor_text, paste('FR',i - 4))
    if (i > 2 * size_subsample) factor_text = c(factor_text, paste('CA',i - 8))
  }
  
  ################## PROCESS PARTIAL SCORES #####################
  # extract partial scores for each assessor
  partial_factor_scores <- attributes(x$partial_factor_scores)
  partial_factor_scores[[1]] <- NULL
  
  ################## PROCESS LOADINGS #####################
  # re-scale loadings to have a variance of 1st 2 eigen values
  # re-scale to 1
  Loadings_std <- sweep(x$loadings, 2, apply(x$loadings, 2, sd), "/")
  # re-scale to eigen-value
  loadings <- sweep(Loadings_std, 2, sqrt(x$eigen), "*")
  # seperate different assessors
  loadings_list <- list(
    "1" = loadings[1:6,],
    "2" = loadings[7:12,],
    "3" = loadings[13:18,],
    "4" = loadings[19:23,],
    "5" = loadings[24:29,],
    "6" = loadings[30:34,],
    "7" = loadings[35:38,],
    "8" = loadings[39:44,],
    "9" = loadings[45:49,],
    "10" = loadings[50:53,]
  )
  # name loadings
  loadings_text <- list(
    "1"<- c("V1", "V2", "V3", "V4", "V5", "V6"),
    "2"<- c("V1", "V2", "V3", "V4", "V7", "V8"),
    "3"<- c("V1", "V2", "V3", "V4", "V9", "V10"),
    "4"<- c("V1", "V2", "V3", "V4", "V8"),
    "5"<- c("V1", "V2", "V3", "V4", "V11", "V12"),
    "6"<- c("V1", "V2", "V3", "V4", "V13"),
    "7"<- c("V1", "V2", "V3", "V4"),
    "8"<- c("V1", "V2", "V3", "V4", "V14", "V15"),
    "9"<- c("V1", "V2", "V3", "V4", "V15"),
    "10"<- c("V1", "V2", "V3", "V4")
  )
  
  # plot 10 graphs in one doc.
  multi_graph <- par(mfrow=c(2,5))
  # plot partial factor scores
  for (i in 1: length(partial_factor_scores)) {    # index = order of assessor + 1 
    plot(partial_factor_scores[[i]][,dim_plot[1]], partial_factor_scores[[i]][,dim_plot[2]], main = "Partial Factor Scores",
         xlab = paste0("dim", dim_plot[1]), ylab = paste0("dim", dim_plot[2]), bty = "l", type = "n", las = 1,
         xlim = c(min(partial_factor_scores[[i]][,dim_plot[1]])-0.5, max(partial_factor_scores[[i]][,dim_plot[1]])+0.5),
         ylim = c(min(partial_factor_scores[[i]][,dim_plot[2]]), max(partial_factor_scores[[i]][,dim_plot[2]])))
    # plot partial factor scores
    text(partial_factor_scores[[i]][,dim_plot[1]], partial_factor_scores[[i]][,dim_plot[2]], cex = cex, 
         labels = factor_text, col = "blue")
    # plot loadings
    text(loadings_list[[i]][,dim_plot[1]], loadings_list[[i]][,dim_plot[2]], labels = loadings_text[[i]], col = "orange")
    mtext(side=1,text=paste0("Assessor", i), cex = cex)
    abline(h = 0, v = 0)
  }
  par(multi_graph)
}

#plot_partial_factor_loading(review)

################ PLOTS for bootstrap ###################

fsb = function(PFS){
  #compute bootstrap factor score from partial factore scores of the inital data
  #with a random sequence B
  #sample with replacement
  B=sample(1:10, 10, replace=TRUE)
  FS_B = matrix(data=0, nrow=12, ncol=ncol(PFS[[1]]))
  for (i in 1:length(B)){
    FS_B = FS_B + PFS[[B[i]]]
  }
  FS_B = 1/length(B)*FS_B
}


bootstrap.mfa <- function(X, L){
  #FS: factor scores of the initial data tablesi
  #compute the bootstrapped estimate of the factor scores
  PFS = attributes(X$partial_factor_scores)
  PFS[[1]]=NULL
  FSBs = replicate(L, fsb(PFS))
  #compute bootstrap ratios
  FSB.mean = apply(FSBs, 1:2, mean)
  FSB.sd = apply(FSBs, 1:2, sd)
  boostrap.ratio = FSB.mean/FSB.sd
  result <- list(
    FSBs = FSBs,
    FSB = boostrap.ratio
  )
  return(result)
}


######### Bootstrap ratio plot for Components 1 and 2 ##########
plot.boostrap_ratio <- function(X, L) {
  two_graph <- par(mfrow=c(1,2))
  FSB = bootstrap.mfa(X, L)$FSB
  FSBs = bootstrap.mfa(X, L)$FSBs
  # set laebls
  labels <- rev(c("NZ 1", "NZ 2", "NZ 3", "NZ 4",
                  "FR 1", "FR 2", "FR 3", "FR 4", 
                  "CA 1", "CA 2", "CA 3", "CA 4"))
  # set colors
  for (j in 1:2) {
    colors <- NULL
    for (i in 1:12) {
      if (FSB[i, j] > 3) colors[i] = "mediumpurple1"
      if (FSB[i, j] > -3 && FSB[i, j] < 3) colors[i] = "grey"
      if (FSB[i, j] < -3) colors[i] = "palegreen3"
    }
    # plot dim 1
    bp = barplot(FSB[,j], main = paste0("Dimension", j), horiz=TRUE, col = colors)
    text(x = FSB[,j], y = bp, labels = labels, cex = 0.7)
    axis(1, at = c(-3,3), labels = c(-3,3))
    abline(v = -3, col = "orange")
    abline(v = 3, col = "orange")
  }
  par(two_graph)
}


  ############ Bootstrap confidence ellipses plotted on Components 1 and 2 ################

plot.boostrap_conf <- function(X, L) {
  FSBs = bootstrap.mfa(X, L)$FSBs
  
  # set laebls
  labels <- rev(c("NZ 1", "NZ 2", "NZ 3", "NZ 4",
                  "FR 1", "FR 2", "FR 3", "FR 4", 
                  "CA 1", "CA 2", "CA 3", "CA 4"))
  
  
  # build a matrix of all boostrap results in NO.1 & NO.2 dims
  locations_m <- matrix(data = NA, nrow = 12*L, ncol = 3)
  for (k in 1:12) {
    for (i in ((k-1)*L + 1):(k*L)) {
      locations_m[i, ] <- c(FSBs[k, 1:2, i-(k-1)*L], k)
    }
  }
  
  colnames(locations_m) <- c("dim1","dim2", "wine_id")
  # convert matrix to data.frame
  locations <- as.data.frame(locations_m)
  # set plot parameters
  centroids <- aggregate(cbind(locations$dim1,locations$dim2)~locations$wine_id,locations,mean)
  conf.rgn  <- do.call(rbind,lapply(unique(locations$wine_id),function(t) 
    data.frame(wine_id=as.character(t),
               ellipse(cov(locations[locations$wine_id==t,1:2]),
                       centre=as.matrix(centroids[t,2:3]),
                       level=0.95),
               stringsAsFactors=FALSE)))
  # plot
  ggplot(data=locations,(aes(x=dim1,y=dim2, colour = wine_id)))+
    geom_path(data=conf.rgn)+
    theme_bw()+
    guides(fill=guide_legend(title=NULL)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_colour_discrete(name  ="Wine",
                          breaks=1:12,
                          labels=rev(labels))
  
}

#plot.boostrap_conf(review,10)











