plot.mfa <- function(x, dim_plot = c(1,2), cex = 0.8, ...) {
######################check input######################
  # check input dataset
  if(class(x) != "mfa") {
    stop("Wrong class of data!\nClass should be 'mfa'.")
  }
  # check input dimensions
  for (i in 1:2) {
    if (dim_plot[i] < 0 || dim_plot[i] > 11 || dim_plot[i] %% 1 != 0) {
      stop("Wrong dimesion values!\nDimensions should be integers between 1 to 11.")
    }
  }
  # check size of points
  if (cex <= 0) stop("Wrong cex values!\nCex should be positive.")
    
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

################## PLOT #####################
###############PLOT common factor scores ####################
  plot(x$common_factor_scores[,1], x$common_factor_scores[,2], main = "Common Factor Scores",
       xlab = "1", ylab = "2", bty = "l", type = "n", las = 1,
       xlim = c(min(x$common_factor_scores[,1])-0.5, max(x$common_factor_scores[,1])+0.5),
       ylim = c(min(x$common_factor_scores[,2]), max(x$common_factor_scores[,2])))
  text(x$common_factor_scores[,1], x$common_factor_scores[,2], labels = factor_text, cex = cex, col = "blue")
  abline(h=0, v=0)
  
##############PLOT partial factor scores ONLY #####################
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
##############################################
##############PLOT loadings ONLY#####################
  # plot 10 graphs in one doc.
  multi_graph <- par(mfrow=c(2,5))
  # plot partial factor scores
  for (i in 1: length(partial_factor_scores)) {    # index = order of assessor + 1 
    plot(partial_factor_scores[[i]][,dim_plot[1]], partial_factor_scores[[i]][,dim_plot[2]], main = "Partial Factor Scores",
         xlab = paste0("dim", dim_plot[1]), ylab = paste0("dim", dim_plot[2]), bty = "l", type = "n", las = 1,
         xlim = c(min(partial_factor_scores[[i]][,dim_plot[1]])-0.5, max(partial_factor_scores[[i]][,dim_plot[1]])+0.5),
         ylim = c(min(partial_factor_scores[[i]][,dim_plot[2]]), max(partial_factor_scores[[i]][,dim_plot[2]])))
    # plot loadings
    text(loadings_list[[i]][,dim_plot[1]], loadings_list[[i]][,dim_plot[2]], labels = loadings_text[[i]], col = "orange")
    mtext(side=1,text=paste0("Assessor", i), cex = cex)
    abline(h = 0, v = 0)
  }
  par(multi_graph)
##############PLOT partial factor scores && Loading TOGETHER#####################
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


