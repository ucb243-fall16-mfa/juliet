plot.mfa <- function(x, ...) {
################## eigenvalues #####################
  pdf("eigen value.pdf")
  barplot(x$eigen, main = 'Eigenvalues', xlab = 'Components', ylab = "Eigenvalue of Component")
  dev.off()
############### common factor scores ####################
  size_subsample <- dim(x$common_factor_scores)[1] / 3
  for (i in 1: dim(x$common_factor_scores)[1]) {
    if (i <= size_subsample) factor_text = c(factor_text, paste('NZ',i))
    if (i > size_subsample && i <= 2 * size_subsample) factor_text = c(factor_text, paste('FR',i - 4))
    if (i > 2 * size_subsample) factor_text = c(factor_text, paste('CA',i - 8))
  }
  pdf("Figure 2a.pdf")
  plot(x$common_factor_scores[,1], x$common_factor_scores[,2], main = "Common Factor Scores",
       xlab = "1", ylab = "2", bty = "l", type = "n", axes=FALSE)
  text(x$common_factor_scores[,1], x$common_factor_scores[,2], labels = factor_text)
  abline(h=0); abline(v=0)
  dev.off()
################## PROCESS PARTIAL SCORES #####################
# exact partial scores for each assessor
  partial_factor_scores <- attributes(x$partial_factor_scores)
  partial_factor_scores[[1]] <- NULL

################## PROCESS LOADINGS #####################
# re-scale loadings to have a variance of 1st 2 eigen values
  loadings <- cbind(x$loadings[,1] / sd(x$loadings[,1]) * sqrt(sqrt(x$eigen[1])), 
                    x$loadings[,2] / sd(x$loadings[,2]) * sqrt(sqrt(x$eigen[2])))
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
  pdf("Figure3.pdf", width=20, height=10)
  # plot 10 graphs in one doc.
  par(mfrow=c(2,5))
  # plot partial factor scores
  for (i in 1: length(partial_factor_scores)) {    # index = order of assessor + 1 
    plot(partial_factor_scores[[i]][,1], partial_factor_scores[[i]][,2],  
         xlab = "", ylab = "", bty = "l", type = "n", asp = 2, axes = FALSE)
    # x axis
    axis(side = 1, pos = c(0,0), tick = TRUE, labels = FALSE, lty = 1)
   # y axis
   axis(side = 2, pos = c(0,0), tick = TRUE, labels = FALSE, lty = 1)
    # plot partial scores
    text(partial_factor_scores[[i]][,1], partial_factor_scores[[i]][,2], labels = factor_text, cex = 0.8,
          col = "blue")
    # plot loadings
    text(loadings_list[[i]][,1], loadings_list[[i]][,2], labels = loadings_text[[i]], col = "orange")
   
   mtext(side=1,text=paste0("Assessor", i), cex = 0.8)
  }
  dev.off()
}
##############################################

