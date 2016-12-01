# packages needed for graphs
# install.packages("ellipse")
library(ellipse)
library(ggplot2)

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

#' @title bootstrap.mfa
#' @description main function of bootstrap
#' @param X: a mfa object
#' @param L: number of bootstrap samples are computed
#' @export bootstrap.mfa
#' @return bootstrap estimates and bootstrap ratio
#' @examples \dontrun{
#' an example}

bootstrap.mfa <- function(X, L = 10){
#############check input values####################
  # check input dataset
  if(class(X) != "mfa") {
    stop("Wrong class of data!\nClass should be 'mfa'.")
  }
  # check input number of samples L
    if (L <= 0 || L %% 1 != 0) {
      stop("Wrong sample size L!\nL should be positive integers.")
    }
##############main function#########################
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

#' @title plot.boostrap
#' @description plot figure 7 & 8 in paper, use AFTER running bootsrap.mfa()
#' @param X: a mfa object
#' @param L: number of bootstrap samples are computed
#' @return figure 7 & 8 in paper
#' @examples \dontrun{
#' an example}

plot.boostrap <- function(X, L) {
#############check input values####################
  # check input dataset
  if(class(X) != "mfa") {
    stop("Wrong class of data!\nClass should be 'mfa'.")
  }
  # check input number of samples L
  if (L <= 0 || L %% 1 != 0) {
    stop("Wrong sample size L!\nL should be positive integers.")
  }
############# plot bar plot ###################
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
############plot confidence circle################
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
