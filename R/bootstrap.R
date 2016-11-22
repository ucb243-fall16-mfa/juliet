#' @title bootstrap.mfa
#' @description a brief description
#' @param X: a mfa object
#' @param L: number of bootstrap samples are computed
#' @export
#' @return bootstrap ratio
#' @examples \dontrun{
#' an example}


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
}


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
FSB = bootstrap.mfa(review, 10)
FSB
