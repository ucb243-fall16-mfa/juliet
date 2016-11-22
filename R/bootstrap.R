#' @title bootstrap.mfa
#' @description a brief description
#' @param X: a mfa object
#' @export
#' @return result:...
#' @examples \dontrun{
#' an example}


bootstrap.mfa <- function(X){
#FS: factor scores of the initial data tablesi
K=10
#sample with replacement
B=sample(1:10, 10, replace=TRUE)
#compute the bootstrapped estimate of the factor scores
PFS = attributes(X$partial_factor_scores)
PFS[[1]]=NULL
sum = matrix(data=0, nrow=12, ncol=2)
print(sum)
print(PFS[[1]])
FS_B = 1/10*(lapply(B, FUN = function(i) sum+PFS[[i]]))
}
