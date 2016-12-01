#' @title print.mfa
#' @description print 4 tables containing eigen values, common factor scores, partial facotr scores, and loadings.
#' @param x: data set(class should be mfa)
#' @param n_assessor: integer between 1 to 10 indicating the number of the chosen assessor.
#' @export
#' @return 4 tables
#' @examples \dontrun{
#' print.mfa(review, n_assessor = 1)}

print.mfa <- function(x, n_assessor = 1, ...) {
  # check input dataset class
  if(class(x) != "mfa") {
    stop("Wrong class of data!\nClass should be 'mfa'.")
  }
  # check input n_accessor
  if (n_assessor %% 1 != 0 || n_assessor < 1 || n_assessor > 12) {
    stop("n_assessor should be an integer between 1 to 12")
  }
  # 1) print table1 of eigens
  eigen_values = x$eigen
  singular_values = sqrt(eigen_values)
  table1 = round(as.data.frame(rbind(singular_values, eigen_values)), 3)
  cat("Table 1. Head of Singular Values and Eigen Values\n")
  print(table1)
  # 2) print table2 of common factor scores
  table2 = round(cbind(x$common_factor_scores[,1], x$common_factor_scores[,2]),3)
  cat("\nTable 2. First Two Dimensions of Common Factor Scores\n")
  print(table2)
  # 3) print table3 of partial factor scores
  # extract partial factor scores
  partial_factor_scores <- attributes(x$partial_factor_scores)
  partial_factor_scores[[1]] <- NULL
  # get the partial factor scores for Assessor n_assessor
  table3 = round(cbind(partial_factor_scores[[n_assessor]][,1],
                       partial_factor_scores[[n_assessor]][,2]),3)
  cat("\nTable 3. Partial Factor Scores of Assessor", n_assessor, "\n")
  print(table3)
}

