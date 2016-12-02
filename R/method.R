#' @title summary_eigen
#' @description summary_eigen is the function of MFA package.
#'              The function aims to analyze the property of eigenvalues.
#' @param x  a mfa object
#' @export
#' @return The function will reutrn a data frame including 5 componenets.
#' @return 1.singular value:the square root of eigenvalue.
#' @return 2.eigenvalue: one of the properties contained in the mfa project.
#' @return 3.cumulative1: the cumlulative of the eigenvalues in decreasing series.
#' @return 4.Inertia: the proportion of each eigenvalue to the sum of all eigenvalues.
#' @return 5.cumulative2: the cumlulative of the intertia in decreasing series.


#Make a summary of the eigenvalue of the mfa
summary_eigen<-function(x){
  if(class(x)!="mfa"){
    stop("\n'object' need to be mfa")
  }
  eigens<-x$eigen
  singular<-sqrt(eigens)
  cumulative<-cumsum(eigens)
  interia<-eigens/cumulative[length(cumulative)]
  i_cunmulative<-cumsum(interia)
  result<-data.frame("singular_values"=singular,"eigenvalue"=eigens,
                     "cumulative1"=cumulative,"interia"=interia,
                     "cumulative2"=i_cunmulative)
  result
}

#' @title contribution
#' @description contribution is the function of MFA package.
#'              The function aims to analyze the contributions of important elements to dimensions.
#' @param x  a mfa object
#' @param type  a character-"observation","variable" or "table"
#' @return The function will reutrn a data frame.
#' @return Each componets of the data frame represeting the proportion of each element's
#'         contribution to the certain dimension

#Three types-observation,variable and table
contribution<-function(x,type="table"){
  if(class(x)!="mfa"){
    stop("\n'object' need to be mfa")
  }
  if(type=="observation"){
    summation=colSums(diag(x$mass) %*% x$common_factor_scores^2)
    percent=apply(diag(x$mass) %*% x$common_factor_scores^2,1,function(y) y/summation)
    percent=t(percent)
    colnames(percent)<-paste0(rep("dim",length(percent[1,])),seq(length(percent[1,])))
    rownames(percent)<-paste0(rep("ob",length(percent[,1])),seq(length(percent[,1])))
    round(percent,3)
  }
  else if(type=="variable"){
    percent=diag(rep(x$weight,diff(x$divide))) %*% x$loadings^2
    colnames(percent)<-paste0(rep("dim",length(percent[1,])),seq(length(percent[1,])))
    rownames(percent)<-paste0(rep("vr",length(percent[,1])),seq(length(percent[,1])))
    percent
  }
  else if(type=="table"){
    tmp=diag(rep(x$weight,diff(x$divide))) %*% x$loadings^2
    percent=matrix(ncol=dim(x$loadings)[2])
    for(num in seq(length(x$divide)-1)){
      table<-tmp[x$divide[num]:(x$divide[num+1]-1),]
      table<-colSums(table)
      if(any(is.na(percent))){
        percent<-table
      }
      else{
        percent<-rbind(percent,table)
      }
    }
    colnames(percent)<-paste0(rep("dim",length(percent[1,])),seq(length(percent[1,])))
    rownames(percent)<-paste0(rep("tb",length(percent[,1])),seq(length(percent[,1])))
    round(percent,3)
  }
  else{
    stop("\nThe type is Error")
  }
}
#To find the sum of the diagonal entried of the matrix
tr<-function(matrix){
  if(dim(matrix)[1]!=dim(matrix)[2]){
    stop("\nThe matrix need to be a squared matrix")
  }
  sum(diag(matrix))
}

#' @title RV
#' @description RV is the function of MFA package.
#'              The function aims to analyze the similarity between different table
#' @param table1  a matrix
#' @param table2  a matrix which has the same number of rows as table1
#' @return The function will reutrn a number which is a coefficient to evaluate the
#'         similarity between two table.

RV<-function(table1,table2){
  if(!is.numeric(table1) | !is.numeric(table1)){
    stop("\nThe parameter need to be a numeric matrix or vector")
  }
  numerator=tr((table1 %*% t(table1)) %*% (table2 %*% t(table2)))
  denominator=sqrt(tr(table1 %*% t(table1) %*% table1 %*% t(table1))*
                     tr(table2 %*% t(table2) %*% table2 %*% t(table2)))
  numerator/denominator
}

#' @title RV_table
#' @description RV_table is the function of MFA package.
#'              The function aims to analyze the similarity between different table in a dataset
#' @param dataset  a data frame
#' @param sets  a list of vectors.Each vector represets a table
#' @return The function will reutrn a data frame.
#' @return The components of the data frame are RV coefficients between each pair of tables.

RV_table<-function(dataset,sets){
  if(!is.data.frame(dataset)){
    stop("\nThe parameter dataset need to be a data frame")
  }
  if(!is.list(sets)){
    stop("\nThe parameter ests need to be a list")
  }
  N<-length(sets)
  result<-matrix(ncol=N,nrow=N)
  store<-list()
  for(x in seq(N)){
    store[[x]]=as.matrix(data[,sets[[x]]])
  }
  for(i in seq(N)){
    for(j in seq(N)){
      result[i,j]<-RV(store[[i]],store[[j]])
    }
  }
  colnames(result)<-paste0(rep("tb",length(N)),seq(N))
  rownames(result)<-paste0(rep("tb",length(N)),seq(N))
  result
}

#' @title LG
#' @description LG is the function of MFA package.
#'              The function aims to analyze the similarity between different table
#' @param table1  a matrix
#' @param table2  a matrix which has the same number of rows as table1
#' @return The function will reutrn a number which is a coefficient to evaluate the
#'         similarity between two table.

#We give the original normailized data and the number of block to construct the table
LG<-function(table1,table2){
  if(!is.numeric(table1) | !is.numeric(table1)){
    stop("\nThe parameter need to be a numeric matrix or vector")
  }
  a1<-svd(table1)$d[1]
  a2<-svd(table2)$d[1]
  tr((table1 %*% t(table1)) %*% (table2 %*% t(table2)))*(1/a1^2)*(1/a2^2)
}

