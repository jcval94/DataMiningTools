#' Applies all funtions in a list in order to an R's object
#'
#' @param X object
#' @param lst list of functions
#'
#' @return objeto after have applied the respective functions
#' @export
#'
#' @examples
#' #Is the sum of X greater than 3
#' lst<-list(sum,function(x) x > 3)
#' X<-c(4,2,3,2,6)
#'
#' nested(X,lst)
#'
#' #How many numbers in X are greater than 3
#' lst<-list(function(x) x > 3,sum)
#' X<-c(4,2,3,2,6)
#'
#' nested(X,lst)
#'
nested<-function(X,lst){
  if(!all(sapply(lst,is.function))){
    warning("lst must contain functions as elements");return(invisible())
  }
  for(fun in lst){
    X<-fun(X)
  }
  return(X)
}
