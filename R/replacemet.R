#' Replace values in a numeric vector
#'
#' @param X numeric vector
#' @param values values to replace
#' @param limits numeric vector, contain the upper limits of each interval to return values vector
#' @param include_all FALSE, if TRUE, changes the low limit to -Inf and the upper value to Inf
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' values <- c(4, 8)
#' limits <- c(1, 5, 10)
#'
#' replacemet(1, values, limits)
#'
#' replacemet(5, values, limits)
#'
#' replacemet(10, values, limits)
#'
#'
#' X <- c(0, 3, 6, 14)
#' replacemet(X, values, limits)
#'
#'
replacemet <- function(X, values, limits, include_all=FALSE) {
  lv<-length(values)
  ll<-length(limits)
  lx<-length(X)
  if(lv!=(ll-1)){
    warning("limits must have the same length that values + 1")
    return(invisible())
  }
  if(include_all){
    limits[1]<- -Inf
    limits[ll]<- Inf
  }
  a <- rep(NA,lx)
  for (i in 1:(ll - 1)) {
    cond<-X < limits[i + 1] & X >= limits[i]
    a[cond] <- values[i]
    if(!any(is.na(a))){
      break()
    }
  }
  return(a)
}
