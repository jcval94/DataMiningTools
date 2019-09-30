#' determines if a number is between two nombers given
#'
#' @param e1 numeric vector
#' @param e2 numeric range
#'
#' @return TRUE or FALSE vector
#' @export
#'
#' @examples
#'
#' 100 %c% c(93,124)
#'
#' !runif(100) %c% c(0,1)
#'
#'
#'
`%c%` <- function(e1, e2){
  as.numeric(e1)>=as.numeric(e2[1]) & as.numeric(e1)<=as.numeric(e2[2])
}
