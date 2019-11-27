#' Replace values in a numeric vector
#'
#' @param X
#' @param valores
#' @param cortes
#'
#' @return
#' @export
#'
#' @examples
#'
#' proporciones <- c(4, 2)
#' valores <- c(4, 8)
#' cortes <- c(1, 5, 10)
#'
#' replacemet(1, valores, cortes)
#'
#' replacemet(5, valores, cortes)
#'
#' replacemet(10, valores, cortes)
#'
#' replacemet(.1, valores, cortes)
#'
#'
#'
replacemet <- function(X, valores, cortes) {
  a <- c()
  for (i in 1:(length(cortes) + 1)) {
    a <- ifelse(X < cortes[i + 1] & X >= cortes[i], valores[i], NaN)
    ifelse(is.na(a), a <- ifelse(X < cortes[i + 1] & X >= cortes[i], valores[i], NaN), (break)())
  }
  return(a)
}
