#' Replace values in a numeric vector
#'
#' @param X numeric vector
#' @param values values to replace
#' @param limits numeric vector, contain the upper limits of each interval to return values vector
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
#' X <- c(1.5, 3, 6, 14)
#' replacemet(X, values, limits)
#'
#'
replacemet <- function(X, values, limits) {
  a <- c()
  for (i in 1:(length(limits) + 1)) {
    a <- ifelse(X < limits[i + 1] & X >= limits[i], values[i], NaN)
    ifelse(is.na(a), a <- ifelse(X < limits[i + 1] & X >= limits[i], values[i], NaN), (break)())
  }
  return(a)
}
