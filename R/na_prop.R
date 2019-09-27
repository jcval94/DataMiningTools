#' Shows the proportion of NA's in a vector
#'
#' @param X a numeric vector
#'
#' @return proportion of NA's in a vector
#' @export
#'
#' @examples
#'
#' na_prop(c(NA, rnorm(100)))
#'
na_prop <- function(X) {
  if(!is.numeric(X)){
    warning("X must contain numeric values");return(invisible)
  }
    sum(ifelse(is.na(X), 1, 0))/length(X)
}

