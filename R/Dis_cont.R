#' Split numeric and non numeric columns of a data.frame
#'
#' @param df data.frame object to be filtered
#'
#' @return a list of data frame objects with numeric variables and non nmeric variables
#' @export
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#'
#' @examples
#'
#' data(iris)
#'
#' Dis_cont(iris)
#'
Dis_cont <- function(df) {
  df<-tibble::as_tibble(df)
  vrbls <- purrr::map_chr(df, ~class(.x))
  num_var <- vrbls != "numeric"
  return(list(df[, num_var], df[, !num_var]))
}
