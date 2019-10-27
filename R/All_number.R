#' Transforms all non numeric variables of a data.frame into numeric variables by geting dummies features of each variable category
#'
#' @param df data.frame object
#'
#' @return data.frame object with new variables
#' @export
#'
#'
#' @importFrom dplyr as_tibble
#' @importFrom DataMinigTools categorizar
#'
#' @examples
#'
#' data(iris)
#' Iris<-all_number(iris)
#' Iris
#'
all_number <- function(df,...) {
    df <- dplyr::as_tibble(df)
    classes <- sapply(df, class)
    nnm <- !classes %in% "numeric"
    no_num <- df[, names(df)[nnm]]
    no_mun_2 <- categorizar(no_num, ALL = F,...)
    df_n <- cbind(df[, !nnm], no_mun_2)
    df_n
}
