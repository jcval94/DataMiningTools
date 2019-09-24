#' Transform all non numeric variables of a data.frame into numeric variables by geting dummies features of each variable category
#'
#' @param df data.frame object
#'
#' @return data.frame object with new variables
#' @export
#'
#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#'
#' @examples
#'
#'data(iris)
#'Iris<-all_number(iris)
#'Iris
#'
all_number <- function(df,...) {
    df <- as_tibble(df)
    classes <- purrr::map_chr(df, class)
    nnm <- !classes %in% "numeric"
    no_num <- df[, names(df)[nnm]]
    no_mun_2 <- categorizar(no_num, ALL = F,...)
    df_n <- cbind(df[, !nnm], no_mun_2)
    df_n
}
