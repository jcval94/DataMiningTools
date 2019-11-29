#' Transforms all factors class verctors of a data.frame object into string/character class
#'
#' @param df data frame object
#'
#' @return
#' @export
#'
#' @examples
#'
#' P<-fact_as_string(iris)
#'
fact_as_string <- function(df) {
    f_a_s <- function(X) {
        if (class(X) %in% c("factor")) {
            X <- as.character(X)
        }
        X
    }
    purrr::map_df(df,f_a_s)
}
