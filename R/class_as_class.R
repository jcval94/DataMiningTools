#' Transform columns class of a data.frame into
#'
#' @param df data.frame objec
#' @param cl_from a character which indicates the class to be changed
#' @param cl_to a character which indicates the new class
#'
#' @return A tibble object with new class on corresponding columns
#' @export
#'
#' @examples
#'
#'data(iris)
#'
#'class_as_class(iris,"factor","character")
#'
#'class_as_class(iris,"numeric","character")
#'
#'class_as_class(iris,"factor","numeric")
#'
class_as_class <- function(df, cl_from, cl_to) {
    f_a_s <- function(X) {
        if (class(X) %in% cl_from) {
            fun_as <- get(paste0("as.", cl_to))
            X <- fun_as(X)
        }
        X
    }
    purrr::map_df(df, ~f_a_s(.x))
}
