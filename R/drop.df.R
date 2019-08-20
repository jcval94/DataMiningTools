#' Drop repeated rows or columns and columns with just one value
#'
#' @param df data frame to be clean
#'
#' @return data.frame cleane
#' @export
#'
#' @importFrom purrr map_lgl
#'
#' @examples
#'
#' data(iris)
#' iris[["A"]] <- 0
#' head(iris)
#' head(drop.df(iris))
#'
drop.df <- function(df) {
    df1<-df[, purrr::map_lgl(df, ~length(unique(.x)) > 1)]
    df1<-df1[!duplicated(df1),]
    df1<-dft(df1)
    df1<-df1[, purrr::map_lgl(df1, ~length(unique(.x)) > 1)]
    df1<-df1[!duplicated(df1),]
    return(dft(df1))
}
