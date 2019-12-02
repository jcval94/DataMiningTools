#' Selects random rows from a table
#'
#' @param df data.frame object
#' @param abs
#' @param rel
#'
#' @return
#' @export
#'
#' @examples
#'
#' iris2<-sample.df(iris,rel=.5)
#'
#' iris2
#'
#' iris3<-sample.df(iris,20)
#'
#' iris3
#'
sample.df <- function(df, abs = 25000, rel) {
  if (nrow(df) > abs & missing(rel)) {
    return(df[sample(seq_len(nrow(df)), size = abs), ])
  }
  if (!missing(rel)) {
    if (rel > 1 | rel < 0) {
      warning("rel must be between 0 and 1");return(NULL)
    }
    return(df[sample(seq_len(nrow(df)), size = floor(nrow(df) * rel)), ])
  }
}
