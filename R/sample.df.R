#' Drop columns whose NA percentage be greater than max_na
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
#' iris2<-reducir.df.row(iris,rel=.5)
#'
#' iris2
#'
#' iris3<-reducir.df.row(iris,20)
#'
#' iris3
#'
reducir.df.row <- function(df, abs = 25000, rel) {
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
