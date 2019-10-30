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
reducir.df.row <- function(df, abs = 25000, rel) {
    if (rel > 1 | rel < 0) {
        return(NULL)
    }
    if (nrow(df) > abs & missing(rel)) {
        return(df[sample(seq_len(nrow(df)), size = abs), ])
    }
    if (!missing(rel)) {
        return(df[sample(seq_len(nrow(df)), size = floor(nrow(df) * rel)), ])
    }
}
