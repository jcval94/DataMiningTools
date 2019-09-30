#' content operator in a numeric range
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
#'
#' @examples
`%c%` <- function(e1, e2){
  as.numeric(e1)>=as.numeric(e2[1]) & as.numeric(e1)<=as.numeric(e2[2])
}
