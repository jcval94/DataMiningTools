#' Title
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
interquartile_range_dummy<-function(X){
  IR<-quantile(X)
  IR_diff<-IR[4]-IR[2]
  Inter_Q<-c(IR[2]-IR_diff,IR[4]+IR_diff)
  X_OL<-ifelse(X< Inter_Q[1] | X> Inter_Q[2],1,0)
  list(Inter_Q,X_OL)
}
