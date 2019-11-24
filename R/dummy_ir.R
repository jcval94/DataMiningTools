#' interquartile range
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
#'
#' dummy_ir(iris)
#'
dummy_ir<-function(X){
  if("data.frame" %in% class(X)){
    return(sapply(X,dummy_ir(X)[[2]]))
  }else{
    if(is.numeric(X)){
      IR<-quantile(X)
      IR_diff<-IR[4]-IR[2]
      Inter_Q<-c(IR[2]-IR_diff,IR[4]+IR_diff)
      X_OL<-ifelse(X< Inter_Q[1] | X> Inter_Q[2],1,0)
      return(list(Inter_Q,X_OL))
    }
  }
}
