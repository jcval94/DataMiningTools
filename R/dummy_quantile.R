#' Get dummie variable from a variable or
#'
#' @param X vnumeric ector
#' @param InfSup TRUE,
#' @param a quantile
#'
#' @return a list with the superior and inferior quantile (if applies) and a dummie vector
#' @export
#'
#' @examples
#'
#' x<-rnorm(1000)
#'
#' quantile_dummy(x)
#'
#'
dummy_quantile<-function(X,InfSup=TRUE,a=.975){
  if(InfSup){q<-c(1-a,a)}else{q<-c(0,a)}
  Q<-quantile(X,q)
  X_OL<-ifelse(X< Q[1] | X> Q[2],1,0)
  list(Q,X_OL)
}
