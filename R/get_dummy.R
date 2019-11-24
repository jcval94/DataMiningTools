#' Funtion to get Outliers and splits through standar deviation
#'
#' @param X random sample, numeric vector
#' @param InfSup If TRUE, outliers from the left side of the density function will be considered
#' @param sigmas Integer that indicates the number that will be multiplied by the standard deviation to considere an observation as outlier
#'
#' @return a list with the splits and a dummy variable indicating if an observation is an outlier
#' @export
#'
#' @examples
#'
#'
get_dummy<-function(X,method=c("all","ir","sd","quantile"),...){
  if("all" %in% method){
    method<-c("ir","sd","quantile")
  }
  fun_dum<-lapply(method,function(x){
    get(paste0("dummy_",x))
  })

  nested(X,fun_dum)

}
