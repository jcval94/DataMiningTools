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
sd_dummy<-function(X,InfSup=TRUE,sigmas=5){
  if(InfSup){cond<-X< -s*sigmas | X> s*sigmas}else{cond<- X> s*sigmas}
  s<-sd(X)
  X_OL<-ifelse(cond,1,0)
  list(s,X_OL)
}
