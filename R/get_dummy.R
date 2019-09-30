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
get_dummy<-function(X,method,...){
  interquartile_range_dummy<-function(X){
    IR<-quantile(X)
    IR_diff<-IR[4]-IR[2]
    Inter_Q<-c(IR[2]-IR_diff,IR[4]+IR_diff)
    X_OL<-ifelse(X< Inter_Q[1] | X> Inter_Q[2],1,0)
    list(Inter_Q,X_OL)
  }

  sd_dummy<-function(X,InfSup=TRUE,sigmas=5){
    if(InfSup){cond<-X< -s*sigmas | X> s*sigmas}else{cond<- X> s*sigmas}
    s<-sd(X)
    X_OL<-ifelse(cond,1,0)
    list(s,X_OL)
  }

  quantile_dummy<-function(X,InfSup=T,a=.975){
    if(InfSup){q<-c(1-a,a)}else{q<-c(0,a)}
    Q<-quantile(X,q)
    X_OL<-ifelse(X< Q[1] | X> Q[2],1,0)
    list(Q,X_OL)
  }
}
