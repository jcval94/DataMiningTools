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
#' set.seed(31109)
#' x<-rnorm(1000)
#'
#' #Numeric vector
#' dummy_sd(x)
#'
#' #Data frame
#' dummy_sd(iris)
#'
dummy_sd<-function(X,InfSup=TRUE,sigmas=3){
  if("data.frame" %in% class(X)){
    Cols<-(lapply(X,dummy_sd))
    cuts<-list()
    for(cl in 1:length(Cols)){
      clm<-Cols[[cl]][[2]]
      cuts[[cl]]<-Cols[[cl]][[1]]
      if(!is.numeric(clm)){next()}
      X[[paste0(names(Cols)[cl],"_ir")]]<-clm
    }
    return(list(X,cuts))
  }else{
    if(is.numeric(X)){
      s<-sd(X)
      if(InfSup){cond<-X< -s*sigmas | X> s*sigmas}else{cond<- X> s*sigmas}
      X_OL<-ifelse(cond,1,0)
      list(s,X_OL)
    }else{
      return(list(NA,X))
    }
  }
}
