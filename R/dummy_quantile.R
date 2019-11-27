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
#' set.seed(31109)
#' x<-rnorm(1000)
#'
#' #Numeric vector
#' dummy_quantile(x)
#'
#' #Data frame
#' dummy_quantile(iris)
#'
#'
dummy_quantile<-function(X,InfSup=TRUE,a=.975){
  if("data.frame" %in% class(X)){
    Cols<-(lapply(X,dummy_quantile))
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
      if(InfSup){q<-c(1-a,a)}else{q<-c(0,a)}
      Q<-quantile(X,q)
      X_OL<-ifelse(X< Q[1] | X> Q[2],1,0)
      list(Q,X_OL)
    }else{
      return(list(NA,X))
    }
  }
}
