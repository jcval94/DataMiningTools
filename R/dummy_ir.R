#' interquartile range
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
#'
#' set.seed(31109)
#' x<-rnorm(1000)
#'
#' #Numeric vector
#' dummy_ir(x)
#'
#' #Data frame
#' dummy_ir(iris)
#'
dummy_ir<-function(X){
  if("data.frame" %in% class(X)){
    Cols<-(lapply(X,dummy_ir))
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
      IR<-quantile(X)
      IR_diff<-IR[4]-IR[2]
      Inter_Q<-c(IR[2]-IR_diff,IR[4]+IR_diff)
      X_OL<-ifelse(X< Inter_Q[1] | X> Inter_Q[2],1,0)
      return(list(Inter_Q,X_OL))
    }else{
      return(list(NA,X))
    }
  }
}
