sd_dummy<-function(X,InfSup=T,sigmas=5){
  if(InfSup){cond<-X< -s*sigmas | X> s*sigmas}else{cond<- X> s*sigmas}
  s<-sd(X)
  X_OL<-ifelse(cond,1,0)
  list(s,X_OL)
}
