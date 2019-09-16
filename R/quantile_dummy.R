quantile_dummy<-function(X,InfSup=T,a=.975){
  if(InfSup){q<-c(1-a,a)}else{q<-c(0,a)}
  Q<-quantile(X,q)
  X_OL<-ifelse(X< Q[1] | X> Q[2],1,0)
  list(Q,X_OL)
}
