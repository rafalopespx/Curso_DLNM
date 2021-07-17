

retard<-function(x,i){
##########################################################
# función para calcular el retardo i de x.
##########################################################
	l<-length(x)
if(i>0){
	x1<-lag(x,-i)
	x2<-c(rep(NA,i),x1)
	x2<-x2[1:l]
}
else{
	x1<-lag(x,i)
	x2<-c(x1,rep(NA,abs(i)),x1)	
	x2<-x2[abs(i)+1:l+abs(i)-1]
}
}







###################### Funcion qaic #######################

QAICM <- function(model,type="logLik") {
QAICm <- vector("list",0)

  if(!model$family$family%in%c("poisson","quasipoisson")) {
  	stop("only for poisson/quasipoisson family")
  }

  phi <- summary(model)$dispersion
  if(type=="dev") {
	  QAICm <- deviance(model) + 2*summary(model)$df[3]*phi
  } else {
  	loglik <- sum(dpois( model$y, model$fitted.values, log=TRUE))
  	QAICm <- -2*loglik + 2*(summary(model)$n-summary(model)$residual.df)*phi
  }

  return(QAICm)
}


# WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}


# https://www.google.com/search?q=impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization&rlz=1C1CHBD_esES887ES887&oq=impressibe+package&aqs=chrome.1.69i57j0i22i30j0i8i13i30.8603j1j7&sourceid=chrome&ie=UTF-8