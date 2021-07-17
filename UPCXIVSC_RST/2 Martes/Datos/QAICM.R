
QAICM <- function(model,type="logLik") {
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
