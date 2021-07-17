################################################################################
# FUNCTION TO ESTIMATE MINIMUM OF A EXPOSURE-RESPONSE FUNCTION FROM A FITTED MODEL
################################################################################
#
# DISCLAIMER:
#   THE CODE COMPOSING THIS FUNCTION HAS NOT BEEN SYSTEMATICALLY TESTED. THE
#   PRESENCE OF BUGS CANNOT BE RULED OUT. ALSO, ALTHOUGH WRITTEN GENERICALLY
#   FOR WORKING IN DIFFERENT SCENARIOS AND DATA, THE FUNCTION HAS NOT BEEN
#   TESTED IN CONTEXTS DIFFERENT THAN THE EXAMPLE INCLUDED IN THE PAPER.
#   IT IS RESPONSIBILITY OF THE USER TO CHECK THE RELIABILITY OF THE RESULTS IN
#   DIFFERENT APPLICATIONS.
#
# Update: 18 October 2016
# AN UPDATED VERSION OF THIS FUNCTION CAN BE FOUND AT:
#   https://github.com/gasparrini/2017_tobias_Epidem_Rcodedata
#
################################################################################
#
findmin <- function(basis,model=NULL,coef=NULL,vcov=NULL,at=NULL,from=NULL,
  to=NULL,by=NULL,sim=FALSE,nsim=5000) {
#
################################################################################
#   ARGUMENTS:
#   - basis: A SPLINE OR OTHER BASIS FOR AN EXPOSURE x CREATED BY DLNM FUNCTION 
#            CROSSBASIS OR ONEBASIS
#   - model: THE FITTED MODEL
#   - coef AND vcov: COEF AND VCOV FOR basis IF model IS NOT PROVIDED
#
#   - at: A NUMERIC VECTOR OF x VALUES OVER WHICH THE MINIMUM IS SOUGHT
#   OR 
#   - from, to: RANGE OF x VALUES OVER WHICH THE MINIMUM IS SOUGHT.
#   - by: INCREMENT OF THE SEQUENCES x VALUES OVER WHICH THE MINIMUM IS SOUGHT
# 
#   - sim: IF BOOTSTRAP SIMULATION SAMPLES SHOULD BE RETURNED
#   - nsim: NUMBER OF SIMULATION SAMPLES
################################################################################


################################################################################
# CREATE THE BASIS AND EXTRACT COEF-VCOV
#
  # CHECK AND DEFINE BASIS  
  if(!any(class(basis)%in%c("crossbasis","onebasis")))
    stop("the first argument must be an object of class 'crossbasis' or 'onebasis'")
#
  # INFO
  one <- any(class(basis)%in%c("onebasis"))
  attr <- attributes(basis)
  range <- attr(basis,"range")
  if(is.null(by)) by <- 0.1
  lag <- if(one) c(0,0) else cb=attr(basis,"lag")
  if(is.null(model)&&(is.null(coef)||is.null(vcov)))
    stop("At least 'model' or 'coef'-'vcov' must be provided")
  name <- deparse(substitute(basis))
  cond <- if(one) paste(name,"[[:print:]]*b[0-9]{1,2}",sep="") else 
    paste(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
#
  # SET COEF, VCOV CLASS AND LINK
  if(!is.null(model)) {
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- dlnm:::getlink(model,model.class)
  } else model.class <- NA
#
  # CHECK
  if(length(coef)!=ncol(basis) || length(coef)!=dim(vcov)[1] ||
      any(is.na(coef)) || any(is.na(vcov)))
    stop("model or coef/vcov not consistent with basis")
#
  # DEFINE at
  at <- dlnm:::mkat(at,from,to,by,range,lag,bylag=1)
  predvar <- if(is.matrix(at)) rownames(at) else at
  predlag <- dlnm:::seqlag(lag,by=1)
#
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
  type <- if(one) "one" else "cb"
  Xpred <- dlnm:::mkXpred(type,basis,at,predvar,predlag,cen=NULL)
  Xpredall <- 0
  for(i in seq(length(predlag))) {
    ind <- seq(length(predvar))+length(predvar)*(i-1)
    Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
  }
#  
################################################################################
# FIND THE MINIMUM
#
  pred <- drop(Xpredall%*%coef)
  ind <- which.min(pred)
  min <- predvar[ind]
#
################################################################################
# SIMULATIONS
  #
  if(sim) {
    # SIMULATE COEFFICIENTS
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    # COMPUTE MINIMUM
    minsim <- apply(coefsim,2,function(coefi) {
      pred <- drop(Xpredall%*%coefi)
      ind <- which.min(pred)
      return(predvar[ind])
    })
  }
#
################################################################################
#
  res <- if(sim) minsim else min
#
  return(res)
}

#