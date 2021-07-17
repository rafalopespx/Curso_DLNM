library(dlnm)

# Miercoles
nlag<-l
mi.argvar=list(fun=“lin” int=F)
mi.arglag=list(fun=“poly” degree=2,int=T)
cb<-crossbasis(datos$predictor, lag=nlag, argvar=mi.argvar,
               arglag=mi.arglag)

model.glm<-glm(nonext∼cb+pred1+...+predm,...)

pred<-crosspred(cb,model.glm, at=valorpred1, cen=valorpred0)

tpred<-quantile(predictor, probs)
bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))
pred<-crosspred(basis=bvar,coef=coef(cb),vcov=vcov(cb),at=...,cen=...)

xlag <- 1:L
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
pred<-crosspred(basis=blag,coef=coef(cb),vcov=vcov(cb),at=...,cen=...)

# Jueves

red <-crossreduce(cb, model, at= tpred)

red <-crossreduce(cb, model, type= "var", value= p95, cen= mmt)

mv <- mvmeta(coef ∼ 1, vcov, method="reml")

mv <- mvmeta(coef ∼ metap1+ metap2+ ..., vcov, method="reml")

coefBlup <- blup(mv, vcov=T)

bvar <- do.call("onebasis", c(list(x=tpred), attr(cb,"argvar")))

blag <- do.call("onebasis", c(list(x=xlag), attr(cb,"arglag")))

metapred<-crosspred(basis=bvar,coef=coef(mv),vcov=vcov(mv),at=tpred)

predict(mv, newdata= data.frame(metap10 , metap20 )vcov=T,
        interval=çonfidence")


