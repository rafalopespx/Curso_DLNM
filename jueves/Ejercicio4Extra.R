
## Librerias


rm(list=ls())

library(tidyverse)
library(lubridate)
library(tsModel)
library(MASS)
library(gridExtra)
library(grid)
library(ggplot2)
library(mgcv)
library(splines)
library(dlnm)
library(mvmeta) 
# library(plot3D)
library(tsibble)
library(mixmeta)

# dire
# setwd("C:/4.Curricula/curso UPC/clase 2/practica") 
source("Desktop/Curso_DLNM/jueves/Datos/funciones.R")



# datos

load("Desktop/Curso_DLNM/jueves/Datos/datosSC.RDATA")
lapply(dCV_94_06, function(x){summary(x)})

# Parámetros básicos 

namesc<-names(dCV_94_06)
len<-length(namesc)
dfseas<-7

formula <-tm~cb+dow+ns(fecha,df=dfseas*length(unique(yy)))

varper<-c(10,50,90)
varfun<-"bs"

nlag<-21
xlag<-0:nlag
lagnk <- 3
klag<-logknots(nlag,lagnk)
lagfun<-"ns"



###############################################################################################################
# Ejercicio 4: METARREGRESIÓN  OVERALL ESCALA RELATIVA
################################################################################################################


# 0) pasos previos 

     # 0.1) traslado los datos a escala en percentiles.

      len<-3

      for(i in 1:len){
            Fd<-ecdf(dCV_94_06[[i]]$tmean)
            dCV_94_06[[i]]$ptmean<-Fd(dCV_94_06[[i]]$tmean)}

      sapply(dCV_94_06,function(x) summary(x$ptmean,na.rm=T))
      
      # 0.2) puedo definir la malla general para la predicción y la base general (únicas)
      
      knotsper <- varper/100
      tpred<-(1:99)/100
      
      xlag <- 0:21
      argvarm<-list(fun=varfun, knots=knotsper, int=F)
      arglagm<-list(fun=lagfun, knots=klag,int=T)
      cb <- crossbasis(tpred, lag=nlag, argvar=argvarm, arglag=arglagm)
      bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))
      blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

# 1) Analizamos en las tres ciudades, guardando coeficientes y sus matrices de var-cov (map o bucle)

  redall<-vector("list",len)
  coef<-matrix(NA,len,length(varper)+3,dimnames=list(names(dCV_94_06)))  #objetos para guardar  
  vcov<- vector("list",len)
  names(vcov) <-  names(dCV_94_06)

  for(i in 1:len) {
  
      #extraemos los datos
      datai <- dCV_94_06[[i]]
      
      #especificamos la base 
    
      argvar <- list(fun=varfun,knots=knotsper)
    
      #obtenemos la base
      cb <- crossbasis(datai$ptmean, lag=nlag, argvar=argvarm, arglag=arglagm)
      
      #ajustamos el modelo
      model <- glm(formula,datai,family=quasipoisson,na.action="na.exclude")
    
      #predicción reducida: overall
       redall[[i]] <- crossreduce(cb,model,at=tpred)
       coef[i,] <- redall[[i]]$coef
       vcov[[i]] <- redall[[i]]$vcov
       
       cat("\t",i)
}


  
 # 2) meta-análisis (crudo)
  
      par(mfrow=c(1,3), mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)
      for(i in seq(length(redall))) {
          plot(redall[[i]], main= names(dCV_94_06)[i])
        }  #son bastante diferentes
      
      m.crudo<- mvmeta(coef~1,vcov,method="reml")
      summary(m.crudo)  #elevada heterogeneidad
      
      par(mfrow=c(1,3), mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)
      for(i in seq(length(redall))) {
        plot(redall[[i]], main= names(dCV_94_06)[i])
      }
      
      
      
#  3) meta-regresión (modelo ajustado)
      
      # vamos a crear a partir de las series algunas metavariables que podrían explicar la heterogeneidad
      
      avgtmean <- sapply(dCV_94_06,function(x) mean(x$tmean,na.rm=T))
      avgrh    <- sapply(dCV_94_06,function(x) mean(x$rh,na.rm=T))
      metadata <- data.frame(avgtmean=avgtmean, avgrh=avgrh)
        
      m.aj1<- mvmeta(coef~ avgtmean,vcov, data=metadata, method="reml")
      m.aj2<- mvmeta(coef~ avgrh   ,vcov, data=metadata, method="reml")
      
      summary(m.aj1)
      summary(m.aj2)
      
      
      # WALD TEST
      fwald <- function(model,var) {
        ind <- grep(var,names(coef(model)))
        coef <- coef(model)[ind]
        vcov <- vcov(model)[ind,ind]
        waldstat <- coef%*%solve(vcov)%*%coef
        df <- length(coef)
        return(1-pchisq(waldstat,df))
      }
      
      fwald(m.aj1,"avgtmean")
      fwald(m.aj2,"avgrh")  #explica heterogeneidad
      
      # La temperatura media no está explicando la heterogeneidad, pero la rh sí!
      # Pero deberiase hacer un medida de la modulación de las covariables que explican la heterogeniedad, como un efecto por clima o por region
      
      
      # 3.1. Predicción overall ajustada  en la media de humedad relativa
      
      
      newdata <- data.frame(avgrh=mean(avgrh))
      pred <- predict(m.aj2, newdata=newdata, vcov=T)
      Metapred.aj <- crosspred(bvar, coef=pred$fit, vcov=pred$vcov,model.link="log", at=tpred)
      plot(Metapred.aj)
       
     # 3.2. O comparar curvas para diferentes valores de humedad.
      
      newdata <- data.frame(avgrh=c(min(avgrh), max(avgrh)))
      pred <- predict(m.aj2, newdata=newdata, vcov=T)
      Metapred.ajLowrh <-  crosspred(bvar, coef=pred[[1]]$fit, vcov=pred[[1]]$vcov,model.link="log", at=tpred)
      Metapred.ajHighrh <- crosspred(bvar, coef=pred[[2]]$fit, vcov=pred[[2]]$vcov,model.link="log", at=tpred)
      
      par(mfrow=c(1,3), mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)
      plot(Metapred.ajLowrh, main="humedad en el mínimo")
      plot(Metapred.ajHighrh, main="humedad en el máximo")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      