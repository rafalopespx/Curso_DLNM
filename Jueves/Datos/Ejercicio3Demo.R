
## Librerias


rm(list=ls())

library(tidyverse)
library(lubrifecha)
library(tsModel)
library(MASS)
library(gridExtra)
library(grid)
library(ggplot2)
library(mgcv)
library(splines)
library(dlnm)
library(mvmeta) 
library(plot3D)
library(tsibble)



# datos

load("datosSC.RData")
dCV<-dCV_94_06
lapply(dCV, function(x){summary(x)})

# parámetros previos

namesc<-names(dCV)
len<-length(namesc)
dfseas<-7
formula <-tm~cb+dow+ns(fecha,df=dfseas*length(unique(yy)))


###############################################################################################################
# Ejercicio 3: ANÁLISIS COMBINADO PARA DISTRIBUCIÓN EN LAG ESCALA RELATIVA
################################################################################################################


# 0) pasos previos (igual que 2!!)

     # 0.1) traslado los datos a escala en percentiles.

      for(i in 1:len){
            Fd<-ecdf(dCV[[i]]$tmean)
            dCV[[i]]$ptmean<-Fd(dCV[[i]]$tmean)}

      sapply(dCV,function(x) summary(x$ptmean,na.rm=T))
      
      # 0.2) defino base y mallado
      
      varper<-c(10,50,90)
      knotsper<-varper/100
      varfun<-"bs"
      
      nlag<-21
      xlag<-0:nlag
      lagnk <- 3
      klag<-logknots(nlag,lagnk)
      lagfun<-"ns"
      
      tpred<-(1:99)/100
      
      argvarm<-list(fun=varfun, knots=knotsper, int=F)
      arglagm<-list(fun=lagfun, knots=klag,int=T)
      cb <- crossbasis(tpred, lag=nlag, argvar=argvarm, arglag=arglagm)
      bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar"))) 
      blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
  
      
      

#Etapa 1) Analizamos en las tres ciudades, guardando coeficientes y sus matrices de var-cov (map o bucle)

  coef<-matrix(NA,len,length(varper)+2,dimnames=list(names(dCV)))  #objetos para guardar  
  vcov<- vector("list",len)
  names(vcov) <-  names(dCV)
  
  mmp<-0.93

  for(i in 1:len) {
  
      #extraemos los datos
      datai <- dCV[[i]]
      
      
      #obtenemos la base
      cb <- crossbasis(datai$ptmean, lag=nlag, argvar=argvarm, arglag=arglagm)
      
      #ajustamos el modelo
      model <- glm(formula,datai,family=quasipoisson,na.action="na.exclude")
    
      #predicción reducida lag-distributed en P95 y centrado en mmp
      
      red <- crossreduce(cb, model, type = "var", value = 0.95, cen = mmp)
      mmp  <- red$predvar[which.min(red$RRfit)]
       
      coef[i,]  <- red$coef
      vcov[[i]] <- red$vcov
      
       
       cat("\t",i)
}


  
 # Etapa 2) Meta-análisis 
      
      mv<- mvmeta(coef~1,vcov,method="reml",control=list(showiter=T))
      summary(mv)
      
#  3) la Predicción: 
      
     
      # 3.1. Predicción overall combinada sin centrar
      Metapred<-crosspred(basis=blag,coef=coef(mv),vcov=vcov(mv), model.link="log")  
      plot(Metapred)

     
      ###########################################################################################
      # Algunos "extra".
      
      
# 4)  Resultados
      
      # 4.1) numéricos
      
      RR_P95_CV<-data.frame(lag= Metapred$predvar, RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh)
      
     
      # 4.2) gráficos
      
      # 4.2.1) curva lag respuesta más bonita
      
      RR_P95_CV%>%
        ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                       size = .8, show.legend = FALSE, colour="#d73027") +
        geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
        scale_x_continuous(breaks = seq(0, 21, 2)) +
        scale_y_continuous(breaks = seq(0.8, 3, 0.05)) +
        labs(x = "lag (días)", y = "RR ",title="Efecto P95 sobre Mortalidad total",subtitle="CV, 94-06")+
        theme_bw() 
        labs(x = "percentil temperatura (%) ", y = "RR",title="Temperatura y Mortalidad total",subtitle="CV, 94-06") 
      
      
     
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      