
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

# dire
setwd("C:/4.Curricula/curso UPC/clase 2/practica") 
source("funciones.R")



# datos

load("datosSC.RData")
dCV<-dCV_94_06
lapply(dCV, function(x){summary(x)})

# Parámetros básicos 

namesc<-names(dCV)
len<-length(namesc)
dfseas<-7

formula <-tm~cb+dow+ns(fecha,df=dfseas*length(unique(yy)))




###############################################################################################################
# Ejercicio 3: ANÁLISIS COMBINADO PARA OVERALL ESCALA RELATIVA
################################################################################################################


# 0) pasos previos

     # 0.1) traslado los datos a escala en percentiles.

      
      for(i in 1:len){
            Fd<-ecdf(dCV[[i]]$tmean)
            dCV[[i]]$ptmean<-Fd(dCV[[i]]$tmean)}

      sapply(dCV,function(x) summary(x$ptmean,na.rm=T))
      
      # 0.2) puedo definir la base general y el mallado para la predicción
      
      varper<-c(10,50,90)
      knotsper <- varper/100
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
      
      
   
# 1) Analizamos en las tres ciudades, guardando coeficientes y sus matrices de var-cov (map o bucle)

  coef<-matrix(NA,len,length(varper)+3,dimnames=list(names(dCV)))  #objetos para guardar  
  vcov<- vector("list",len)
  names(vcov) <-  names(dCV)

  for(i in 1:len) {
  
      #extraemos los datos
      datai <- dCV[[i]]
      
      #especificamos la base 
    
      argvar <- list(fun=varfun,knots=knotsper)
    
      #obtenemos la base
      cb <- crossbasis(datai$ptmean, lag=nlag, argvar=argvarm, arglag=arglagm)
      
      #ajustamos el modelo
      model <- glm(formula,datai,family=quasipoisson,na.action="na.exclude")
    
      #predicción reducida: overall
       red <- crossreduce(cb,model,at=tpred)
       coef[i,] <- red$coef
       vcov[[i]] <- red$vcov
       
       cat("\t",i)
}


  
 # 2) meta-análisis
      
      
      mv<- mvmeta(coef~1,vcov,method="reml",control=list(showiter=T))
      summary(mv)
      
#  3) la Predicción: 
      
      bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))  #solo necesitamos bvar
       
      # 3.1. Predicción overall combinada sin centrar
      Metapred<-crosspred(basis=bvar,coef=coef(mv),vcov=vcov(mv),at=tpred,model.link="log")  
      plot(Metapred)
       
      # 3.2 Predicción overall combinada centrada en mmt
      (metaMMT<-Metapred$predvar[which.min(Metapred$allfit)])  #calculamos mmt    
      Metapred<-crosspred(basis=bvar,coef=coef(mv),vcov=vcov(mv),cen=metaMMT,at=tpred,model.link="log")  #centramos
      
      # 3.3. Predicción ciudad específica a partir del meta-análisis (fixef+ranef)
       blups <- blup(mv,vcov=TRUE)  
      
      # 3.4 Predicción ciudad específica a partir del meta-análisis (sólo fixef)
       
      metacity<-predict(mv, vcov=T, interval="confidence")  
      
      # Con base única, 3.1 y 3.4 coinciden.
      
      
###########################################################################################
# Algunos "extra".
      
# 4)  Resultados
      
      # 4.1) numéricos
      
      res_CV<-data.frame(ptmean= Metapred$predvar, RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh)
      
     
    
      # 4.2) gráficos
      
      # 4.2.1) curva dosis respuesta
      
      xlab<-seq(7,28, by=5)
      ylab<-pretty(c( res_CV$LowRR, res_CV$HighRR))
      res_CV %>% 
        ggplot(aes(ptmean, RR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.7) +
        geom_line(colour="#cb181d",size=1) +
        geom_point(aes(metaMMT,1),shape = 21, fill = "white", size = 2, colour="#cb181d",
                   show.legend = FALSE) +
        scale_x_continuous(breaks = xlab) +
        scale_y_continuous(breaks = ylab, limits=c(0.5,2.5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        labs(x = "percentil temperatura (%) ", y = "RR",title="Temperatura y Mortalidad total",subtitle="CV, 94-06") 
      
      
      # 4.2.2) Comparaciónn Blups, original y metas
      
      RR_CV<-data.frame(RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh, ciudad="All")
      
      for(i in 1:3){
        predB<-crosspred(basis=bvar,coef=blups[[i]]$blup,vcov=blups[[i]]$vcov,at=tpred,model.link="log")
        predO<-crosspred(basis=bvar,coef=coef[i,],vcov=vcov[[i]],at=tpred,model.link="log")
        predM<-crosspred(basis=bvar,coef=metacity[[i]]$fit,vcov=metacity[[i]]$vcov,at=tpred,model.link="log")
        
        (metaMMTB<-predB$predvar[which.min(predB$allfit)])
        (metaMMTO<-predO$predvar[which.min(predO$allfit)])
        (metaMMTM<-predM$predvar[which.min(predM$allfit)])
        
        predB<-crosspred(basis=bvar,coef=blups[[i]]$blup,vcov=blups[[i]]$vcov,cen=metaMMTB,at=tpred,model.link="log")
        predO<-crosspred(basis=bvar,coef=coef[i,],vcov=vcov[[i]],cen=metaMMTO, at=tpred,model.link="log")
        predM<-crosspred(basis=bvar,coef=metacity[[i]]$fit,vcov=metacity[[i]]$vcov,cen=metaMMTB,at=tpred,model.link="log")
        
        RR_O_i<-data.frame(ptmean=predO$predvar, RR=predO$allRRfit,LowRR=predO$allRRlow,HighRR=predO$allRRhigh, ciudad=names(dCV)[i],tipo="original")
        RR_B_i<-data.frame(ptmean=predB$predvar, RR=predB$allRRfit,LowRR=predB$allRRlow,HighRR=predB$allRRhigh, ciudad=names(dCV)[i],tipo="blups")
        RR_M_i<-data.frame(ptmean=predM$predvar, RR=predM$allRRfit,LowRR=predM$allRRlow,HighRR=predM$allRRhigh, ciudad=names(dCV)[i],tipo="meta")
         
        RRi<-bind_rows(RR_O_i,RR_B_i,RR_M_i)%>%mutate_at(5:6,"factor") # Riesgos relativos y CI por lag
        assign(paste("RR",i,sep=""),RRi)
      }
      
      RR<-bind_rows(RR1,RR2,RR3)
      
      RR %>% 
        ggplot(aes(ptmean, RR)) + 
        geom_line(aes(x = ptmean, y = RR, colour = tipo),size=1,
                  show.legend = T) +
        scale_x_continuous(breaks = xlab) +
        geom_hline(yintercept = 1, size = 0.5) +
        scale_y_continuous(breaks = ylab,limits=c(0.5,3.5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) + scale_color_brewer(palette = "Set1")+
        labs(x = "Percentil Temperatura (%)", y = "RR",title="Temperatura y Mortalidad total",subtitle="CV, 94-06")+
        facet_wrap(vars(ciudad),ncol=3)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      