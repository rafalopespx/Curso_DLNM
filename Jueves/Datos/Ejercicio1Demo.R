
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

# Parámetros básicos 

namesc<-names(dCV)
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
arlagm=list(fun=lagfun, knots=klag,int=T)

###############################################################################################################
# Ejercicio 1: ANÁLISIS COMBINADO PARA OVERALL ESCALA ABSOLUTA
################################################################################################################


# Analizamos en las tres ciudades, guardando coeficientes y sus matrices de var-cov (map o bucle)

  red <- vector("list",len)
  coef<- matrix(NA,len,length(varper)+3,dimnames=list(names(dCV)))   
  vcov<- vector("list",len); names(vcov) <-  names(dCV)
  
# Bucle para el análisis

  for(i in 1:len) {
  
      #extraemos los datos
      datai <- dCV[[i]]
      
      #especificamos la base 
      knotsper<-quantile(datai$tmean, probs=varper/100, na.rm=T)
      argvarm <- list(fun=varfun,knots=knotsper,int=F)
    
      #obtenemos la base
      cb <- crossbasis(datai$tmean, lag=nlag, argvar=argvarm, arglag=arlagm)
      
      #ajustamos el modelo
      model <- glm(formula,datai,family=quasipoisson,na.action="na.exclude")
    
      #determinamos la malla de predicción
      tpred<-quantile(datai$tmean, probs=(1:99)/100, na.rm=T)
 
      #predicción reducida: overall
       red[[i]] <- crossreduce(cb,model,at=tpred)
       coef[i,] <- red[[i]]$coef
       vcov[[i]] <- red[[i]]$vcov
       
       cat("\t",i)
}

  
   
 # 2) meta-análisis
      
      
      mv<- mvmeta(coef~1,vcov,method="reml",control=list(showiter=T))
      summary(mv)
      
#  3) la Predicción: 
      
      #La base debería ser equivalente a la usada en cada una de las tres ciudades. Punto problemático los knots de la nueva base
      # Una opción:
      knotsper<- rowMeans(sapply(dCV, function(x){quantile(x$tmean, probs=varper/100, na.rm=T)}))
      # Otra algo mejor:
      tmeanCV <-c(dCV[[1]]$tmean,dCV[[2]]$tmean,dCV[[3]]$tmean )
      knotsper<-quantile(tmeanCV, probs=varper/100, na.rm=T)
      
      # para la predicción podemos, por ejemplo,
      #predecir por grado
      tpred  <-round(min(tmeanCV, na.rm=T)): round(max(tmeanCV, na.rm=T))
      #predecir en percentiles
      tpred  <-quantile(tmeanCV, probs=1:99/100, na.rm=T) 
      
      argvarm<-list(fun=varfun, knots=knotsper)
      arglagm<-list(fun=lagfun, knots=klag)
      cb <- crossbasis(tpred, lag=nlag, argvar=argvarm, arglag=arglagm)
      bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))  #solo necesitamos bvar
       
      # 3.1. Predicción overall combinada sin centrar
      Metapred<-crosspred(basis=bvar,coef=coef(mv),vcov=vcov(mv),at=tpred,model.link="log")  
      
      # 3.2 Predicción overall combinada centrada en mmt
      (metaMMT<-Metapred$predvar[which.min(Metapred$allfit)])  #calculamos mmt    
      Metapred<-crosspred(basis=bvar,coef=coef(mv),vcov=vcov(mv),cen=metaMMT,at=tpred,model.link="log")  #centramos
      plot(Metapred)
      
      # 3.3. Predicción ciudad específica a partir del meta-análisis (fixef+ranef)
      blups <- blup(mv,vcov=TRUE)  
      
      # 3.4 Predicción ciudad específica a partir del meta-análisis (sólo fixef)
       
      metacity<-predict(mv, vcov=T, interval="confidence")  
      
      # Con base única, 3.1 y 3.4 coinciden.
      
 ###########################################################################################
 # Algunos "extra".
      
# 4)  Resultados
      
      # 4.1) numéricos
      
      res_CV<-data.frame(tmean= Metapred$predvar, RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh)
      
     
    
      # 4.2) gráficos
      
      # 4.2.1) curva dosis respuesta
      
      xlab<-seq(7,28, by=5)
      ylab<-pretty(c( res_CV$LowRR, res_CV$HighRR))
      res_CV %>% 
        ggplot(aes(tmean, RR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.7) +
        geom_line(colour="#cb181d",size=1) +
        geom_point(aes(metaMMT,1),shape = 21, fill = "white", size = 2, colour="#cb181d",
                   show.legend = FALSE) +
        scale_x_continuous(breaks = xlab) +
        scale_y_continuous(breaks = ylab, limits=c(0.5,2.5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        labs(x = "temperatura media diaria (ºC)", y = "RR",title="Temperatura y Mortalidad total",subtitle="CV, 94-06") 
      
      
      # 4.2.2) Comparaciónn Blups, original y metas
      
      RR_CV<-data.frame(RR=Metapred$allRRfit,LowRR=Metapred$allRRlow,HighRR=Metapred$allRRhigh, ciudad="All")
      
      for(i in 1:3){
        datai<-dCV[[i]]
        tpred<-quantile(datai$tmean,probs=(1:99)/100, na.rm=T)
        argvarm<-list(fun=varfun, knots=knotsper)
        arglagm<-list(fun=lagfun, knots=klag)
        cb <- crossbasis(tpred, lag=nlag, argvar=argvarm, arglag=arglagm)
        bvar <- do.call("onebasis",c(list(x=tpred),attr(cb,"argvar")))  #solo necesitamos bvar
        
        predB<-crosspred(basis=bvar,coef=blups[[i]]$blup,vcov=blups[[i]]$vcov,at=tpred,model.link="log")
        predO<-crosspred(basis=bvar,coef=coef[i,],vcov=vcov[[i]],at=tpred,model.link="log")
        predM<-crosspred(basis=bvar,coef=metacity[[i]]$fit,vcov=metacity[[i]]$vcov,at=tpred,model.link="log")
        
        (metaMMTB<-predB$predvar[which.min(predB$allfit)])
        (metaMMTO<-predO$predvar[which.min(predO$allfit)])
        (metaMMTM<-predM$predvar[which.min(predM$allfit)])
        
        predB<-crosspred(basis=bvar,coef=blups[[i]]$blup,vcov=blups[[i]]$vcov,cen=metaMMTB,at=tpred,model.link="log")
        predO<-crosspred(basis=bvar,coef=coef[i,],vcov=vcov[[i]],cen=metaMMTO, at=tpred,model.link="log")
        predM<-crosspred(basis=bvar,coef=metacity[[i]]$fit,vcov=metacity[[i]]$vcov,cen=metaMMTB,at=tpred,model.link="log")
        
        RR_O_i<-data.frame(tmean=predO$predvar, RR=predO$allRRfit,LowRR=predO$allRRlow,HighRR=predO$allRRhigh, ciudad=names(dCV)[i],tipo="original")
        RR_B_i<-data.frame(tmean=predB$predvar, RR=predB$allRRfit,LowRR=predB$allRRlow,HighRR=predB$allRRhigh, ciudad=names(dCV)[i],tipo="blups")
        RR_M_i<-data.frame(tmean=predM$predvar, RR=predM$allRRfit,LowRR=predM$allRRlow,HighRR=predM$allRRhigh, ciudad=names(dCV)[i],tipo="meta")
         
        RRi<-bind_rows(RR_O_i,RR_B_i,RR_M_i)%>%mutate_at(5:6,"factor") # Riesgos relativos y CI por lag
        assign(paste("RR",i,sep=""),RRi)
      }
      
      RR<-bind_rows(RR1,RR2,RR3)
      
      RR %>% 
        ggplot(aes(tmean, RR)) + 
        geom_line(aes(x = tmean, y = RR, colour = tipo),size=1,
                  show.legend = T) +
        scale_x_continuous(breaks = xlab) +
        geom_hline(yintercept = 1, size = 0.5) +
        scale_y_continuous(breaks = ylab,limits=c(0.5,3.5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) + scale_color_brewer(palette = "Set1")+
        labs(x = "Temperatura media (ºC)", y = "RR",title="Temperatura y Mortalidad total",subtitle="CV, 94-06")+
        facet_wrap(vars(ciudad),ncol=3)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      