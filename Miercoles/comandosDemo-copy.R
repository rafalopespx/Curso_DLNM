
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
library(plot3D)
library(tsibble)


#datos y funciones
source("Dropbox/UPCXIVSC_RST/4 Jueves/Datos/funciones.R")
load("Dropbox/UPCXIVSC_RST/3 Miercoles/Datos/datosSC.RDATA")
dval<-dCV_94_06[["vlnc"]]  
head(dval,3) 

  
## Graficos descriptivos: 

span<-270/nrow(dval)


P1<-ggplot(dval, aes(fecha, tm)) + geom_line()+
  stat_smooth(method="loess",span=span,n=1000, col="#F44336")+
  scale_y_continuous("nº defunciones", breaks = pretty(dval$tm,5))+
  scale_x_date("día", date_breaks = "5 year", date_labels = "%Y")+ labs( title ="Mortalidad",subtitle="Conteos diarios. Valencia, 1990-2006")+ 
  scale_color_brewer(palette = "Set1")+theme_bw()
 

P2<-ggplot(dval, aes(fecha, tmean)) + geom_line()+
  stat_smooth(method="loess",span=span, n=1000,col="#26A69A")+
  scale_y_continuous("ºC", breaks = pretty(dval$tmean,5))+
  scale_x_date("día", date_breaks = "5 year", date_labels = "%Y")+ labs( title ="Temperatura",subtitle="Media diaria. Valencia, 1990-2006")+ 
  scale_color_brewer(palette = "Set1")+theme_bw()



grid.arrange(P1, P2,  nrow=2)
            


 ## generación de la crossbasis
 
mi.argvar=list(fun="poly",degree=3,int=F)    # estructura dosis-respuesta

# knotsx<-quantile(dval$tmean, probs = c(5,50,90)/100, na.rm = T)
# mi.argvar=list(fun="bs",knots = knotsx,int=F)

nlag<-10    
lagnk <- 3; klag<-logknots(nlag,lagnk)     # colocar os nós espaçados em la escala logaritmica 
mi.arglag=list(fun="ns",knots=klag,int=T)    # estructura lag-respuesta

cb<-crossbasis(dval$tmean, lag=nlag, argvar=mi.argvar, arglag=mi.arglag)   
summary(cb) 

# total df produto de u


## ajuste

nanyos<-length(unique(dval$yy))
formula<-"tm ~ cb+dow+ns(fecha,df=7*nanyos)"
model.glm<-glm(formula, data=dval, quasipoisson, na.action="na.exclude")  
summary(model.glm) 

tpred<-seq(3.5, 33.5, by=0.5) # malla para la predicción
pred<-crosspred(cb,model.glm, at=tpred)                 
summary(pred)
names(pred)                                           
pred #veamos...

predCum<-crosspred(cb,model.glm, cumul=T, at=tpred)  #aparecen cumfit y cumse (acumulados lag a lag)



 ## ajuste del modelo
 
 ny<-length(table(dval$yy))
 
 model.glm<-glm(tm~cb+dow+ns(fecha,df=7*ny), data=dval,family=quasipoisson,na.action="na.exclude")  #ajuste del modelo glm (podría ser gam)
 summary(model.glm)
 (pseudoR2<-1-model.glm$deviance/model.glm$null.deviance)
 QAICM(model.glm,"dev")
 
 
 model.gam<-gam(tm~cb+dow+ns(fecha,df=7*ny), data=dval,family=quasipoisson,na.action="na.exclude")  #ajuste del modelo gam (podría ser glm)
 summary(model.gam)
 QAICM(model.gam)

 ## predicción
 
 tpred<-seq(3.5, 33.5, by=0.5) # malla para la predicción
 #tpred<-quantile(dval$tmean, probs=(1:99)/100, na.rm=T)  # malla alternativa para la predicción
 
 pred<-crosspred(cb,model.glm, at=tpred)                 # objeto con las predicciones
 summary(pred)
 names(pred)
 
 # predvar és donde quiero predicir, és tpred,
 # cen, centro de la prediccion, és asumido se no da a crosspred
 
 
    #comprobaciones:
        sum(apply(pred$matfit,1,sum)-pred$allfit)       # el overall (all) es la suma de efectos
        sum(exp(pred$allfit)-pred$allRRfit)             # los RR overall son los beta overall en escala logarítmica.
        sum(exp(pred$matfit)-pred$matRRfit)             # los RR lag a lag son los beta lag a lag en escala logarítmica.
 

## centrado en el temperatura de mínimo efecto (TMM)      
 
 (mmt<-pred$predvar[which.min(pred$allRRfit)])          # temperatura de mínima mortalidad en Valencia 22.1 grados. 
 predcen<-crosspred(cb,model.glm, at=tpred,cen=mmt)         # predicción de efectos respecto de TMM
 
 #comprobaciones:
        predcen$allRRfit[predcen$predvar==mmt]
        predcen$matRRfit[predcen$predvar==mmt]
        
 
## representación grafica de las predicciones (base)
 
        #superficie de predicción
        plot(predcen) 
        
        # curva de efectos para un lag
        plot(predcen,ptype="slices",lag=10) 
        
        #curva de efectos para una exposición
        plot(predcen,ptype="slices",ci="bars", var=10) 
        
        #efecto total (acumulado)
        plot(predcen,ptype="overall")
        
###################################################################################################
# algunos extra
 
 ## rescate de riesgos relativos y gráficos bonitos (ggplot)
 
        tpred<-quantile(dval$tmean, probs=(1:99)/100, na.rm=T) # malla para la predicción
        pred<-crosspred(cb,model.glm, at=tpred) 
        (mmt<-pred$predvar[which.min(pred$allRRfit)]) 
        predcen<-crosspred(cb,model.glm, at=tpred,cen=mmt)
        
 #RR por lag
 df_frio<-data.frame(RR=predcen$matRRfit[5,],LowRR=predcen$matRRlow[5,],HighRR=predcen$matRRhigh[5,],idE="frio")
 df_calor<-data.frame(RR=predcen$matRRfit[95,],LowRR=predcen$matRRlow[95,],HighRR=predcen$matRRhigh[95,],idE="calor")
 f1<- function(x) { as.numeric(str_sub(x, 4)) }
 df_frio<-rownames_to_column(df_frio, "lag")%>% mutate_at(1, f1) 
 df_calor<-rownames_to_column(df_calor, "lag")%>% mutate_at(1, f1)  
 (RRVal_lag<-bind_rows(df_frio,df_calor)%>%mutate_at(5,"factor")) # Riesgos relativos y CI por lag
 
 ylab<-pretty(c(RRVal_lag$LowRR,RRVal_lag$HighRR))
 
 RRVal_lag %>% 
   ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
   geom_hline(yintercept = 1, size = 0.5) +
   geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = idE),
                  size = .8, show.legend = FALSE) +
   geom_point(shape = 21, fill = "white", size = 2, aes(colour = idE),
              show.legend = FALSE) +
   scale_x_continuous(breaks = seq(0, 21, 2)) +
   scale_y_continuous(breaks = ylab) +
   scale_colour_manual(values = c( "#4575b4","#d73027")) +
   labs(x = "lag (days)", y = "RR ")+facet_wrap(vars(idE),nrow=2)
 
 #RR total
 RR_overall<-data.frame(RR=predcen$allRRfit,LowRR=predcen$allRRlow,HighRR=predcen$allRRhigh)
 RR_overall<-rownames_to_column(RR_overall, "tmean")%>% mutate_at(1, as.numeric)  # Riesgos relativos y CI por valor de temperatura
 RR_overall[c(5,95),] # En concreto, Riesgos relativos y CI para frío (percentil 5) y calor (percentil 95)
 mmt<-RR_overall$tmean[RR_overall$RR==1] #temperatura de mínima mortalidad
 
 xlab<-pretty(RR_overall$tmean)
 ylab<-pretty(c(RR_overall$LowRR,RR_overall$HighRR))
 RR_overall %>% 
   ggplot(aes(tmean, RR)) + 
   geom_hline(yintercept = 1, size = 0.5) +
   geom_vline(xintercept = RR_overall$tmean[c(5,95)], size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
   geom_ribbon(aes(ymin = LowRR, 
                   ymax = HighRR),
               fill="grey80",alpha=0.7) +
   geom_line(colour="#cb181d",size=1) +
   geom_point(aes(mmt,1),shape = 21, fill = "white", size = 2, colour="#cb181d",
              show.legend = FALSE) +
   scale_x_continuous(breaks = xlab) +
   scale_y_continuous(breaks = ylab) +
   theme_bw() +
   theme(panel.grid.minor = element_blank()) +
   labs(x = "Temperatura media (ºC)", y = "RR",title="Temperatura y Mortalidad total",subtitle="Valencia, 1994-2006") 
 
 