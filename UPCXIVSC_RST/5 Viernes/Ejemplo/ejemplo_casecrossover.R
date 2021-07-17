##########################################################
### Modelización de regresión de series temporales con R.
### XIV Summer School MESIO UPC-UB
### Práctica Sesión 5: Case-crossover
### Aurelio Tobias
##########################################################

# Load libraries
library(foreign)
library(tidyverse)
library(splines)
library(gnm)

### Example 1. time-series vs time-stratified case-crossover 
############################################################

# load data set from Valencia (Esp) 2000-2006
data <- read.dta("misdatos_valencia.dta")
str(data)
data$tm <- data$tm0 + data$tm1

## 1.1 comparing time-trend fit

# time-series analysis
spl <- ns(data$date, df=42)
model.ts0 <- glm(tm ~ spl, data=data, family=quasipoisson)
pred.ts0 <- predict(model.ts0, type="response")

# time-stratified case-crossover 
data$month    <- as.factor(months(data$date))
data$year     <- as.factor(format(data$date, format="%Y") )
data$dow      <- as.factor(weekdays(data$date))
data$stratum2 <- as.factor(data$year:data$month)
model.cc0 <- glm(tm ~ stratum2, data=data, family=quasipoisson)
pred.cc0 <- predict(model.cc0, type="response")

# comparison
plot(data$date, data$tm, pch=19, cex=0.2, col=grey(0.6), ylab="Num. deaths", xlab="Date")
lines(data$date, pred.cc0, lwd=2, col="blue")
lines(data$date, pred.ts0, lwd=2, col="red")

### Example 2. Comparing health effects of TSP
##############################################

# time-series
model.ts1 <- glm(tm ~ tsp + spl + dow + hol, data=data, family=quasipoisson)
cbind(model.ts1$coefficients[2], sqrt(summary(model.ts1)$cov.scaled[2,2]))

# case-crossover with Poisson regression fitting 3-way interaction
start1 <- Sys.time()
model.cco1 <- glm(tm ~ tsp + hol + factor(year)*factor(month)*factor(dow), data=data, family=quasipoisson)
end1 <- Sys.time()
summary(model.cco1)

data$stratum3 <- as.factor(data$year:data$month:data$dow)
start2 <- Sys.time()
model.cco2 <- glm(tm ~ tsp + hol + stratum3 , data=data, family=quasipoisson)
end2 <- Sys.time()
summary(model.cco2)

# case-crossover with conditional Poisson
start3 <- Sys.time()
model.cco3 <- gnm(tm ~ tsp + hol , data=data, family=quasipoisson, eliminate=factor(stratum3))
end3 <- Sys.time()
summary(model.cco3)

# comparison within case-crossover approaches
cbind(model.cco1$coefficients[2], sqrt(summary(model.cco1)$cov.scaled[2,2]), end1-start1)
cbind(model.cco2$coefficients[2], sqrt(summary(model.cco2)$cov.scaled[2,2]), end2-start2)
cbind(model.cco3$coefficients[1], sqrt(summary(model.cco3)$cov.scaled[1,1]), end3-start3)

# overall comparison between time-series vs case-crossover
# time-series
cbind((exp(model.ts1$coefficients[2]*10)-1)*100, 
      (exp((model.ts1$coefficients[2]-1.96*sqrt(summary(model.ts1)$cov.scaled[2,2]))*10)-1)*100, 
      (exp((model.ts1$coefficients[2]+1.96*sqrt(summary(model.ts1)$cov.scaled[2,2]))*10)-1)*100)
# case-crossover
cbind((exp(model.cco3$coefficients[1]*10)-1)*100, 
      (exp((model.cco3$coefficients[1]-1.96*sqrt(summary(model.cco3)$cov.scaled[1,1]))*10)-1)*100, 
      (exp((model.cco3$coefficients[1]+1.96*sqrt(summary(model.cco3)$cov.scaled[1,1]))*10)-1)*100)

### Example 3. time-stratified case-crossover by age
####################################################

# reshape dataset from wide to long format
data.long <- pivot_longer(data, tm0:tm1, names_to = "age", values_to = "tm_age") %>%
  mutate(age=str_remove(age,"tm") %>% as.factor)
str(data.long)

# overall effect of tsp 
(exp(model.cco3$coefficients[1]*10)-1)*100

# age as confounder
model.age1 <- gnm(tm_age ~ tsp + hol + age, data=data.long, family=quasipoisson, eliminate=factor(stratum3))
summary(model.age1)
(exp(model.age1$coefficients[1]*10)-1)*100

data.long$stratum4 <- as.factor(data.long$year:data.long$month:data.long$dow:data.long$age)
model.age2 <- gnm(tm_age ~ tsp + hol , data=data.long, family=quasipoisson, eliminate=factor(stratum4))
summary(model.age2)
(exp(model.age2$coefficients[1]*10)-1)*100

# age as effect modifier from model with interaction
model.age3 <- gnm(tm_age ~ tsp*age + hol, data=data.long, family=quasipoisson, eliminate=factor(stratum4))
summary(model.age3)
(exp(model.age3$coefficients[1]*10)-1)*100
(exp((model.age3$coefficients[1]+model.age3$coefficients[4])*10)-1)*100

# age as effect modifier from specific-stratum models 
# age <65 years
data.gage0 <- data[which(data.long$age==0),] 
model.gage0 <- gnm(tm ~ tsp + hol , data=data.gage0, family=quasipoisson, eliminate=factor(stratum3))
(exp(model.gage0$coefficients[1]*10)-1)*100
# age >65 years
data.gage1 <- data[which(data.long$age==1),] 
model.gage1 <- gnm(tm ~ tsp + hol , data=data.gage1, family=quasipoisson, eliminate=factor(stratum3))
(exp(model.gage1$coefficients[1]*10)-1)*100

### Example 4. time-stratified case-crossover by heatwaves
############################################################

# define heatwave based as the 95th centile of the mean temperature distribution
q95 <- quantile(x=data$tmean, probs=c(.95))
data$hw <- as.factor(ifelse(data$tmean > q95, 1, 0))
table(data$hw)

# overall effect of tsp 
(exp(model.cco3$coefficients[1]*10)-1)*100

# heatwave as confounder
model.hw1 <- gnm(tm ~ tsp + hol + hw, data=data, family=quasipoisson, eliminate=factor(stratum3))
summary(model.hw1)
(exp(model.hw1$coefficients[1])-1)*100

data$stratum4 <- as.factor(data$year:data$month:data$dow:data$hw)
model.hw2 <- gnm(tm ~ tsp + hol, data=data, family=quasipoisson, eliminate=factor(stratum4))
summary(model.hw2)
(exp(model.hw2$coefficients[1])-1)*100

# heatwave as effect modifier from model with interaction
model.hw3 <- gnm(tm ~ tsp*hw + hol, data=data, family=quasipoisson, eliminate=factor(stratum4))
summary(model.hw3)
(exp(model.hw3$coefficients[1])-1)*100
(exp(model.hw3$coefficients[1]+model.hw3$coefficients[4])-1)*100

# heatwave as effect modifier from specific-stratum models 
# without heatwaves
data.ghw0 <- data[which(data$hw==0),] 
model.ghw0 <- gnm(tm ~ tsp + hol , data=data.ghw0, family=quasipoisson, eliminate=factor(stratum3))
(exp(model.ghw0$coefficients[1])-1)*100
# with heatwaves
data.ghw1 <- data[which(data$hw==1),] 
model.ghw1 <- gnm(tm ~ tsp + hol , data=data.ghw1, family=quasipoisson, eliminate=factor(stratum3))
(exp(model.ghw1$coefficients[1])-1)*100


##########################################################
### End of example
##########################################################
