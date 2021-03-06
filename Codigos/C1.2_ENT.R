#Datos desagregados por entidad
#rm(list=ls())
library(survey)
source("D:/MOHOMA/Calculadores/funcs.R")
source("D:/MOHOMA/Calculadores/Tab_2.R")
##########################################################################################
t7<-t7[t7$foliohog%in%1 & t7$ap_1%in%1,]
##########################################################################################
#Las estimaciones corresponden a: "Lectura de medidor", "Cuota fija","No pagan el agua","No sabe")
z<-c("Localidades con menos de 2500 habitantes","Localidades de 2500 o más habitantes")
t7$TOT<-ifelse(!t7$factor%in%NA,t7$factor,0)
t7$TOT_1<-ifelse(t7$ap_1_1%in%1,t7$factor,0)
t7$TOT_2<-ifelse(t7$ap_1_1%in%2,t7$factor,0)
t7$TOT_3<-ifelse(t7$ap_1_1%in%3,t7$factor,0)
t7$TOT_4<-ifelse(t7$ap_1_1%in%9,t7$factor,0)
asp <- svydesign(id=~upm,strata=~EST,data=t7,weights=~1,nest = T)
bla<-Tab_2(TOT,paste0("TOT_",1:4),t7$ENT,estados[-1])

#####################################################################
est<-bla[[1]]
cv<- bla[[2]]
int<-bla[[3]]
se<- bla[[4]]
