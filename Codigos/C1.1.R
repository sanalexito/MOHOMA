# Información del Cuadro 1.1 del Apartado A del MOHOMA 2017.

#rm(list=ls())
library(survey)
source("D:/MOHOMA/Calculadores/funcs.R")
source("D:/MOHOMA/Calculadores/Tab_2.R")
##########################################################################################
#Los datos corresponden a "Con servicio de agua" y "Sin servicio de agua", respectivamente.
z<-c("Localidades con menos de 2 500 habitantes","Localidades de 2500 o más habitantes")
t7$TOT<-ifelse(!t7$factor%in%NA & t7$foliohog%in%1,t7$factor,0)
t7$TOT_1<-ifelse(t7$ap_1%in%1 & t7$foliohog%in%1,t7$factor,0)
t7$TOT_2<-ifelse(t7$ap_1%in%2 & t7$foliohog%in%1,t7$factor,0)

asp <- svydesign(id=~folioviv,strata=~EST,data=t7,weights=~1,nest = T)
bla<-Tab_2(TOT,paste0("TOT_",1:2),t7$DESAG,z)

#####################################################################
est<-bla[[1]]
cv<- bla[[2]]
int<-bla[[3]]
se<- bla[[4]]

# En "est" aparecen las estimaciones de totales y los relativos correspondientes
# En "cv" están los coeficientes de variación
# En "int" aparecen los intervalos de confianza
# En "se" se localizan los errores estándar
#########################################################################################
