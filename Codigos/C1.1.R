# Tiene lo de la reclasificación de la 5.4

#rm(list=ls())
library(survey)

##########################################################################################
#z<-c("Lectura de medidor", "Cuota fija","No pagan el agua","No sabe")
z<-c("Localidades con menos de 2 500 habitantes","Localidades de 2500 o más habitantes")
t7$TOT<-ifelse(!t7$factor%in%NA & t7$foliohog%in%1,t7$factor,0)
t7$TOT_1<-ifelse(t7$ap_1%in%1 & t7$foliohog%in%1,t7$factor,0)
t7$TOT_2<-ifelse(t7$ap_1%in%2 & t7$foliohog%in%1,t7$factor,0)

asp <- svydesign(id=~UPM_DIS,strata=~EST_DIS,data=t7,weights=~1,nest = T)
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
