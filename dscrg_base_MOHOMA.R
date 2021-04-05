
#Este script sirve para descargar las bases de datos del módulo de Hogares y Medio Ambiente
#de la encuesta Nacional de Hogares que vienen en ZIP. Sirve para preparar las variables que se 
#usan en los cálculos.

url<-"https://www.inegi.org.mx/contenidos/programas/mohoma/2017/microdatos/bd_mohoma_2017_csv.zip"
download.file(url,destfile = "D:/MOMOHA/dataset.zip",mode = "wb")
unzip(zipfile = "D:/MOMOHA/dataset.zip", exdir = "D:/MOMOHA/Bases_mohoma")

#Se cargan las bases que vienen en formato CSV
t7<-read.csv("D:/MOMOHA/bases_mohoma/mohoma.csv")
tviv<-read.csv("D:/MOMOHA/bases_mohoma/vivienda.csv")

#Se lleva la variable upm a la tabla que yo llamé t7
t7<-merge(t7,tviv[,c("folioviv","upm")])

#Ahora se define la variable entidad si es que se utiliza.
t7$entidad<-ifelse(nchar(t7$folioviv)==9, substr(t7$folioviv,1,1),substr(t7$folioviv,1,2))
for(i in 1:32){
t7[t7$entidad%in%i,"ENT"]<-i}

t7$UPM<-t7$upm #esta es una copia de la variable upm

#Se arma la varible estrato y se reclasifica como Rural y Urbano
t7$EST<-substr(t7$folioviv,nchar(t7$folioviv)-7,nchar(t7$folioviv)-7)
t7$EST<-ifelse(t7$EST%in%6,"Rural","Urbano")

#DESAG es la variable por la cual se desagrega 
#1: localidades con menos de 2500 habitantes; 2: Localidades con más de 2500 habitantes
t7[t7$tam_loc%in%4,"DESAG"]<-1
t7[t7$tam_loc%in%c(1,2,3),"DESAG"]<-2

#Hogares principales con servicio de agua
#t7[t7$foliohog%in%1 & t7$ap_1%in%1,]
