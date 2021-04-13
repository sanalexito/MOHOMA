# Tiene lo de la reclasificación de la 5.4

#rm(list=ls())
library(survey)
source("D:/MOMOHA/Calculadores/funcs.R")
source("D:/MOMOHA/Calculadores/Tab_2.R")

##########################################################################################
t7<-t7[t7$foliohog%in%1 & t7$ap_1%in%1,]
##########################################################################################
#z<-c("Lectura de medidor", "Cuota fija","No pagan el agua","No sabe")
z<-c("Localidades con menos de 2500 habitantes","Localidades de 2500 o más habitantes")
acciones<-c("Sabor, olor, claridad del agua suministrada","Confianza en relación con la salud",
            "Presión del suministro","Continuidad en el suministro del líquido",
            "Monto cobrado","Medición del consumo de agua","Atención al cliente en oficinas",
            "Reparación de fugas")
#Datos nacionales
aa<-c(1:3,9)
t7$TOT<-ifelse(!t7$factor%in%NA,t7$factor,0)
p1<-eval(parse(text = paste0('t7$ap_1_4_',1:8,'%in%1',collapse = "|")))
p2<-eval(parse(text = paste0('t7$ap_1_4_',1:8,'%in%2',collapse = "|")))
p3<-eval(parse(text = paste0('t7$ap_1_4_',1:8,'%in%3',collapse = "|")))
p4<-eval(parse(text = paste0('t7$ap_1_4_',1:8,'%in%9',collapse = "|")))

t7$TOT_1<-ifelse(p1%in%T,t7$factor,0)
t7$TOT_2<-ifelse(p2%in%T,t7$factor,0)
t7$TOT_3<-ifelse(p3%in%T,t7$factor,0)
t7$TOT_4<-ifelse(p4%in%T,t7$factor,0)
asp <- svydesign(id=~folioviv,strata=~EST,data=t7,weights=~1,nest = T)
bla<-Tab_2(TOT,paste0('TOT_',1:4),t7$DESAG,z)

#Desagregaciones
for(j in 1:8)
for(i in 1:length(aa))
  eval(parse(text = paste0("
           t7$TOT_",i,"<-ifelse(t7$ap_1_4_",j,"%in%aa[i],t7$factor,0)
  
asp <- svydesign(id=~folioviv,strata=~EST,data=t7,weights=~1,nest = T)
bla_",j,"<-Tab_2(TOT,paste0('TOT_',1:4),t7$DESAG,z)
")))

#########################################################################################
x<-list()
x[[1]]<-rbind(bla[[1]][1,],bla_1[[1]][1,],bla_2[[1]][1,],bla_3[[1]][1,],
              bla_4[[1]][1,], bla_5[[1]][1,], bla_6[[1]][1,],bla_7[[1]][1,],bla_8[[1]][1,])

rownames(x[[1]])<-c(estados[1],acciones)
x[[1]][,1]<-rownames(x[[1]])
rownames(x[[1]])<-NULL
indice<-x[[1]][c( 1, order(x[[1]][2:dim(x[[1]])[1],11],decreasing = TRUE)+1),]
ordenn<-rownames(indice)
x[[1]]<-x[[1]][ordenn,]
x[[1]][c(2:dim(x[[1]])[1]),2:3]<-NA
x[[1]][1,4:dim(x[[1]])[2]]<-NA

#[c( 1, order(x[[1]][2:12,5],decreasing = TRUE)+1,13),]

y<-list()
for(i in 2:3)
  eval(parse(text = paste0("
      y[[i]]<-rbind(bla[[1]][i,],bla_1[[1]][i,],bla_2[[1]][i,],bla_3[[1]][i,],bla_4[[1]][i,], bla_5[[1]][i,], bla_6[[1]][i,],
      bla_7[[1]][i,],bla_8[[1]][i,]) 
      rownames(y[[i]])<-c(z[i-1],acciones) 
      y[[i]][,1]<-rownames(y[[i]])
      rownames(y[[i]])<-NULL
      #y[[i]][,5]<-as.numeric(y[[i]][,5])
      indice",i,"<-y[[i]][c( 1, order(y[[i]][2:(dim(y[[i]])[1]),11],decreasing = TRUE)+1),]
      orden",i,"<-rownames(indice",i,")
      y[[i]]<-y[[i]][orden",i,",]
      y[[i]][c(2:dim(y[[i]])[1]),2:3]<-NA
      y[[i]][1,4:dim(y[[i]])[2]]<-NA
      #y[[i]]<-y[[i]][1:6,]
      y[[i]]<-rbind(y[[i]],NA)
      ")))

y<-do.call(rbind,y)
colnames(x[[1]])<-colnames(y)
est<-rbind(x[[1]],NA,y)
est<-est[-dim(est)[1],]
########################### CV #################################################
x<-list()
x[[1]]<-rbind(bla[[2]][1,],bla_1[[2]][1,],bla_2[[2]][1,],bla_3[[2]][1,],bla_4[[2]][1,], bla_5[[2]][1,], bla_6[[2]][1,],
              bla_7[[2]][1,],bla_8[[2]][1,])
rownames(x[[1]])<-c(estados[1],acciones)
x[[1]][,1]<-rownames(x[[1]])
rownames(x[[1]])<-NULL
x[[1]]<-x[[1]][ordenn,]
x[[1]][c(2:dim(x[[1]])[1]),2:3]<-NA
x[[1]][1,4:5]<-NA



y<-list()
for(i in 2:3)
  eval(parse(text = paste0("
                           y[[i]]<-rbind(bla[[2]][i,],bla_1[[2]][i,],bla_2[[2]][i,],bla_3[[2]][i,],bla_4[[2]][i,], bla_5[[2]][i,], bla_6[[2]][i,],
                           bla_7[[2]][i,],bla_8[[2]][i,]) 
                           rownames(y[[i]])<-c(z[i-1],acciones) 
                           y[[i]][,1]<-rownames(y[[i]])
                           rownames(y[[i]])<-NULL
                           # y[[i]][,5]<-as.numeric(y[[i]][,5])
                           y[[i]]<-y[[i]][orden",i,",]
                           y[[i]][c(2:dim(y[[i]])[1]),2:3]<-NA
                           y[[i]][1,4:dim(y[[i]])[2]]<-NA
                           # y[[i]]<-y[[i]][1:6,]
                           y[[i]]<-rbind(y[[i]],NA)
                           ")))

y<-do.call(rbind,y)
colnames(x[[1]])<-colnames(y)
cv<-rbind(x[[1]],NA,y)
cv<-cv[-dim(cv)[1],]

################################ INT ###############################################
x<-list()
x[[1]]<-rbind(bla[[3]][1,],bla_1[[3]][1,],bla_2[[3]][1,],bla_3[[3]][1,],bla_4[[3]][1,], bla_5[[3]][1,], bla_6[[3]][1,],
              bla_7[[3]][1,],bla_8[[3]][1,])
rownames(x[[1]])<-c(estados[1],acciones)
x[[1]][,1]<-rownames(x[[1]])
rownames(x[[1]])<-NULL
x[[1]]<-x[[1]][ordenn,]
x[[1]][c(2:dim(x[[1]])[1]),2:3]<-NA
x[[1]][1,4:dim(x[[1]])[2]]<-NA



y<-list()
for(i in 2:3)
  eval(parse(text = paste0("
                           y[[i]]<-rbind(bla[[3]][i,],bla_1[[3]][i,],bla_2[[3]][i,],bla_3[[3]][i,],bla_4[[3]][i,], bla_5[[3]][i,], bla_6[[3]][i,],
                           bla_7[[3]][i,],bla_8[[3]][i,]) 
                           rownames(y[[i]])<-c(z[i-1],acciones) 
                           y[[i]][,1]<-rownames(y[[i]])
                           rownames(y[[i]])<-NULL
                           # y[[i]][,5]<-as.numeric(y[[i]][,5])
                           y[[i]]<-y[[i]][orden",i,",]
                           y[[i]][c(2:dim(y[[i]])[1]),2:3]<-NA
                           y[[i]][1,4:dim(y[[i]])[2]]<-NA
                           # y[[i]]<-y[[i]][1:6,]
                           y[[i]]<-rbind(y[[i]],NA)
                           ")))


y<-do.call(rbind,y)
colnames(x[[1]])<-colnames(y)
int<-rbind(x[[1]],NA,y)
int<-int[-dim(int)[1],]

########################### SE #################################################
x<-list()
x[[1]]<-rbind(bla[[4]][1,],bla_1[[4]][1,],bla_2[[4]][1,],bla_3[[4]][1,],bla_4[[4]][1,], bla_5[[4]][1,], bla_6[[4]][1,],
              bla_7[[4]][1,],bla_8[[4]][1,])
rownames(x[[1]])<-c(estados[1],acciones)
x[[1]][,1]<-rownames(x[[1]])
rownames(x[[1]])<-NULL
x[[1]]<-x[[1]][ordenn,]
x[[1]][c(2:dim(x[[1]])[1]),2:3]<-NA
x[[1]][1,4:dim(x[[1]])[2]]<-NA


y<-list()
for(i in 2:3)
  eval(parse(text = paste0("
                           y[[i]]<-rbind(bla[[4]][i,],bla_1[[4]][i,],bla_2[[4]][i,],bla_3[[4]][i,],bla_4[[4]][i,], bla_5[[4]][i,], bla_6[[4]][i,],
                           bla_7[[4]][i,],bla_8[[4]][i,]) 
                           rownames(y[[i]])<-c(z[i-1],acciones) 
                           y[[i]][,1]<-rownames(y[[i]])
                           rownames(y[[i]])<-NULL
                           # y[[i]][,5]<-as.numeric(y[[i]][,5])
                           y[[i]]<-y[[i]][orden",i,",]
                           y[[i]][c(2:dim(y[[i]])[1]),2:3]<-NA
                           y[[i]][1,4:dim(y[[i]])[2]]<-NA
                           #y[[i]]<-y[[i]][1:6,]
                           y[[i]]<-rbind(y[[i]],NA)
                           ")))

y<-do.call(rbind,y)
colnames(x[[1]])<-colnames(y)
se<-rbind(x[[1]],NA,y)
se<-se[-dim(se)[1],]
openxlsx::write.xlsx(est,"D:/MOMOHA/Tablas/C1.4.xlsx",asTable = T)

