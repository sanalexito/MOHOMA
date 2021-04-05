#Estas son las funciones que usaremos para calcular las precisiones estadísticas de la información.

Prec<-function(A,B) #A es la variable. B es el diseño, C es por la que desagrego
{
  a<-data.frame(svytotal(~A,B,deff = T))
  x<-svytotal(~A,B,deff = T)
  prec_nac<-data.frame(x[[1]],cv(x)*100,confint(x,level=0.9),a[[2]])
  colnames(prec_nac)<-c("pob_tot","CVpob_tot ","intervalo de","confianza","SE")
  return(prec_nac)
}  

##Prec(t7$TOT,asp)
######################################################################################
#para que jale se pone también el nombre de la base en las variables.
Prec_Des<-function(TOT,asp,D)
{  
  eval(parse(text = paste("
         x<-svyby(~TOT,by=~D,asp,svytotal)
         Prec_Desagregada<-data.frame(x[2],cv(x)*100,confint(x,level=0.9),SE(x))   
         colnames(Prec_Desagregada)<-c(\"pob_Desagregada\",\"CVpob_Desagregada\",\"intervalo de\",\"confianza\",\"SE\")
                          
       return(Prec_Desagregada)
",sep="")))
}

#No jaló definiendo la función de forma usual así que puse una función para cada variable.
#Rara vez aparecen más de once características en un mismo tabulado
for(j in 1:11)
{eval(parse(text = paste(" 
                         Prec_Des",j,"<-function(TOT_",j," ,asp, D){
                         
                         x",j,"<-svyby(~TOT_",j,",by=~D,asp,svytotal)
                         
                         Prec_Des",j,"<-data.frame(x",j,"[2],cv(x",j,")*100,confint(x",j,",level=0.9),SE(x",j,"))
                         colnames(Prec_Des",j,")<-c(\"relativo\",\"CV relativo\",\"intervalo de\",\"confianza\",\"SE\")
                         
                         return(Prec_Des",j,")
                         }
                         ",sep="")))}

##Prec_Des(t7$TOT,asp,t7$DESAG)
######################################################################################
Rel<-function(x,y,asp){
  x<-svyratio(~x,denominator=~y,asp,deff=T)
  Relativo<-data.frame(x[[1]]*100,cv(x)*100,confint(x,level=0.9)*100,SE(x)*100)
  colnames(Relativo)<-c("relativo","CVrelativo","intervalo de","confianza")
  
  return(Relativo) 
}

#######################################################################
#No jaló definiendo la función de forma usual así que puse una función para cada variable.
#Rara vez aparecen más de once características en un mismo tabulado
for(j in 1:11)
{eval(parse(text = paste(" 
Rel_Des",j,"<-function(TOT_",j,", TOT ,asp, D){
x",j,"<-svyby(~TOT_",j,",denominator=~TOT,by=~D,asp,svyratio)
Rel_Des",j,"<-data.frame(x",j,"[2]*100,cv(x",j,")*100,confint(x",j,",level=0.9)*100,SE(x",j,")*100)
colnames(Rel_Des",j,")<-c(\"relativo\",\"CV relativo\",\"intervalo de\",\"confianza\",\"SE\")
return(Rel_Des",j,")
}
",sep="")))}

#####################################################################################################
#Vector con los nombres de las entidades federativas de México
estados <- c("Estados Unidos Mexicanos","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
         "Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Estado de México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León",
         "Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")


