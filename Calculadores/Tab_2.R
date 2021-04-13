# Para que este código funcione se deben establecer las etiquetas correspondientes. 
# Se usan las etiquetas del vector "estados" para las desagregaciones por la variable entidad "ENT"
# Para la variable "DESAG" se usan el vector "z" definido en los códigos C1.1.R y C1.2.R
# Yo suelo llamar a la base t7, tal como puede verse a lo largo de los scripts.

Tab_2<-function(TOT,x,D,z) #la variable D es por la que se desagrega.
{
 
  pob<-Prec(t7$TOT,asp)
  for(i in 1:length(x))
    eval(parse(text=paste0("pob_",i,"<-Prec(t7$TOT_",i,",asp) ")))
  
  for(i in 1:length(x))
    eval(parse(text=paste0("rel_pob_",i,"<-Rel(t7$TOT_",i,",t7$TOT,asp) ")))
  #####################################################################
  ent<-Prec_Des(t7$TOT,asp,D) # ent<-Prec_Des(t7$TOT,asp,t7$AMB)
  for(i in 1:length(x))
    eval(parse(text=paste0("ent_",i,"<-Prec_Des",i,"(t7$TOT_",i,",asp,D) ")))
  
  for(i in 1:length(x))
    eval(parse(text=paste0("rel_ent_",i,"<-Rel_Des",i,"(t7$TOT_",i,",t7$TOT,asp,D) ")))
  ##########bloques###################################################################################
  nacional_est<-data.frame(pob[1])
  nacional_cv<-data.frame(pob[2])
  nacional_int<-data.frame(pob[3:4])
  nacional_SE<-data.frame(pob[5])
  
  for(i in 1:length(x))
    eval(parse(text = paste0("
    nacional_est<-data.frame(nacional_est,NA,pob_",i,"[1],rel_pob_",i,"[1])     
    nacional_cv<- data.frame(nacional_cv, NA,pob_",i,"[2],rel_pob_",i,"[2])
    nacional_int<-data.frame(nacional_int,NA,pob_",i,"[3:4],NA,rel_pob_",i,"[3:4])
    nacional_SE<- data.frame(nacional_SE, NA,pob_",i,"[5],rel_pob_",i,"[5])
    ")))
  
  nacional<-data.frame(nacional_est,99997,nacional_cv,99998,nacional_int,99999,nacional_SE)
  #############################################################################################################
  entidad_est<-data.frame(ent[1])
  entidad_cv<-data.frame(ent[2])
  entidad_int<-data.frame(ent[3:4])
  entidad_SE<-data.frame(ent[5])
  
  for(i in 1:length(x))
    eval(parse(text = paste0("
  entidad_est<-data.frame(entidad_est,NA, ent_",i,"[1],rel_ent_",i,"[1])  
  entidad_cv<-data.frame( entidad_cv, NA, ent_",i,"[2],rel_ent_",i,"[2])
  entidad_int<-data.frame(entidad_int,NA, ent_",i,"[3:4],NA,rel_ent_",i,"[3:4])
  entidad_SE<-data.frame( entidad_SE, NA, ent_",i,"[5],rel_ent_",i,"[5])
  "))) 
  entidad<-data.frame(entidad_est,99997,entidad_cv,99998,entidad_int,99999,entidad_SE)
  ##############################################################################################################
  colnames(nacional)<-colnames(entidad)<-seq(1:length(nacional))
  bla<-rbind(nacional,entidad)
  tab<-list()
  et<-c(estados[1],z)
  tab[[1]]<-data.frame(et,bla[,1:(which(bla[1,]%in%99997)-1)])
  tab[[2]]<-data.frame(et,bla[,(which(bla[1,]%in%99997)+1):(which(bla[1,]%in%99998)-1)])
  tab[[3]]<-data.frame(et,bla[,(which(bla[1,]%in%99998)+1):(which(bla[1,]%in%99999)-1)])
  tab[[4]]<-data.frame(et,bla[,(which(bla[1,]%in%99999)+1):dim(bla)[2]])
  
  return(tab)
}
