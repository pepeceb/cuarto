cruzando<-function(x,y) {
  
   library(data.table) 
   # NVDP<-fread("NVDP2022_SIRENO.txt")
    
    NVDP$FECHA <- as.Date(NVDP$FECHA_DESEMBARQUE, format = "%d/%m/%Y")  
    
NVDT<-NVDP%>%group_by (IDMAREA,FECHA, FECHA_DESEMBARQUE,PUERTO_DESEMBARQUE,
                  DIVICES,  LABORATORIO, ESTRATO_RIM,
                  SIRENO_SPP, ANYO,CODIGO_BUQUE)%>%
 dplyr::summarise(PESO=sum(PESO))%>%as.data.table()
    
NVDT <- NVDT %>%
  # Rename the columns
 dplyr:: rename(
    #FECHA_DESEM = FECHA_DESEMBARQUE,
    ESTRATO_NVDP= ESTRATO_RIM,
    ESPECIE= SIRENO_SPP
  )   
    
    

    
  NVDT<-NVDT %>%group_by(IDMAREA)%>%
  dplyr:: mutate(PESO_MAREA_DP=sum(PESO))%>%as.data.frame()


  
  
  library(readxl)
CFPO <- read_excel("CFPO.xlsx")


CFPO <- CFPO %>%
  # Rename the columns
 dplyr:: rename(
    CODSGPM= CODIGOBUQUE,
  )%>%as.data.table()

  
  CFPO1<-CFPO[, c("CODIGO_BUQUE",  "NOMBRE_BUQUE", "CODSGPM", "MODALIDAD")]
CFPO1<-unique( CFPO1)
  CFPO1$CODSGPM<-as.factor(CFPO1$CODSGPM)
DTCFPO<- data.table(CFPO1)
rm(CFPO1)
  NVDT<-dplyr::left_join(NVDT,DTCFPO)
          fwrite(NVDT, "NVDT.txt", bom=TRUE)
  
  
  
  
  library(readr)
  

  
muestreos[,"P_VIVO"][is.na(muestreos[,"P_VIVO"])]<- 0
  muestreos$FECHA_MUE<-toupper (muestreos$FECHA_MUE)
muestreos_22<- muestreos
muestreos_22$FECHA_MUE<-as.character (muestreos_22$FECHA_MUE)
muestreos_22$FECHA_MUE<-str_replace_all(muestreos_22$FECHA_MUE, "ENE", "JAN")
muestreos_22$FECHA_MUE<-str_replace_all(muestreos_22$FECHA_MUE, "ABR", "APR")
muestreos_22$FECHA_MUE<-str_replace_all(muestreos_22$FECHA_MUE, "AGO", "AUG")
muestreos_22$FECHA_MUE<-str_replace_all(muestreos_22$FECHA_MUE, "DIC", "DEC")
muestreos_22$FECHA<-dmy(muestreos_22$FECHA_MUE)
muestreos_22$QUARTER<-quarter(muestreos_22$FECHA)
ID_RIM<-paste(muestreos_22$FECHA,muestreos_22$CODSGPM)
muestreos_22<-cbind(ID_RIM, muestreos_22)  #Unimos este nuevo vector a la matriz muestreos
muestreos_22$ID_RIM<-as.factor(muestreos_22$ID_RIM)
levels(muestreos_22$ID_RIM) <- c(1:length(unique(muestreos_22$ID_RIM)))#Se numeran las mareas
muestreos_22 <- muestreos_22[order(muestreos_22$ID_RIM),]



muestreos_22$FECHA_MENOS1 <- as.Date(muestreos_22$FECHA-1, format = "%d/%m/%Y")
muestreos_22$FECHA_MAS1 <- as.Date(muestreos_22$FECHA+1, format = "%d/%m/%Y")
muestreos_22$FECHA_MENOS2 <- as.Date(muestreos_22$FECHA-2, format = "%d/%m/%Y")
muestreos_22$FECHA_MENOS3 <- as.Date(muestreos_22$FECHA-3, format = "%d/%m/%Y")
muestreos_22$FECHA_MENOS4 <- as.Date(muestreos_22$FECHA-4, format = "%d/%m/%Y")
muestreos_22$FECHA_MENOS5 <- as.Date(muestreos_22$FECHA-5, format = "%d/%m/%Y")
muestreos_22$FECHA_MAS2 <- as.Date(muestreos_22$FECHA+2, format = "%d/%m/%Y")
muestreos_22$FECHA_MAS3 <- as.Date(muestreos_22$FECHA+3, format = "%d/%m/%Y")
muestreos_22$FECHA_MAS4 <- as.Date(muestreos_22$FECHA+4, format = "%d/%m/%Y")

  
 #muestreos_22<-muestreos_22[complete.cases(muestreos_22[c("P_VIVO")]),]

  
 muestreos2<-muestreos_22  %>%
  group_by(COD_ID,ID_RIM,FECHA_MUE, FECHA,FECHA_MENOS1,FECHA_MENOS2,FECHA_MENOS3,
                                 FECHA_MENOS4,FECHA_MENOS5,FECHA_MAS1,FECHA_MAS2,FECHA_MAS3,
                                 COD_TIPO_MUE,  ESTRATO_RIM, METIER_DCF, PUERTO, CODSGPM,BARCO,ESP_MUE)%>%
  dplyr::summarise(PESO_SP= round(sum(P_VIVO,2)))%>%as.data.table()%>%
  unique()
  
  
  
#head (as.data.frame(muestreos2))
#muestreos2[,c("PESO_MAREA"):=list(sum(PESO_SP)),by=COD_ID]
  muestreos2<-muestreos2%>%group_by(COD_ID)%>%
  dplyr:: mutate(PESO_MAREA= sum(PESO_SP))
#muestreos2[,c("RATIO"):=list((PESO_SP/PESO_MAREA)*100),by=COD_ID]
 muestreos2<-muestreos2 %>% group_by(COD_ID)%>%
  dplyr:: mutate(RATIO= (PESO_SP/PESO_MAREA)*100)
muestreos2$RATIO<-round(muestreos2$RATIO,1)
#head(as.data.frame(muestreos2))
  
     fwrite(muestreos2, "muestreos2.txt", bom=TRUE) 
  
  library(tidyr)
  muestreos3<- tidyr::gather (muestreos2,"TIPO_FECHA", "FECHA", 4:12)%>%
  dplyr::mutate(ORDEN=TIPO_FECHA)%>%
  select(ID_RIM,COD_ID,  CODSGPM, BARCO ,FECHA_MUE,
         FECHA, TIPO_FECHA, ORDEN, ESTRATO_RIM, METIER_DCF, PUERTO,
         COD_TIPO_MUE,ESP_MUE, PESO_MAREA, PESO_SP, RATIO )
  muestreos3$ESPECIE<-muestreos3$ESP_MUE
muestreos3<- arrange (muestreos3,FECHA)
muestreos3 <- distinct (muestreos3)
  
library(plyr)  
  muestreos3$ORDEN<-plyr::revalue(muestreos3$ORDEN, c("FECHA"="1", "FECHA_MENOS1"="2","FECHA_MAS1"="3",
                                                    "FECHA_MENOS2"="4" ,  "FECHA_MAS2"="6",
                                                    "FECHA_MENOS3" = "5","FECHA_MENOS4" = "7",
                                                    "FECHA_MENOS5"="8","FECHA_MAS3"="9"))
  
  
  
  muestreos3$CODSGPM<-as.factor(muestreos3$CODSGPM)

NVDT$CODSGPM<-as.factor(NVDT$CODSGPM)

{
cruce1 <-  full_join(muestreos3,NVDT, by=c("FECHA", "CODSGPM")) %>%
  arrange ( ORDEN,-RATIO) %>%
 dplyr:: mutate(ORDEN2 = ifelse(duplicated(IDMAREA),ORDEN=="10", ORDEN),
         ORDEN3 = ifelse(is.na(IDMAREA),"NULL","OK")) %>%distinct()%>%
  arrange(ID_RIM, ORDEN)%>%as.data.frame()
  }

cruce1[,"PESO_SP"][is.na(cruce1[,"PESO_SP"])]<- 0
  
 cruce1<-cruce1%>%dplyr::select(ANYO,ID_RIM,IDMAREA,COD_ID,PUERTO,BARCO, NOMBRE_BUQUE,CODSGPM,ESTRATO_RIM, ESTRATO_NVDP,
                        DIVICES,LABORATORIO, COD_TIPO_MUE,FECHA_MUESTREO=FECHA_MUE,FECHA ,TIPO_FECHA,ORDEN , ORDEN2,ORDEN3,
                        FECHA_DESEMBARQUE, PUERTO_DESEMBARQUE,ESP_MUE,PESO_SP,RATIO, PESO_MAREA,PESO_MAREA_DP )%>%distinct()
#cruce1<-cruce1[complete.cases(cruce1[c("COD_ID")]),]#Elimina los registros 
  
  
  
  cruce1<-cruce1%>%group_by(COD_ID)%>%
  dplyr::mutate(
  ORDEN4=ifelse(any(ORDEN3=="OK"),"cruza", "NO_cruza"))%>%
  arrange(ID_RIM, ORDEN, -RATIO) %>%as.data.frame()
as.data.frame(head (cruce1,20))

     fwrite(cruce1, "cruce1.txt", bom=TRUE)  
  
no_cruzan<-subset(cruce1, ORDEN4=="NO_cruza")%>%select(FECHA_MUESTREO,ID_RIM, IDMAREA,COD_ID,CODSGPM,COD_TIPO_MUE, PUERTO,BARCO,
                                      ESTRATO_RIM
                                              )%>%distinct()%>%arrange( ESTRATO_RIM, BARCO)  
      fwrite(no_cruzan, "NO_CRUZAN.txt", bom=TRUE) 
  
  library(openxlsx)
  write.xlsx(no_cruzan, no_cruzan.xlsx')
  
  
  
  cruce2<-cruce1 %>%
  group_by(COD_ID, ORDEN) %>%
  dplyr::slice(which.max(RATIO))%>%arrange(ID_RIM) %>%as.data.frame()
  
cruce2<-subset(cruce2,!ORDEN2==FALSE & ORDEN3=="OK")%>%
  arrange(ID_RIM)%>%
  dplyr::mutate(dif=abs(PESO_MAREA-PESO_MAREA_DP))  
  
  
  no_cruzan<-subset(cruce1, ORDEN4=="NO_cruza")%>%
  dplyr::select(FECHA_MUESTREO,ID_RIM, IDMAREA,COD_ID,CODSGPM,COD_TIPO_MUE, PUERTO,BARCO,
                                      ESTRATO_RIM
                                              )%>%distinct()%>%arrange( ESTRATO_RIM, BARCO)
   fwrite(cruce2, "cruce2.txt") 
  

  
  
  cruce3<-cruce2 %>%
  group_by(COD_ID) %>%
  top_n(n =-2, wt = ORDEN2)  %>%
  arrange(ID_RIM)%>%group_by(COD_ID) %>%
  top_n(n =-1, wt = dif)%>%group_by(COD_ID) %>%
dplyr::  slice(which.min(ORDEN))%>%
  as.data.frame()
  
 
   cruce3[,"PESO_SP"][is.na(cruce3[,"PESO_SP"])]<- 0
     fwrite(cruce3, "cruce3.txt", bom=TRUE) 
  
 # cruce3<-cruce3 %>%dplyr::select(ANYO,ID_RIM,IDMAREA,COD_ID,PUERTO, PUERTO_DESEMBARQUE,BARCO, NOMBRE_BUQUE,CODSGPM,
  #                                ESTRATO_RIM, ESTRATO_NVDP,
   #                               DIVICES,LABORATORIO, COD_TIPO_MUE,FECHA_DESEMBARQUE,FECHA_MUESTREO,
    #                              FECHA ,TIPO_FECHA,ORDEN , ESP_MUE,PESO_SP,RATIO, PESO_MAREA,PESO_MAREA_DP )%>%distinct()
  
  
  
  export_cruce<-cruce3[,c(
  "ANYO"          , "ID_RIM"              ,"COD_ID"        ,  "IDMAREA"      ,
  "PUERTO"        , "PUERTO_DESEMBARQUE" ,"BARCO"          ,  "NOMBRE_BUQUE"      ,
  "CODSGPM"            ,"ESTRATO_RIM"    ,  "ESTRATO_NVDP"      ,
  "DIVICES"       , "LABORATORIO"        , "COD_TIPO_MUE"  ,  "FECHA_DESEMBARQUE" ,
  "FECHA_MUESTREO", "FECHA"              , "TIPO_FECHA"    ,  "ORDEN"             ,
  "ESP_MUE"       , "PESO_SP"            , "RATIO"         ,  "PESO_MAREA"        ,
  "PESO_MAREA_DP" , "dif" )]%>%  arrange(ID_RIM)

#head (export_cruce)

  library(openxlsx)
  write.xlsx(export_cruce, export_cruce.xlsx')
  
 fwrite(export_cruce, "EXPORT_CRUCE.txt", bom=TRUE) 
return(export_cruce) 
    
  }
