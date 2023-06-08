muesting <- function(x, COD_TIPO_MUE, COD_ID, ESTRATO_RIM, PUERTO, FECHA, QUARTER, BARCO, ESP_MUE, CATEGORIA, ESP_CAT, P_VIVO,
                     EJEM_MEDIDOS, SOP, TAXON, ESPECIE, MUEST_SP_CAT, MUEST_CAT, EJEM_MEDIDOS_CAT, PESO_SP_CAT, TALLA,
                     EJEM_POND_CAT, PESO_DESEM_TALLA, EJEM_MED_TALLA, PESO_MUEST_TALLA, EJEM_POND_TALLA, EJEM_MED_MAREA,
                     TALLA_MEDIA_MAREA, ObsNumber, MN, MAX) {
  muestreos_tallas$PUERTO<-toupper(stri_trans_general(muestreos_tallas$PUERTO,"Latin-ASCII"))
  
  
  muestreos_tallas$PUERTO<-toupper(stri_trans_general(muestreos_tallas$PUERTO,"Latin-ASCII"))
  
  

  
 
  muestreos_tallas<-data.table(muestreos_tallas)

  muestreos_tallas<-na.omit(muestreos_tallas, cols=c("EJEM_MEDIDOS", "SOP"))
  muestreos_tallas$FECHA_MUE<-as.character (muestreos_tallas$FECHA_MUE)
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ENE", "JAN")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ABR", "APR")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "AGO", "AUG")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "DIC", "DEC")
  muestreos_tallas$FECHA<-dmy(muestreos_tallas$FECHA_MUE)
  muestreos_tallas$QUARTER<-quarter(muestreos_tallas$FECHA)

  
  
  
  
  
  tallas<-muestreos_tallas[,c("CALADERO_DCF",   "COD_ID", "FECHA","QUARTER","ESTRATO_RIM", "PUERTO","COD_TIPO_MUE","BARCO","ESP_MUE", "CATEGORIA",
                              "ESP_CAT","P_MUE_VIVO","P_VIVO", "TALLA", "EJEM_MEDIDOS", "SOP")]%>%as.data.frame()%>%
    distinct()
  tallas<-tallas[complete.cases(tallas[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]

  substring2(tallas$PUERTO, "CILLERO") <- "CELEIRO"

  pesos<-tallas%>%
    group_by(COD_TIPO_MUE,
             COD_ID, ESTRATO_RIM, PUERTO,FECHA,QUARTER,
             BARCO, TAXON=ESP_MUE,CATEGORIA, ESPECIE=ESP_CAT,P_VIVO) %>%
    summarise(
      EJEM_MEDIDOS_CAT=sum(EJEM_MEDIDOS),
      MUEST_SP_CAT= sum(SOP)
    )  %>%
    group_by(COD_ID, TAXON,ESPECIE) %>%
    mutate(
      MUEST_SP=sum(MUEST_SP_CAT)# este es el peso muestreado de la especie esa marea, de todas las categor?as
    )  %>%
    group_by(COD_ID, TAXON, CATEGORIA) %>%
    mutate(
      MUEST_CAT=sum(MUEST_SP_CAT)
    ) %>%
    group_by(COD_ID, ESPECIE) %>%
    mutate(
      PESO_SP_CAT=round((P_VIVO*MUEST_SP_CAT)/MUEST_CAT,2)
    )  %>%
    group_by(COD_TIPO_MUE,COD_ID,  ESTRATO_RIM, PUERTO,FECHA,
             BARCO , TAXON,ESPECIE)%>%
    mutate(
      EJEM_MEDIDOS_SP= sum(EJEM_MEDIDOS_CAT),#EJEMPLARES MEDIDOS DE LA SP EN LA MAREA
      PESO_SP=sum(PESO_SP_CAT), ##ESTE PESO DE LA ESPECIE EN LA MAREA
      PESO_SIRENO= sum(P_VIVO)) #PESO MAL PONDERADO DE SIRENO
  pesos<-pesos[complete.cases(pesos[c("PESO_SP")]),]

  pesos1<-pesos[,c("COD_TIPO_MUE","COD_ID", "FECHA","QUARTER", "ESTRATO_RIM","PUERTO","BARCO",
                   "TAXON", "CATEGORIA", "ESPECIE",
                   "MUEST_SP_CAT", "PESO_SP_CAT", "MUEST_SP", "PESO_SP", "PESO_SIRENO")]
  pesos1<-distinct(pesos1)

  #subset(pesos1, COD_ID== "202200022")%>%as.data.frame()
  tallas1<-distinct(tallas[,c("CALADERO_DCF",   "COD_ID","FECHA", "QUARTER",  "ESTRATO_RIM","PUERTO", "COD_TIPO_MUE",
                              "ESP_MUE","CATEGORIA", "ESP_CAT","TALLA", "EJEM_MEDIDOS")])

  colnames(tallas1)[colnames(tallas1) %in% c("ESP_MUE", "ESP_CAT")] <- c("TAXON", "ESPECIE")
  

  

  tallas1<-tallas1[complete.cases(tallas1[c("EJEM_MEDIDOS")]),]
  

  tallas2<-full_join(pesos1, tallas1)%>%distinct()  %>%
    group_by(COD_ID,TALLA, ESPECIE)%>%
    mutate(
      EJEM_POND_CAT= round((PESO_SP_CAT*EJEM_MEDIDOS/MUEST_SP_CAT),2)
      
    )  %>% group_by(COD_ID,TALLA, ESPECIE)%>%
    mutate(
      EJEM_MED_TALLA=sum(EJEM_MEDIDOS),
      EJEM_POND_TALLA=sum(EJEM_POND_CAT),
      PESO_MUEST_TALLA= sum(MUEST_SP_CAT),
      PESO_DESEM_TALLA = sum (PESO_SP_CAT),
      EJEM_POND_METODOB= round((PESO_DESEM_TALLA*EJEM_MED_TALLA/PESO_MUEST_TALLA),2)
    )  %>%    
    group_by( COD_ID, ESPECIE)  %>%
    mutate(
      EJEM_MED_MAREA=sum(EJEM_MEDIDOS),
      TALLA_MEDIA_MAREA= round (weighted.mean(TALLA, EJEM_POND_TALLA),2))%>%
    
    group_by(COD_ID, ESPECIE, CATEGORIA)%>%
    mutate(TALLA_MEDIA_CAT=round (weighted.mean(TALLA, EJEM_POND_CAT),2))
  tallas2<-tallas2[complete.cases(tallas2[c("PESO_SP")]),]
 
  


  TALLAS<-tallas2[,c( "CALADERO_DCF",   "COD_TIPO_MUE", "COD_ID","FECHA", "QUARTER", "ESTRATO_RIM","PUERTO","BARCO", "TAXON",
                      "ESPECIE", "TALLA_MEDIA_MAREA", "EJEM_MED_MAREA",
                      "PESO_SP")]%>% distinct()%>%as.data.frame()
  

  TALLAS<-subset(TALLAS, EJEM_MED_MAREA>3)
  head (TALLAS,10)

  
}
