  muesting <- function(muestreos_tallas) {
    
library(stringi)    
 #data=muestreos_tallas
  muestreos_tallas$PUERTO<-toupper(stri_trans_general(muestreos_tallas$PUERTO,"Latin-ASCII"))
  
  
  
  muestreos_tallas<-data.table(muestreos_tallas)
  header (muestreos_tallas)
  muestreos_tallas<-na.omit(muestreos_tallas, cols=c("EJEM_MEDIDOS", "SOP"))
  muestreos_tallas$FECHA_MUE<-as.character (muestreos_tallas$FECHA_MUE)
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ENE", "JAN")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ABR", "APR")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "AGO", "AUG")
  muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "DIC", "DEC")
  muestreos_tallas$FECHA<-dmy(muestreos_tallas$FECHA_MUE)
  muestreos_tallas$QUARTER<-quarter(muestreos_tallas$FECHA)
  header(muestreos_tallas)
  
  {
  
  
  tallas<-muestreos_tallas[,c("CALADERO_DCF",   "COD_ID", "FECHA","QUARTER","ESTRATO_RIM", "PUERTO","COD_TIPO_MUE","BARCO","ESP_MUE", "CATEGORIA",
                              "ESP_CAT","P_MUE_VIVO","P_VIVO", "TALLA", "EJEM_MEDIDOS", "SOP")]%>%as.data.frame()%>%
    distinct()
  tallas<-tallas[complete.cases(tallas[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]
  




    
header<-function(x, y) {
    as.data.frame(head(x,2))
  }

header (tallas)
