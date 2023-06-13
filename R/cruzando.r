cruzando<-function(x,y) {
  
   library(data.table) 
    NVDP<-fread("NVDP2022_SIRENO.txt")
    
    NVDP$FECHA <- as.Date(NVDP$FECHA_DESEMBARQUE, format = "%d/%m/%Y")  
    
NVDT<-NVDP%>%group_by (IDMAREA,FECHA, FECHA_DESEMBARQUE,PUERTO_DESEMBARQUE,
                  DIVICES,  LABORATORIO,METIER_IEO,LOA, ESTRATO_RIM,
                  SIRENO_SPP, DIAS_MAREA,ANYO,CODIGO_BUQUE)%>%
 dplyr::summarise(PESO=sum(PESO))%>%as.data.table()
    

    
    
    
    
    
    
NVDT <- NVDT %>%
  # Rename the columns
  rename(
    #FECHA_DESEM = FECHA_DESEMBARQUE,
    ESTRATO_NVDP= ESTRATO_RIM,
    ESPECIE= SIRENO_SPP
  )
    head (NVDT)
    
    
    
    
    
    
      fwrite(NVDT, "NVDT.txt")
    
  NVDT<-NVDT %>%group_by(IDMAREA)%>%
  mutate(PESO_MAREA_DP=sum(PESO))%>%as.data.frame()
  library(readxl)
CFPO <- read_excel("CFPO.xlsx")


CFPO <- CFPO %>%
  # Rename the columns
  rename(


    CODSGPM= CODIGOBUQUE,
  )
CFPO<-data.table(CFPO)
  
  CFPO1<-CFPO[, c("CODIGO_BUQUE",  "NOMBRE_BUQUE", "CODSGPM", "MODALIDAD")]
CFPO1<-unique( CFPO1)
  CFPO1$CODSGPM<-as.factor(CFPO1$CODSGPM)
DTCFPO<- data.table(CFPO1)
rm(CFPO1)
  NVDT<-dplyr::left_join(NVDT,DTCFPO)
  library(readr)
  
  muestreos <- read_delim("~/2023/cruce/IEOUPMUEDESTOTJLCEBRIAN.TXT", 
                               delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                               trim_ws = TRUE)
  
muestreos[,"P_VIVO"][is.na(muestreos[,"P_VIVO"])]<- 0
muestreos_22<- muestreos[complete.cases(muestreos[c(  "P_VIVO")]),]
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
  
  
    
  }
