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
    ESPECIE= SIRENO_SPP,
    PESO= V1
  )
    head (NVDT)
    
    
    
    
    
    
      fwrite(NVDT, "NVDT.txt")
    
    
    
  }
