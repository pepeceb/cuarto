cruzando<-function(x,y) {
  
   library(data.table) 
    
    
    NVDP$FECHA <- as.Date(NVDP$FECHA_DESEMBARQUE, format = "%d/%m/%Y")

    nvdpdata<-data.table(NVDP)

    head (nvdpdata)


   
    
    
    
    NVDT<-nvdpdata[, (PESO=round(sum(PESO,2))), by = c("IDMAREA","FECHA", "FECHA_DESEMBARQUE","PUERTO_DESEMBARQUE",
                                                        "DIVICES",  "LABORATORIO","METIER_IEO","LOA", "ESTRATO_RIM", "SIRENO_SPP", "DIAS_MAREA","ANYO","CODIGO_BUQUE")]
    

    
    
    
    
    
    
    NVDT <- NVDT %>%
      # Rename the columns
  dplyr:: rename(
        #FECHA_DESEM = FECHA_DESEMBARQUE,
        ESTRATO_NVDP= ESTRATO_RIM,
        ESPECIE= SIRENO_SPP
      )
    head (NVDT)
    
    
    
    
    
    NVDT<-NVDT %>%group_by(IDMAREA)%>%
      mutate(PESO_MAREA_DP=sum(PESO))%>%as.data.frame()
    
      fwrite(nvdpdata, "nvdpdata.txt")
    
    
    
  }
