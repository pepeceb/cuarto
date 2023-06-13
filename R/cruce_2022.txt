cruzando<-function(x,y) {
  
    
    
    
    NVDP$FECHA <- as.Date(NVDP$FECHA_DESEMBARQUE, format = "%d/%m/%Y")
    #NVDP2019_SIRENO_IEO_v28032020$FECHA <- dmy (NVDP2019_SIRENO_IEO_v28032020$FECHA_DESEMBARQUE)
    nvdpdata<-data.table(NVDP)

    head (nvdpdata)


   # muestreos<-read_delim("IEOUPMUEDESTOTJLCEBRIAN.TXT", 
                                 #  delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                                  # trim_ws = TRUE)
    
    
    
    
    NVDT<-nvdpdata[, .(PESO=round(sum(PESO,2))), by = c("IDMAREA","FECHA", "FECHA_DESEMBARQUE","PUERTO_DESEMBARQUE",
                                                        "DIVICES",  "LABORATORIO","METIER_IEO","LOA", "ESTRATO_RIM", "SIRENO_SPP", "DIAS_MAREA","ANYO","CODIGO_BUQUE")]
    

    
    
    
    
    
    
    NVDT <- NVDT %>%
      # Rename the columns
      rename(
        #FECHA_DESEM = FECHA_DESEMBARQUE,
        ESTRATO_NVDP= ESTRATO_RIM,
        ESPECIE= SIRENO_SPP
      )
    head (NVDT)
    
    
    
    
    
    NVDT<-NVDT %>%group_by(IDMAREA)%>%
      mutate(PESO_MAREA_DP=sum(PESO))%>%as.data.frame()
    
      fwrite(nvdpdata, "nvdpdata.txt")
    
    
    
  }
