library(dplyr)


modelando <- function(x, y) {

  

  mod <- lm(TALLA_MEDIA_MAREA~ESTRATO_RIM*ESPECIE, data=TALLAS)
 
  cooksd <- cooks.distance(mod)
  
  
  
  cooksd2<-as.data.frame(cooksd)   #anadimos numero de observacion para cruzarlas
  
  
  cooksd2$ObsNumber <- 1:length(cooksd)
  TALLAS$ObsNumber <- 1:length(cooksd)
 
 
  sp2<-dplyr::full_join(TALLAS, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
    arrange(ObsNumber)%>%as.data.frame()
  
  sp2<-sp2[complete.cases(sp2[c("cooksd")]),]
  
  dMean <- sp2 %>%
    group_by(ESPECIE, ESTRATO_RIM) %>%
    dplyr::summarise(MN = mean(cooksd))%>%arrange(-MN)%>%as.data.frame()
  
  dMean<-dMean[complete.cases(dMean[c("MN")]),]
 
  sp3<-left_join(sp2, dMean)%>%unique()%>%arrange(FECHA)%>%as.data.frame()
  
  
  sp3<-sp3[complete.cases(sp3[c("MN")]),]
  
  sp3<-sp3%>%group_by(ESTRATO_RIM,ESPECIE)%>%
    mutate(
      mareas=length(unique(COD_ID)),
      MAX=1.2*max(cooksd),
      t_max= max(TALLA_MEDIA_MAREA),
      t_min= min(TALLA_MEDIA_MAREA))%>%as.data.frame()
  
  
  OUTLIERS<-subset(sp3, cooksd>8*MN & EJEM_MED_MAREA>3)%>%
    dplyr::select(-c( MAX, MN)); as.data.frame(OUTLIERS)
  
  
  fwrite(OUTLIERS,"OUTLIERS.txt")
  library(openxlsx)
  write.xlsx(OUTLIERS, 'OUTLIERS.xlsx')
  
  rm(sp2)
  
  library(ggbeeswarm)
  sp3$PUERTO2<-ifelse(sp3$COD_TIPO_MUE %in% c(4,6), "A BORDO", sp3$PUERTO)
  table(sp3$CALADERO_DCF)
  table(sp3$ESTRATO_RIM)
  sp3<-sp3%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(
    FILTRO=ifelse(any(cooksd>8*MN),
                  "OUT", "NO OUT"  ))
sp3<-sp3%>%#  dplyr::select(1:6,10,14,26,25,29:36)%>%unique() %>%
    as.data.table()

  return(sp3)
  print(sp3)

}
