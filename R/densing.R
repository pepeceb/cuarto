densing <- function(x) {
library(stringr)
library(lubridate)
library(dplyr)
library(data.table)
library(janitor)
library(stringi)
library(tidyr) 

 
  colp <- c("A CORUNA" = "steelblue", "SANTA EUGENIA DE RIBEIRA" = "blue","RIBEIRA" = "blue", "CILLERO"="darkgreen",
            "VIGO" = "orange", "AVILES-GIJON" = "darkblue","AVILES"="red", "GIJON"="#00BFC4",
            "SANTONA" = "#7CAE00", "CEDEIRA"="forestgreen", "FINISTERRE"= "darkgoldenrod2",
            "LUARCA" = "chartreuse4", "MUROS"= "#619CFF", "CELEIRO"="darkgreen", 
            "BURELA" ="yellowgreen","SUANCES"="deeppink3",
            "MARIN"= "mediumorchid", "SAN VICENTE DE LA BARQUERA"= "tomato",
            "ISLA CRISTINA" ="steelblue", "LLANES"= "darksalmon",
            "PUNTA UMBRIA" = "slateblue3", "BARBATE"= "red3","SANTANDER"= "red",
            "PUERTO DE SANTA MARIA"="darkorchid2","ROTA"="orange","A BORDO" = "black",
            "CADIZ"="Chartreuse2", "TARIFA"= "coral1", "AYAMONTE"= "coral3",
            "SANLUCAR DE BARRAMEDA"= "darksalmon","PUNTA DEL MORAL"= "red",
            "CASTLETOWN BERE" = "deeppink3", "PUERTO DE LA VEGA"="black", "MUXIA"="tomato2")    
  
  
  
  
  
  tallax<-tallax[complete.cases(tallax[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]
  
    tallax$PUERTO <- toupper(stri_trans_general(
    tallax$PUERTO,    "Latin-ASCII"))
  
  
    espania <- tallax%>% group_by(ESTRATO_RIM,COD_ID,COD_TIPO_MUE,PUERTO,ESP_MUE,ESP_CAT, TALLA) %>%
    expand(count = seq(1:EJEM_MEDIDOS)) %>% mutate(n=1)  
    
  
  
  
  
ggplot(espania, aes(TALLA, fill=PUERTO)) + 
 # geom_histogram(binwidth = 2.5)+
  geom_density(aes(y=2.5 * ..count..))  +
  facet_wrap(.~COD_ID, scales="fixed")   +
  scale_fill_manual(values=colp)  +
  theme_bw()+
  labs(subtitle = espania$ESP_CAT)+
  scale_y_sqrt()+
    theme(strip.text.x = element_text(size=10, angle=0,face="bold", colour="white"),
          strip.text.y = element_text(size=10, face="bold",colour="white"),
          strip.background = element_rect(colour="white", fill=c( "steelblue")))  +
    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic", 
                                       size = 14)) +
  ggtitle(espania$ESTRATO_RIM)

 
}
