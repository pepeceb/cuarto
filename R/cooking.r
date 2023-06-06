cooking <- function(AC, cooksd, PESO_SP, PUERTO2, ESPECIE, ESTRATO_RIM, sp3, MN, FECHA) {
temp_plot  <-ggplot(data =AC,
         mapping = aes(y = cooksd, x=TALLA_MEDIA_MAREA, col=factor(PUERTO2)))  +
    geom_quasirandom(aes(colour = PUERTO2, size=2,x=TALLA_MEDIA_MAREA, y=cooksd  ),
                     method = "smiley")  +
    
    geom_hline(data = AC, aes(yintercept = 4*AC$MN),size=1.5, colour="red")  +
    guides(colour = guide_legend(override.aes = list(size = 3)))     +
    guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=3))) +
    scale_size(range=c(2,5))  +
    facet_wrap(ESPECIE~ESTRATO_RIM, scales="free")   +
    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic", 
                                       size = 14)) +
    scale_colour_manual(values=colp,limits = force)   +
    theme(legend.text = element_text(colour="blue", size = 10, face = "bold"))+
    theme(axis.text=element_text(angle=0, size=12, face="bold")) +
    
    geom_blank(aes(x = 0.99*t_min)) + 
    geom_blank(aes(x = 1.05*t_max)) +
    geom_blank(aes(y = 0.99*MAX)) +
    
    
    
    geom_label_repel(show.legend=FALSE,data=subset(AC,cooksd>4*MN),aes(fontface="bold", 
                          TALLA_MEDIA_MAREA,cooksd, 
label = ifelse(cooksd>4*MN,paste(round(TALLA_MEDIA_MAREA,2), "cm", "\n",
                          FECHA, " ", "\n",EJEM_MED_MAREA, "Ejemplares"),"")  ,
                              vjust=0, hjust=0.5))     +
    guides(colour = guide_legend(override.aes = list(size=5,linetype=4)))
  
ggsave(temp_plot, file=paste0("2021_plot_TALLAS_MEDIAS ",AC$ESPECIE," ", ".png"), width = 35, height =25, units = "cm")
}
