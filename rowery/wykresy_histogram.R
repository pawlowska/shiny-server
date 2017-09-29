
wykres_histogram<-function(dane) {
  theme_set(theme_light(base_size = 14))
  
  g<-ggplot(dane, aes(as.factor(month(Data)), Liczba_rowerow,  fill=Miejsce)) +
    geom_bar(stat = "identity", position = "dodge", color="black", 
             aes(alpha=as.factor(year(Data))))+
    facet_grid(Miejsce ~ .) +
    xlab("Miesiąc")+ylab("Liczba rowerów")+
    theme(legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  g
}