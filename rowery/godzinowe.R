

wykres_test<-function(tabela) {
  library(ggplot2)
  
  g<-ggplot()+
    geom_point(data=tabela, aes(Godzina, Banacha, color=as.factor(month(Data))))+
    scale_fill_discrete()
  g
}

wykres_box<-function(tabela) {
  library(ggplot2)
  
  g<-ggplot(data=tabela)+
    geom_boxplot(aes(as.factor(Godzina), Banacha))
  g
}

pora<-function(jaki_dzien, godzina) {
  ifelse (jaki_dzien=="roboczy",
          ifelse (godzina %in% c(7,8), "rano", ifelse(godzina %in% c(16,17,18), "popoludnie", "reszta")),
          ifelse (godzina %in% c(12, 13, 14, 15, 16), "weekend", "reszta")
  )
}

rodzaj_dnia<-function(dzien) {
  ifelse (dzien=="sobota",
          "sobota",
          ifelse (dzien %in% c("wtorek", "środa", "czwartek"), "wt-czw", "inny")
  )
}

wide_to_long_godz<-function(dane) {
  #print(str(dane))
  tabela<-melt(dane,
               id.vars = c("Data","Czas","Godzina", "temp_min", "temp_avg", "temp_max", 
                           "deszcz", "snieg", "chmury", "wiatr","Jaki_dzien", "Rok", "Pora"), 
               variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

#podsumowanie po stratTyg danych w formacie long
podsumuj <- function(tabela_long) { 
  podsumowanie <- tabela_long[, .(sum(Liczba_rowerow), mean(Liczba_rowerow)), by=.(Miejsce,Data,Pora, temp_min, temp_avg, temp_max, deszcz, snieg, chmury, wiatr)]
  setnames(podsumowanie, c("V1","V2"), c("Liczba rowerow","Liczba rowerow/h"))
  podsumowanie[,Jaki_dzien:=weekend(Data)]
  podsumowanie[,Rok:=as.factor(year(Data))]
  podsumowanie
}



#wykres kilku kolumn
wykres_pora<-function(dane, linie=c("solid", "dashed", "dotted", "dotdash")) {
  #dane w formacie long do łatwiejszego wyboru grup
  
  #set theme    
  theme_set(theme_light(base_size = 14))
  
  g<-ggplot(dane
  )+geom_line( aes(Data, `Liczba rowerow/h`, colour=Miejsce, linetype=Pora), size=1)

  g<-g+scale_y_continuous(breaks = pretty_breaks(7), labels=comma_format())+
    scale_x_date(date_breaks = "2 days", expand = c(0,0))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  #colours and line types
  g<-g+scale_linetype_manual(values=linie)
  #axis labels
  g<-g+xlab("Data")+ylab("Liczba rowerów/h")
  
  g<-g+guides(col = guide_legend(nrow = 2, byrow = TRUE))
  
  g
}

wykres_jedno_miejsce<-function(dane, linie=c("solid", "dashed", "dotted", "dotdash")) {
  #dane w formacie long do łatwiejszego wyboru grup
  
  #set theme    
  theme_set(theme_light(base_size = 14))
  
  g<-ggplot(dane
  )+geom_line( aes(Data, `Liczba rowerow/h`, colour=Rok, linetype=Pora), size=1)
  
  g<-g+scale_y_continuous(breaks = pretty_breaks(7), labels=comma_format())+
    scale_x_date(date_breaks = "2 days", expand = c(0,0))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  #colours and line types
  g<-g+scale_linetype_manual(values=linie)
  #axis labels
  g<-g+xlab("Data")+ylab("Liczba rowerów/h")
  
  g<-g+guides(col = guide_legend(nrow = 2, byrow = TRUE))
  
  g
}

# w_2017_roboczy<-wykres_pora(pods[(Rok==2017)&(Jaki_dzien=="roboczy"),])
# w_2016_roboczy<-wykres_pora(pods[(Rok==2016)&(Jaki_dzien=="roboczy"),])
# w_2015_roboczy<-wykres_pora(pods[(Rok==2015)&(Jaki_dzien=="roboczy"),])
# 
# w_2017_weekend<-wykres_pora(pods[(Rok==2017)&(Jaki_dzien=="weekend"),])
# w_2016_weekend<-wykres_pora(pods[(Rok==2016)&(Jaki_dzien=="weekend"),])
# w_2015_weekend<-wykres_pora(pods[(Rok==2015)&(Jaki_dzien=="weekend"),])

#w<- wykres_jedno_miejsce(pods[(Miejsce=="Banacha")&(Jaki_dzien=="roboczy"),])
