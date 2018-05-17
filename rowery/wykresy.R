library(RColorBrewer)
library(ggplot2)
library(scales) #for nicer y axis
library(grid)

rozmiar_czcionki=12

labelsy<-function(krok) {
  function(x) {
    if (krok==7) {l=format(x, format="%Y-%U")}
    else if (krok==31) {l=format(x, format="%Y-%m")}
    else if (krok==366) {l=format(x, format="%Y")}
    else l=x
    l
  }
}

wykres_lokalny<-function(dane, kolumny, start=as.Date("2016-01-01"), stop=as.Date("2017-08-31"), paleta=kolory) {
  require(lubridate)
  
  #zakres_dat=seq(from=start, to=stop, by=1)
  zakres_dat=interval(start, stop)
  dane_zakres<-dane[Data %within% zakres_dat & Miejsce %in% kolumny]
  wykres_kilka(dane_zakres, start, stop)
}
  
better_ticks<-function(zakres_dat, krok=1) {
  iledni = time_length(zakres_dat, unit="day")
  if ((iledni<28)&&(krok==1)) {breaks="1 day"}
  else if ((iledni<100)&&(krok<=7)) {breaks = "1 week"}
  else if ((iledni<210)&&(krok<=7)) {breaks = "2 weeks"}
  else if (iledni<450) {breaks = "1 month"}
  else if (krok>50) {breaks="1 year"}
  else {breaks = "2 months"}
  breaks
}

lista_weekendow<-function(dane) {
  przedzial<-seq(min(dane[,Data]), max(dane[,Data]), by="days")
  daty<-przedzial[weekend(przedzial)=="weekend"]

  krok = time_length(interval(daty[1], daty[2]), unit = "day")
  if (is.na(krok)) {
    soboty=as.Date(character())
    niedziele=as.Date(character())
  }
  else {
    if (krok!=1) daty<-daty[2:length(daty)]
    soboty<-daty[weekdays(daty) %in% c("sobota","Saturday", "Sat")]
    niedziele<-daty[weekdays(daty) %in% c("niedziela","Sunday", "Sun")]
    soboty<-soboty[1:length(niedziele)]
  }
  l<-data.table(soboty=soboty, niedziele=niedziele)
  l
  
}


#wykres kilku kolumn
wykres_kilka<-function(dane, start, stop, paleta, linie, alfy, krok=1, wartosc='bezwzględne') {
#dane w formacie long do łatwiejszego wyboru grup
  start=min(c(start,dane[,Data]))
    
  #x data range and ticks  
  zakres_dat=interval(start, stop)
  breaks<-better_ticks(zakres_dat, krok)

  #set theme    
  theme_set(theme_light(base_size = rozmiar_czcionki))
  
  #start plot
  g<-ggplot(dane)
  
  if (wartosc=='bezwzględne') {
    g<-g+geom_line(aes(Data, Liczba_rowerow, colour=Miejsce, linetype=Miejsce),size=0.7)
    if(krok>30) {g<-g+geom_point(aes(Data, Liczba_rowerow, colour=Miejsce))}
  }
  else
    g<-g+geom_area( aes(Data, Liczba_rowerow,  fill=Miejsce, linetype=Miejsce, alpha=Miejsce))
  
  #show weekends only for the daily plot
  if(krok==1) { 
    lista<-lista_weekendow(dane)
    if(nrow(lista)>0) {
      g<-g+geom_rect(data=lista, 
                     aes(xmin=soboty, xmax=niedziele, ymin=-Inf, ymax=+Inf), 
                     fill='gray', alpha=0.2) +
        theme( # remove the vertical grid lines
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
    }
  } 
  
  #oś x
  g<-g+scale_x_date(date_breaks = breaks, labels=labelsy(krok),limits=c(min(start),max(stop)),
                    expand=c(0,0)) #numer X ticks
  #oś y
  g<-g+scale_y_continuous(breaks = pretty_breaks(7), labels=comma_format())
  
  g<-g+theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  
  #colours and line types
  g<-g+scale_linetype_manual(values=linie)+
       scale_colour_manual(values=paleta)+
       scale_fill_manual(values=paleta)+
       scale_alpha_manual(values = alfy)
    
  #axis labels
  g<-g+xlab("Data")+ylab("")

  g<-g+guides(col = guide_legend(byrow = TRUE))
  #g<-g+guides(col = guide_legend(ncol=4))
  
  g
}

# wykres_godzinowy<-function(dane, paleta, linie) {
#   theme_set(theme_light(base_size = rozmiar_czcionki))
#   
#   g<-ggplot(dane, aes(x = Godzina, y = Liczba_rowerow)) +
#     geom_line(aes(group=interaction(Data,Miejsce), colour=Miejsce), size=0.3, alpha=0.5) +
#     geom_line(aes(x=Godzina, y=Srednia, group=Miejsce, colour=Miejsce, linetype=Miejsce), size=0.95) +
#     scale_colour_manual(values=paleta)+scale_linetype_manual(values=linie) +
#     scale_x_continuous(breaks=pretty_breaks(11), expand=c(0, 1)) +
#     ylab("") +
#     facet_grid(. ~ Jaki_dzien) +
#     guides(col = guide_legend(ncol = 3, byrow = TRUE))+
#     theme(legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
#   g
# }

pogoda_basic<-function(dane, paleta) {
  #set theme    
  theme_set(theme_light(base_size = rozmiar_czcionki))
  
  method_fit<-ifelse(length(unique(dane[,Data]))<100, "lm", "loess")
  
  gg<-ggplot(dane, 
             aes(temp_avg, Liczba_rowerow, colour = Miejsce, shape = Jaki_dzien, size=(deszcz+snieg))) +
    geom_point(alpha=0.8) +
    scale_size(range = c(1.5, 8)) +
    scale_shape_manual(values=c(16,1)) + #full and empty circles
    scale_x_continuous(breaks=pretty_breaks(8), expand=c(0, 1)) +
    scale_y_continuous(breaks=pretty_breaks(8), limits = c(-20, NA)) +
    geom_smooth(size=0.7, alpha=0.2, span = 0.5, method=method_fit) +   # Add a loess smoothed fit curve with confidence region
    theme(legend.position="bottom", legend.justification="left", legend.box.just = "left",
          legend.margin=margin(0, -2, 0, 1, "cm"), legend.box="vertical")+
    scale_colour_manual(values=paleta) +
    xlab("Średnia temperatura dobowa (°C)")+ylab("")+ #axis labels
    guides(shape = guide_legend(title="Jaki dzień", order = 1),
           size = guide_legend(title="Opady (deszcz i śnieg) w mm", order = 2),
           colour = guide_legend(ncol = 3, byrow = TRUE, order = 3))
  
  gg
}

pogoda_opady<-function(dane, paleta) {
  #set theme    
  theme_set(theme_light(base_size = rozmiar_czcionki))
  
  method_fit<-ifelse(length(unique(dane[,Data]))<100, "lm", "loess")
  
  gg<-ggplot(dane, 
             aes(deszcz, Liczba_rowerow, colour = Miejsce, shape = Jaki_dzien, size=temp_avg)) +
    geom_point(alpha=0.8) +
    scale_size_continuous(range = c(0.01, 8), limits = c(5,25)) +
    scale_shape_manual(values=c(16,1)) + #full and empty circles
    scale_x_continuous(breaks=pretty_breaks(8), expand=c(0, 1)) +
    scale_y_continuous(breaks=pretty_breaks(8), limits = c(-20, NA)) +
    geom_smooth(size=0.7, alpha=0.2, span = 0.5, method=method_fit) +   # Add a loess smoothed fit curve with confidence region
    theme(legend.position="bottom", legend.justification="left", legend.box.just = "left",
          legend.margin=margin(0, -2, 0, 1, "cm"), legend.box="vertical")+
    scale_colour_manual(values=paleta) +
    xlab("Opady (mm)")+ylab("")+ #axis labels
    guides(shape = guide_legend(title="Jaki dzień", order = 1),
           size = guide_legend(title="Temperatura", order = 2),
           colour = guide_legend(ncol = 3, byrow = TRUE, order = 3))
  
  gg
}

wykres_pogody_w_czasie<-function(dane) {
  #set theme    
  theme_set(theme_light(base_size = rozmiar_czcionki))
  
  g_t<-ggplot(dane)+
    geom_line( aes(Data, temp_avg), colour='red3', size=0.5 )+
    geom_ribbon(aes(x=Data, ymin=temp_min, ymax=temp_max), fill='red', alpha=0.1)+
    scale_x_date(expand=c(0,0), labels=NULL)+ #numer X ticks
    ylab("Temperatura (°C)")+xlab("")
  
  g_d<-ggplot(dane)+
    geom_segment(aes(x=Data, xend=Data, y=0, yend=deszcz+snieg, color=Rodzaj_opadu), 
                 lineend = "butt", size=1)+
    scale_x_date(expand=c(0,0), labels=NULL)+ #numer X ticks
    scale_color_manual(values = c("midnightblue","deepskyblue"))+
    ylab("Opady (mm)")+xlab("")+theme(legend.position="none")
  
  
  lista<-lista_weekendow(dane)
  
  plot_list=list(g_t, g_d)
  for (i in 1:2) {
      plot_list[[i]]<-plot_list[[i]]+geom_rect(data=lista, 
                   aes(xmin=soboty, xmax=niedziele, ymin=-Inf, ymax=+Inf), 
                   fill='gray', 
                   alpha=0.2) +
      theme( # remove the vertical grid lines
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
      theme(plot.margin = unit(c(0,0,-5,0), "pt"))
  }
  plot_list
}

wykres_pogoda_liczba<-function(dane, start, stop, paleta, linie) {
  lista<- wykres_pogody_w_czasie(dane)
  g2<- wykres_kilka(dane, start, stop, paleta, linie)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(lista[[1]]), ggplotGrob(lista[[2]]),
                   ggplotGrob(g2), size = "last"))
}