library(ggplot2)
library(data.table)

weekend<-function(data) {
  dzien<-weekdays(data)
  ifelse (dzien %in% c("niedziela","sobota", "Sunday", "Sun", "Saturday", "Sat"), "weekend", "roboczy")
}

dodaj_pogode<-function(tabela, 
                       plik_temperatura="IMGW_temp_20170420.csv", 
                       plik_opady="IMGW_opady_20170331.csv") {
  temperatura<-fread(plik_temperatura, header = TRUE, encoding = "UTF-8", drop=1)
  opady<-fread(plik_opady, header = TRUE, encoding = "UTF-8", drop=1)
  pogoda<-merge(temperatura, opady, by="Data")
  pogoda[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  pogoda[,Jaki_dzien:=weekend(Data)]
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane
}


pogoda_basic<-function(dane, paleta) {
  #set theme    
  theme_set(theme_light(base_size = 14))

  breaks<-better_ticks(interval(as.Date("2014-08-01"), as.Date("2017-03-31")))
  
  gg<-ggplot(dane, 
             aes(temp_avg, Liczba_rowerow, colour = Miejsce, shape = Jaki_dzien, size=(deszcz+snieg))) +
    geom_point(alpha=0.8) +
    scale_size(range = c(1.5, 8)) +
    scale_shape_manual(values=c(16,1)) + #full and empty circles
    scale_x_continuous(breaks=pretty_breaks(8), expand=c(0, 1)) +
    scale_y_continuous(breaks=pretty_breaks(8), limits = c(-20, NA)) +
    geom_smooth(size=0.7, alpha=0.2, span = 0.5) +           # Add a loess smoothed fit curve with confidence region
    theme(legend.position="bottom", legend.justification="left", legend.box.just = "left",
          legend.margin=margin(0, -2, 0, 1, "cm"), legend.box="vertical")+
    scale_colour_manual(values=paleta) +
    xlab("Średnia temperatura dobowa (°C)")+ylab("")+ #axis labels
    guides(shape = guide_legend(title="Jaki dzień", order = 1),
           size = guide_legend(title="Opady (deszcz i śnieg) w mm", order = 2),
           colour = guide_legend(ncol = 3, byrow = TRUE, order = 3))
  
  gg
}