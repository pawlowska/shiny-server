library(ggplot2)
library(data.table)

weekend<-function(data) {
  dzien<-weekdays(data)
  ifelse (dzien %in% c("niedziela","sobota"), "weekend", "roboczy")
}

dodaj_pogode<-function(tabela) {
  temperatura<-fread("pogoda/IMGW_temp_20170228.csv", header = TRUE, encoding = "UTF-8", drop=1)
  opady<-fread("pogoda/IMGW_opady_20170305.csv", header = TRUE, encoding = "UTF-8", drop=1)
  opady[,"status.deszcz":=NULL]
  opady[,"status.snieg":=NULL]
  pogoda<-merge(temperatura, opady, by="data")
  pogoda[,data := as.Date(data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setnames(pogoda, "data", "Data")
  pogoda[,Jaki_dzien:=weekend(Data)]
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane
}


pogoda_basic<-function(dane, paleta) {
  #set theme    
  theme_set(theme_light(base_size = 14))

  breaks<-better_ticks(interval(as.Date("2014-08-01"), as.Date("2017-02-28")))
  
  gg<-ggplot(dane, 
             aes(temp_avg, Liczba_rowerow, colour = Miejsce, shape = Jaki_dzien)) +
    geom_point(size=2, alpha=0.7) +
    scale_shape_manual(values=c(16,1)) + #full and empty circles
    scale_x_continuous(breaks=pretty_breaks(8)) +
    geom_smooth(size=0.7, alpha=0.2, span = 0.5) +           # Add a loess smoothed fit curve with confidence region
    theme(legend.position="bottom", legend.justification="left", legend.box.just = "left",
          legend.margin=margin(0, -2, 0, 1, "cm"), legend.box="vertical")+
    scale_colour_manual(values=paleta) +
    xlab("Średnia temperatura dobowa")+ylab("")+ #axis labels
    guides(shape = guide_legend(title="Jaki dzień", order = 1), 
           colour = guide_legend(ncol = 3, byrow = TRUE, order = 2))
  
  gg
}