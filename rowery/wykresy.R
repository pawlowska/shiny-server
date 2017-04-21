library(RColorBrewer)
library(ggplot2)
library(data.table)
library(scales) #for nicer y axis

#wykres pojedynczej kolumny w zakresie danych
# wykres_jeden<-function(dane, kolumna, start=as.Date("2016-01-01"), stop=as.Date("2016-12-19")) {
#   zakres=seq(from=start, to=stop, by=1)
#   do_wykresu<-dane[Data %in% zakres, c("Data",kolumna), with = F]
# 
#   
#   g<-ggplot(do_wykresu, 
#             aes(Data, get(kolumna)))+geom_line(colour=kolory[2], size=1
#             )+ylab("Liczba rowerów")+ggtitle(kolumna)
#   g<-g+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   g<-g+scale_x_date(date_breaks = "2 weeks")
#   g
# }

labelsy<-function(krok) {
  function(x) {
    if (krok==7) {l=format(x, format="%Y-%U")}
    else if (krok==31) {l=format(x, format="%Y-%m")}
    else l=x
    l
  }
}

wykres_lokalny<-function(dane, kolumny, start=as.Date("2016-01-01"), stop=as.Date("2016-12-19"), paleta=kolory) {
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
  else {breaks = "2 months"}
  breaks
}

lista_weekendow<-function(dane) {
  daty<-dane[weekend(Data)=="weekend", Data] #now it's a vector
  daty<-unique(daty)

  krok = time_length(interval(daty[1], daty[2]), unit = "day")
  if (krok!=1) daty<-daty[2:length(daty)]
  soboty<-daty[weekdays(daty) %in% c("sobota","Saturday", "Sat")]
  niedziele<-daty[weekdays(daty) %in% c("niedziela","Sunday", "Sun")]
  soboty<-soboty[1:length(niedziele)]
  l<-data.table(soboty=soboty, niedziele=niedziele)
  l
  
}


#wykres kilku kolumn
wykres_kilka<-function(dane, start, stop, paleta, linie) {
#dane w formacie long do łatwiejszego wyboru grup
  
  krok = time_length(interval(dane[1,Data],dane[2,Data]), unit = "day")

  #x data range and ticks  
  zakres_dat=interval(start, stop)
  breaks<-better_ticks(zakres_dat, krok)

  #set theme    
  theme_set(theme_light(base_size = 14))

  g<-ggplot(dane
            )+geom_line( aes(Data, Liczba_rowerow, colour=Miejsce, linetype=Miejsce),size=0.7
            )#+ggtitle("Liczba rowerów zarejestrowanych przez liczniki")
  
  if(krok==1) #show weekends only for the daily plot
  {
    lista<-lista_weekendow(dane)
    g<-g+geom_rect(data=lista, 
                   aes(xmin=soboty, xmax=niedziele, ymin=-Inf, ymax=+Inf), 
                   fill='gray', 
                   alpha=0.2) +
      theme( # remove the vertical grid lines
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  }
  
  g<-g+scale_x_date(date_breaks = breaks, labels=labelsy(krok),limits=c(min(start),max(stop)),
                    expand=c(0,0)) #numer X ticks
  g<-g+scale_y_continuous(breaks = pretty_breaks(7), labels=comma_format())+theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  #colours and line types
  g<-g+scale_linetype_manual(values=linie)+(scale_colour_manual(values=paleta))
  #axis labels
  g<-g+xlab("Data")+ylab("")

  g<-g+guides(col = guide_legend(ncol = 3, byrow = TRUE))
  
  g
}