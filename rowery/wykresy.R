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
  

#wykres kilku kolumn
wykres_kilka<-function(dane, start, stop, paleta, linie) {
#dane w formacie long do łatwiejszego wyboru grup
  
  #x data range and ticks  
  zakres_dat=interval(start, stop)
  iledni = zakres_dat %/% days(1)
  if (iledni<21) {breaks="1 day"}
  else if (iledni<100) {breaks = "1 week"}
  else if (iledni<210) {breaks = "2 weeks"}
  else if (iledni<450) {breaks = "1 month"}
  else {breaks = "2 months"}

  krok = interval(dane[1,Data],dane[2,Data]) %/% days(1)

  #set theme    
  theme_set(theme_light(base_size = 14))
  
  g<-ggplot(dane, 
            aes(Data, Liczba_rowerow, colour=Miejsce, linetype=Miejsce)
            )+geom_line(size=0.7
            )#+ggtitle("Liczba rowerów zarejestrowanych przez liczniki")
  g<-g+scale_x_date(date_breaks = breaks, labels=labelsy(krok),limits=c(min(start),max(stop)),
                    expand=c(0,0)) #numer X ticks
  g<-g+scale_y_continuous(breaks = pretty_breaks(5), labels=comma_format())+theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position="bottom", legend.margin=margin(0, -2, 0, 1, "cm"))
  #colours and line types
  g<-g+scale_linetype_manual(values=linie)+(scale_colour_manual(values=paleta))
  #axis labels
  g<-g+xlab("Data")+ylab("")
  #g<-g+guides(title=NULL, fill=guide_legend(nrow=4,byrow=TRUE))
  g<-g+guides(col = guide_legend(ncol = 3, byrow = TRUE))
  g
  
  
}