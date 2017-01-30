library(RColorBrewer)
library(ggplot2)
library(data.table)

source('palety_kolorow.R')

kolory<-paleta2

#wykres pojedynczej kolumny w zakresie danych
wykres_jeden<-function(dane, kolumna, start=as.Date("2016-01-01"), stop=as.Date("2016-12-19")) {
  zakres=seq(from=start, to=stop, by=1)
  do_wykresu<-dane[Data %in% zakres, c("Data",kolumna), with = F]

  
  g<-ggplot(do_wykresu, 
            aes(Data, get(kolumna)))+geom_line(colour=kolory[2], size=1
            )+ylab("Liczba rowerów")+ggtitle(kolumna)
  g<-g+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g<-g+scale_x_date(date_breaks = "2 weeks")
  g
}

#wykres kilku kolumn
wykres_kilka<-function(dane, kolumny, start=as.Date("2016-01-01"), stop=as.Date("2016-12-19"), paleta=kolory) {
#dane w formacie long do łatwiejszego wyboru grup
  zakres_dat=seq(from=start, to=stop, by=1)
  iledni = length(zakres_dat)
  if (iledni<21) {breaks="1 day"}
  else if (iledni<100) {breaks = "1 week"}
  else if (iledni<210) {breaks = "2 weeks"}
  else if (iledni<450) {breaks = "1 month"}
  else {breaks = "2 months"}
  dane_zakres<-dane[Data %in% zakres_dat & Miejsce %in% kolumny]

  theme_set(theme_gray(base_size = 14))
  
  g<-ggplot(dane_zakres, 
            aes(Data, Liczba_rowerow, by=Miejsce, colour=Miejsce)
            )+geom_line(size=0.7
            )#+ggtitle("Liczba rowerów zarejestrowanych przez liczniki")
  g<-g+scale_x_date(date_breaks = breaks, limits=c(min(start),max(stop)),expand=c(0,0)) #numer X ticks
  g<-g+theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position="bottom", legend.margin  =margin(0, -2, 0, 1, "cm"))+(
      scale_colour_manual(values=paleta)
    )
  g<-g+xlab("Data")+ylab("")
  #g<-g+guides(title=NULL, fill=guide_legend(nrow=4,byrow=TRUE))
  g<-g+guides(col = guide_legend(ncol = 3, byrow = TRUE))
  g
  
}