library(RCurl)
library(data.table)

warszawaOkecie="352200375"
krakowBalice="350190566"

p_temp_avg="B100B008CD" #"Średnia temperatura powietrza-doba-synop"
p_temp_max="B100B008BD" #"Maksymalna temperatura powietrza-doba-synop"
p_temp_min="B100B008AD" #"Minimalna temperatura powietrza-doba-synop"
p_temp_avg_h="B100B00400"
p_deszcz_doba="B600B008FD" #Suma opadu - deszcz-doba-synop
p_snieg_doba="B650B008FD"
p_zachmurzenie_godz = "B314B00400" #Zachmurzenie całkowite-pomiar godzinowy-synop
p_wiatr_godz = "B200B00400" #"Prędkość wiatru-pomiar godzinowy-synop-obserwator"

#B600B018FD - Suma dobowa opadu - woda - NIE MA
#B601E002FD - Czas trwania opadu za ostatnie 24 h-synop - NIE MA
#B609B00400 - Opad za 6 godzin-synop - JEST
#B200B00400


zrob_link<-function(parametr, data, stacja=warszawaOkecie, baza=bazaO, zakres='/tydzien/') {
  paste("https://dane.imgw.pl/1.0/pomiary/", baza, stacja, '-', parametr, zakres, data, '?format=csv', sep="")
}

czytaj_dane_zipy<-function(od=1, do=1, nazwa_out="pogoda/IMGW_2018_01.csv", format='pogoda/s_d_%02d_2018.csv', stacja=warszawaOkecie) {
  nazwy_kolumn<-c('r', 'm', 'd', 'temp_max', 'temp_min', 'temp_avg', 'opad', 'rodzaj')
  pogoda<-data.table(matrix(nrow = 0, ncol = 8))
  setnames(pogoda, names(pogoda), nazwy_kolumn)
  for (i in od:do) {
    nazwa<-sprintf(format,i)
    dane<-fread(nazwa,colClasses="numeric")[V1==stacja]
    dane<-dane[,c('V3', 'V4', 'V5', 'V6', 'V8', 'V10', 'V14', 'V16') ]
    setnames(dane, names(dane), nazwy_kolumn)
    pogoda<-rbind(pogoda, dane)
  }
  pogoda[,Data:=as.Date(paste(r,m,d, sep='-'))]
  pogoda[,c('r','m','d'):=NULL]
  pogoda[,deszcz:=ifelse(rodzaj=='W', opad, 0)]
  pogoda[,snieg:= ifelse(rodzaj=='S', opad, 0)]
  setcolorder(pogoda, c('Data', nazwy_kolumn[4:8], 'deszcz', 'snieg'))
  write.csv(pogoda, file = nazwa_out, fileEncoding = 'UTF-8', row.names = F)
  pogoda
}