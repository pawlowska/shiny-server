library(RCurl)

source('haslo.R', encoding = 'UTF-8')

bazaHist="cbdh/"
bazaO="cbdo/"
warszawaOkecie="352200375"

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
#B600B008FD - Suma opadu - deszcz-doba-synop - JEST
#B650B008FD - Suma opadu - śnieg-doba-synop - JEST
#B609B00400 - Opad za 6 godzin-synop - JEST
#B600K00200 JEST
#B600K002FG JEST
#B200B00400

lista_poniedzialkow<-function(od=as.POSIXct("2014-07-28"), do=as.POSIXct("2017-03-30")) {
  library(lubridate)
  iledni = time_length(interval(od, do), unit="week")
  daty<-od+c(0:iledni)*days(7)
  daty_txt<-as.character(daty)
  daty_txt
}

lista_dni<-function(od=as.POSIXct("2014-07-28"), do=as.POSIXct("2017-06-30")) {
  library(lubridate)
  iledni = time_length(interval(od, do), unit="day")
  daty<-od+c(0:iledni)*days(1)
  daty_txt<-as.character(daty)
  daty_txt
}

zrob_link<-function(parametr, data, stacja=warszawaOkecie, baza=bazaO, zakres='/tydzien/') {
  paste("https://dane.imgw.pl/1.0/pomiary/", baza, stacja, '-', parametr, zakres, data, '?format=csv', sep="")
}

#link = "https://dane.imgw.pl/1.0/pomiary/cbdh/352200375-B100E00200/tydzien/2017-01-25?format=csv"


sciagaj_liste<-function(parametr, lista_dat, baza=bazaHist, zakres='/tydzien/') {
  library(data.table)
  dane<-NULL
  for (data in lista_dat) {
    link <- zrob_link(parametr, data, warszawaOkecie, baza, zakres)
    print(link)
    txt<- getURL(link, userpwd = credentials)
    tabelka<-read.csv(text=txt, sep=';')
    dane<-rbind(dane, tabelka)
    Sys.sleep(0.5)
  }
  print(nrow(dane))
  data.table(dane)

}

# temp_min<-sciagaj_liste(p_temp_min, poniedzialki[1:100])

#write.csv(temp_avg, file = "IMGW_temp_avg_B100B008CD_20170220.csv", fileEncoding = 'UTF-8')
#write.csv(temp_max, file = "IMGW_temp_max_B100B008BD_20170228.csv", fileEncoding = 'UTF-8')