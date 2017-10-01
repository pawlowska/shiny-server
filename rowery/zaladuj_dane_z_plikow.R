### ladowanie danych:
library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')
source('read_from_api.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')

#dane od 08.2014 do 09.2017
dane_dowrzesnia2017<-zaladuj_dane_new('dane/dane_201408_20170925.csv')

ids<-read_counterids("pliki/counterids.json")
nowe_dane<-zaladuj_dane_api(ids=ids, od="2017-09-26")

dane<-rbind(dane_dowrzesnia2017, nowe_dane)

dane<-numery_dat(dane)

write.csv(dane, file = "dane/dane_polaczone.csv", fileEncoding = 'UTF-8')

dane_zsumowane<-suma_licznikow(dane)
rm(dane)
nazwy<-names(dane_zsumowane)[4:length(names(dane_zsumowane))]
listy_stylow<-zrob_listy_stylow(nazwy) #w obsluga_sumowania
write.csv(listy_stylow, file = "pliki/listy_stylow.csv", fileEncoding = 'UTF-8', row.names = F)
write.csv(dane_zsumowane, file = "dane/dane_polaczone_zsumowane.csv", fileEncoding = 'UTF-8')
dane_zsumowane<-dodaj_pogode(dane_zsumowane)
write.csv(dane_zsumowane, file = "dane/dane_zsumowane_z_pogoda.csv", fileEncoding = 'UTF-8')
dane_long<-wide_to_long(dane_zsumowane)
write.csv(dane_long, file = "pliki/dane_long.csv", fileEncoding = 'UTF-8')

#dane godzinowe
#pilotaz
godz_wybrane_2017<-zaladuj_dane_godzinowe('dane/2017_08_05-14_06.csv', format="%y-%m-%d %H:%M", ziw=F)
godz_wybrane_2017<-suma_licznikow_old(godz_wybrane_2017)
nazwy_licznikow<-names(godz_wybrane_2017)[4:27]
godz_wybrane_2017[,Jaki_dzien:=weekend(Data)]
write.csv(godz_wybrane_2017, file = "dane/dane_godzinowe_zsumowane.csv", fileEncoding = 'UTF-8', row.names = FALSE)
#wide to long
godz_2017_long<-wide_to_long(godz_wybrane_2017, c("Czas", "Data", "Godzina", "Jaki_dzien"))
godz_2017_long_srednia<-godz_2017_long[,mean(Liczba_rowerow), by=c("Godzina","Jaki_dzien", "Miejsce")]
setnames(godz_2017_long_srednia, 'V1',"Srednia")
godz_2017_long<-merge(godz_2017_long, godz_2017_long_srednia, by=c("Godzina","Jaki_dzien", "Miejsce"))
write.csv(godz_2017_long, file = "pliki/dane_godzinowe_long.csv", fileEncoding = 'UTF-8', row.names = FALSE)
