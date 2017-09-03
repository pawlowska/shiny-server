### ladowanie danych:
library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')
source('read_from_api.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')

#dane polaczone z danych godzinowych
dane_dolutego2017<-fread("dane/dane-20140813-20170202-poprSw.csv", encoding = 'UTF-8')
dane_dolutego2017[,Data := as.Date(Data, tz="Europe/Berlin")]

dane_domarca2017<-zaladuj_dane('dane/2017styczen_marzecutf8-poprSw.csv') 
setnames(dane_domarca2017, grep("Praska", names(dane_domarca2017), value = TRUE), grep("Praska", names(dane_dolutego2017), value = TRUE))
setnames(dane_domarca2017, names(dane_domarca2017), names(dane_dolutego2017))

dane_od2014_domarca2017<-rbind(dane_dolutego2017[Data<min(dane_domarca2017[,Data]),], dane_domarca2017)

#polacz kwiecien i czerwiec
dane_dokwietnia2017<-zaladuj_dane('dane/2017marzec_kwiecien_utf8_poprPZ-poprSw.csv', zwirki_i_wigury = FALSE) 
dane_doczerwca2017<-zaladuj_dane('dane/2017kwiecien-czerwiec_utf8-poprSw.csv', zwirki_i_wigury = FALSE) 

dane_odmarca_doczerwca2017<-rbind(dane_dokwietnia2017[Data<min(dane_doczerwca2017[,Data]),], dane_doczerwca2017)
dane_odmarca_doczerwca2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_odmarca_doczerwca2017, "Rowery", grep("Praska", names(dane_dolutego2017), value = TRUE))

dane_dolipca2017<-zaladuj_dane('dane/2017_czerwiec-lipiec_poprPZ-poprSw.csv', zwirki_i_wigury = FALSE) 
dane_dolipca2017[,"Praska sciezka rekreacyjna":=NULL]
dane_dolipca2017[,"Piesi":=NULL]
setnames(dane_dolipca2017, "Rowery", grep("Praska", names(dane_dolutego2017), value = TRUE))

dane_od2014_lipca2017<-rbind(dane_od2014_domarca2017[Data<min(dane_odmarca_doczerwca2017[,Data]),], 
                               dane_odmarca_doczerwca2017[Data<min(dane_dolipca2017[,Data]),],
                               dane_dolipca2017, fill=TRUE)


rm(dane_dolutego2017)
rm(dane_od2014_domarca2017)
rm(dane_odmarca_doczerwca2017)
rm(dane_doczerwca2017)
rm(dane_dolipca2017)

ids<-read_counterids()
nowe_dane<-zaladuj_dane_api(ids=ids, od="2017-07-31")

dane<-rbind(dane_od2014_lipca2017[Data<as.Date("2017-07-31")], nowe_dane, fill=TRUE)

dane<-numery_dat(dane)

write.csv(dane, file = "dane/dane_polaczone.csv", fileEncoding = 'UTF-8')
nazwy<-names(dane)[4:ncol(dane)]


dane_zsumowane<-suma_licznikow(dane)
rm(dane)
nazwy<-names(dane_zsumowane)[4:length(names(dane_zsumowane))]
listy_stylow<-zrob_listy_stylow(nazwy) #w obsluga_sumowania
write.csv(listy_stylow, file = "listy_stylow.csv", fileEncoding = 'UTF-8', row.names = F)
write.csv(dane_zsumowane, file = "dane/dane_polaczone_zsumowane.csv", fileEncoding = 'UTF-8')
dane_zsumowane<-dodaj_pogode(dane_zsumowane)
write.csv(dane_zsumowane, file = "dane/dane_zsumowane_z_pogoda.csv", fileEncoding = 'UTF-8')
dane_long<-wide_to_long(dane_zsumowane)
write.csv(dane_long, file = "dane_long.csv", fileEncoding = 'UTF-8')



#dane godzinowe
#pilotaz
godz_wybrane_2017<-zaladuj_dane_godzinowe('dane/2017_08_05-14_06.csv', format="%y-%m-%d %H:%M", ziw=F)
godz_wybrane_2017<-suma_licznikow(godz_wybrane_2017)
nazwy_licznikow<-names(godz_wybrane_2017)[4:27]
godz_wybrane_2017[,Jaki_dzien:=weekend(Data)]
write.csv(godz_wybrane_2017, file = "dane/dane_godzinowe_zsumowane.csv", fileEncoding = 'UTF-8', row.names = FALSE)
#wide to long
godz_2017_long<-wide_to_long(godz_wybrane_2017, c("Czas", "Data", "Godzina", "Jaki_dzien"))
godz_2017_long_srednia<-godz_2017_long[,mean(Liczba_rowerow), by=c("Godzina","Jaki_dzien", "Miejsce")]
setnames(godz_2017_long_srednia, 'V1',"Srednia")
godz_2017_long<-merge(godz_2017_long, godz_2017_long_srednia, by=c("Godzina","Jaki_dzien", "Miejsce"))
write.csv(godz_2017_long, file = "dane_godzinowe_long.csv", fileEncoding = 'UTF-8', row.names = FALSE)
