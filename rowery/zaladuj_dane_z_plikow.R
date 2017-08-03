### ladowanie danych:
library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')

#dane polaczone z danych godzinowych
dane_dolutego2017<-fread("dane-20140813-20170202.csv", encoding = 'UTF-8')
dane_dolutego2017[,Data := as.Date(Data, tz="Europe/Berlin")]

dane_domarca2017<-zaladuj_dane('dane/2017styczen_marzecutf8.csv') 
setnames(dane_domarca2017, grep("Praska", names(dane_domarca2017), value = TRUE), grep("Praska", names(dane_dolutego2017), value = TRUE))
setnames(dane_domarca2017, names(dane_domarca2017), names(dane_dolutego2017))

dane_od2014_domarca2017<-rbind(dane_dolutego2017[Data<min(dane_domarca2017[,Data]),], dane_domarca2017)

#polacz kwiecien i czerwiec
dane_dokwietnia2017<-zaladuj_dane('dane/2017marzec_kwiecien_utf8.csv', zwirki_i_wigury = FALSE) 
dane_doczerwca2017<-zaladuj_dane('dane/2017kwiecien-czerwiec_utf8.csv', zwirki_i_wigury = FALSE) 

dane_odmarca_doczerwca2017<-rbind(dane_dokwietnia2017[Data<min(dane_doczerwca2017[,Data]),], dane_doczerwca2017)
dane_odmarca_doczerwca2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_odmarca_doczerwca2017, "Rowery", grep("Praska", names(dane_dolutego2017), value = TRUE))

#OK

dane_dolipca2017<-zaladuj_dane('dane/2017_czerwiec-lipiec.csv', zwirki_i_wigury = FALSE) 
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

dane<-numery_dat(dane_od2014_lipca2017)

write.csv(dane, file = "dane_polaczone.csv", fileEncoding = 'UTF-8')
nazwy<-names(dane)[4:ncol(dane)]

dane_zsumowane<-suma_licznikow(dane)
write.csv(dane_zsumowane, file = "dane_polaczone_zsumowane.csv", fileEncoding = 'UTF-8')


#dane godzinowe
#pilotaz
godz_wybrane_2017<-zaladuj_dane_godzinowe('dane/2017_08_05-14_06-bez-kierunkow.csv', format="%y-%m-%d %H:%M", ziw=F)
godz_wybrane_2017<-suma_licznikow(godz_wybrane_2017)
nazwy_licznikow<-names(godz_wybrane_2017)[4:27]
godz_wybrane_2017[,Jaki_dzien:=weekend(Data)]
write.csv(godz_wybrane_2017, file = "dane_godzinowe_zsumowane.csv", fileEncoding = 'UTF-8', row.names = FALSE)
#wide to long
godz_2017_long<-wide_to_long(godz_wybrane_2017, c("Czas", "Data", "Godzina", "Jaki_dzien"))
godz_2017_long_srednia<-godz_2017_long[,mean(Liczba_rowerow), by=c("Godzina","Jaki_dzien", "Miejsce")]
setnames(godz_2017_long_srednia, 'V1',"Srednia")
godz_2017_long<-merge(godz_2017_long, godz_2017_long_srednia, by=c("Godzina","Jaki_dzien", "Miejsce"))
write.csv(godz_2017_long, file = "dane_godzinowe_long.csv", fileEncoding = 'UTF-8', row.names = FALSE)
