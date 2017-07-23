### ladowanie danych:
library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')

#dane od Pawla Ziniewicza
dane_od2014<-zaladuj_dane('dane/zapytanie PZiniewicz_utf8.csv')
#wybierz lata 2014-15
dane_2014_2015<-dane_od2014[as.numeric(format(Data,'%Y'))<2016]
#wykasuj praska sciezke, bo nie wiadomo, czy oddziela rowery od pieszych
dane_2014_2015_bez_praskiej<-dane_2014_2015[,"Praska sciezka rekreacyjna":=NA]
rm(dane_od2014)

#dane z maila 20.12.2016
dane_wiekszosc2016<-zaladuj_dane('dane/Zapytanie2016utf8.csv')
setnames(dane_wiekszosc2016, "Dworzec Wileński Nowy( Targowa)", "Dworzec Wileński Nowy (Targowa)")
setnames(dane_wiekszosc2016, "Rowery (Praska ścieżka rekreacyjna)", "Praska ścieżka rekreacyjna")

#ujednolic nazwy
setnames(dane_2014_2015_bez_praskiej, names(dane_2014_2015_bez_praskiej), names(dane_wiekszosc2016))

dane_dostycznia2017<-zaladuj_dane('dane/5720_2017-01-26-12-50-35utf8.csv')
dane_dostycznia2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_dostycznia2017, names(dane_dostycznia2017), names(dane_wiekszosc2016))

dane_domarca2017<-zaladuj_dane('dane/2017styczen_marzecutf8.csv') 
#poprawiona Praska sciezka - dane z 5720_2017-03-21-15-58-06.xlsx
#dane_domarca2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_domarca2017, names(dane_domarca2017), names(dane_wiekszosc2016))

dane_dokwietnia2017<-zaladuj_dane('dane/2017marzec_kwiecien_utf8.csv', zwirki_i_wigury = FALSE) 
dane_dokwietnia2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_dokwietnia2017, names(dane_dokwietnia2017), names(dane_wiekszosc2016))

dane_doczerwca2017<-zaladuj_dane('dane/2017kwiecien-czerwiec_utf8.csv', zwirki_i_wigury = FALSE) 
dane_doczerwca2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_doczerwca2017, "Rowery", "Praska ścieżka rekreacyjna" )
setnames(dane_doczerwca2017, "Dworzec Wileński Nowy( Targowa)", "Dworzec Wileński Nowy (Targowa)")
setnames(dane_doczerwca2017, "Żwirki i Wigury/Trojdena zach.", "Żwirki i Wigury/Trojdena, zach")
setnames(dane_doczerwca2017, "Żwirki i Wigury/Trojdena wsch.", "Żwirki i Wigury/Trojdena, wsch")
setcolorder(dane_doczerwca2017, names(dane_wiekszosc2016))
setnames(dane_doczerwca2017, names(dane_doczerwca2017), names(dane_wiekszosc2016))

#setnames(dane_dokwietnia2017, names(dane_dokwietnia2017), names(dane_wiekszosc2016))

dane<-rbind(dane_2014_2015_bez_praskiej, dane_wiekszosc2016[1:354], 
            dane_dostycznia2017[1:37], dane_domarca2017[1:53], 
            dane_dokwietnia2017[1:31], dane_doczerwca2017)

rm(dane_2014_2015_bez_praskiej)
rm(dane_wiekszosc2016)
rm(dane_dostycznia2017)
rm(dane_domarca2017)
rm(dane_dokwietnia2017)
rm(dane_doczerwca2017)

dane<-numery_dat(dane)

write.csv(dane, file = "dane_polaczone.csv", fileEncoding = 'UTF-8')
nazwy<-names(dane)[4:ncol(dane)]

dane_zsumowane<-suma_licznikow(dane)
write.csv(dane_zsumowane, file = "dane_polaczone_zsumowane.csv", fileEncoding = 'UTF-8')


#dane godzinowe
#pilotaz
godz_wybrane_2017<-zaladuj_dane_godzinowe('dane/2017_08_05-14_06-bez-kierunkow.csv', format="%y-%m-%d %H:%M")
godz_wybrane_2017<-filtruj_in_out(godz_wybrane_2017)
setnames(godz_wybrane_2017, "Żwirki i Wigury/Trojdena zach.", "Żwirki i Wigury/Trojdena, zach")
setnames(godz_wybrane_2017, "Żwirki i Wigury/Trojdena wsch.", "Żwirki i Wigury/Trojdena, wsch")
godz_wybrane_2017<-suma_licznikow(godz_wybrane_2017)
nazwy_licznikow<-names(godz_wybrane_2017)[4:27]
print(nazwy_licznikow)
godz_wybrane_2017[,Jaki_dzien:=weekend(Data)]
write.csv(godz_wybrane_2017, file = "dane_godzinowe_zsumowane.csv", fileEncoding = 'UTF-8', row.names = FALSE)
#wide to long
godz_2017_long<-wide_to_long(godz_wybrane_2017, c("Czas", "Data", "Godzina", "Jaki_dzien"))
godz_2017_long_srednia<-godz_2017_long[,mean(Liczba_rowerow), by=c("Godzina","Jaki_dzien", "Miejsce")]
setnames(godz_2017_long_srednia, 'V1',"Srednia")
godz_2017_long<-merge(godz_2017_long, godz_2017_long_srednia, by=c("Godzina","Jaki_dzien", "Miejsce"))
write.csv(godz_2017_long, file = "dane_godzinowe_long.csv", fileEncoding = 'UTF-8', row.names = FALSE)
