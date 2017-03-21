### ladowanie danych:

library(data.table)

#dane od Pawla Ziniewicza
dane_od2014<-zaladuj_dane('dane/zapytanie PZiniewicz_utf8.csv')
#wybierz lata 2014-15
dane_2014_2015<-dane_od2014[as.numeric(format(Data,'%Y'))<2016]
#wykasuj praska sciezke, bo nie wiadomo, czy oddziela rowery od pieszych
dane_2014_2015_bez_praskiej<-dane_2014_2015[,"Praska sciezka rekreacyjna":=NA]

#dane z maila 20.12.2016
dane_wiekszosc2016<-zaladuj_dane('dane/Zapytanie2016utf8.csv')

#ujednolic nazwy
setnames(dane_2014_2015_bez_praskiej, names(dane_2014_2015_bez_praskiej), names(dane_wiekszosc2016))

dane_dostycznia2017<-zaladuj_dane('dane/5720_2017-01-26-12-50-35utf8.csv')
dane_dostycznia2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_dostycznia2017, names(dane_dostycznia2017), names(dane_wiekszosc2016))

dane_domarca2017<-zaladuj_dane('dane/2017styczen_marzecutf8.csv') 
#poprawiona Praska sciezka - dane z 5720_2017-03-21-15-58-06.xlsx
#dane_domarca2017[,"Praska sciezka rekreacyjna":=NULL]
setnames(dane_domarca2017, names(dane_domarca2017), names(dane_wiekszosc2016))


dane<-rbind(dane_2014_2015_bez_praskiej, dane_wiekszosc2016[1:354], dane_dostycznia2017, dane_domarca2017)

dane<-numery_dat(dane)

write.csv(dane, file = "dane_polaczone.csv", fileEncoding = 'UTF-8')
#dane_long<-raw_to_long(dane)
nazwy<-names(dane)[4:ncol(dane)]