library(data.table)

nazwa_pliku = 'Zapytanie2016.csv'

zaladuj_dane<-function(plik, sep=';') {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8")
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols] 
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  #tabela[,Data := as.POSIXct(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setorder(tabela, Data)
  setnames(tabela, 19:20, c("Żwirki i Wigury/Trojdena1 Rower", "Żwirki i Wigury/Trojdena2 Rower"))
  nazwy<-names(tabela)
  nowe_nazwy<-gsub(" Rower", "", nazwy)
  setnames(tabela, nazwy, nowe_nazwy)
  tabela
}

raw_to_long<-function(dane) {
  tabela<-melt(dane, id.vars = "Data", variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

### ladowanie danych:
#dane<-zaladuj_dane(nazwa_pliku)
#dane_long<-raw_to_long(dane)
#nazwy<-names(dane)[2:20]