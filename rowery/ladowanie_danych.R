library(data.table)

zaladuj_dane<-function(plik, sep=';') {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8")
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols] 
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setorder(tabela, Data)
  setnames(tabela, 19:20, c("Żwirki i Wigury/Trojdena1 Rower", "Żwirki i Wigury/Trojdena2 Rower"))
  nazwy<-names(tabela)
  nowe_nazwy<-gsub(" Rower", "", nazwy)
  setnames(tabela, nazwy, nowe_nazwy)
  tabela
}

raw_to_long<-function(dane) {
  #print(str(dane))
  tabela<-melt(dane, id.vars = "Data", variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

wczytaj_dane<-function(plik = "dane_polaczone.csv") {
  tabela<-fread(plik, header = TRUE, encoding = "UTF-8")
  tabela[,V1:=NULL]
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  tabela
}

podsumuj.tygodnie<-function(tabela) {
  tabela[,Tydzien:=format(Data, format="%Y-%U")]
  tabela
}