library(data.table)

zaladuj_dane<-function(plik, sep=';') {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8")
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols] 
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setorder(tabela, Data)
  setnames(tabela, 19:20, c("Żwirki i Wigury/Trojdena, zach Rower", "Żwirki i Wigury/Trojdena, wsch Rower"))
  nazwy<-names(tabela)
  nowe_nazwy<-gsub(" Rower", "", nazwy)
  setnames(tabela, nazwy, nowe_nazwy)
  tabela
}

#currently unused
raw_to_long<-function(dane) {
  #print(str(dane))
  tabela<-melt(dane, id.vars = "Data", variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

wczytaj_dane<-function(plik = "dane_polaczone.csv") {
  tabela<-fread(plik, header = TRUE, encoding = "UTF-8")
  tabela[,V1:=NULL]
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  tabela[,startTyg := as.Date(startTyg, tz="Europe/Berlin", format="%Y-%m-%d")]
  tabela[,startM := as.Date(startM, tz="Europe/Berlin", format="%Y-%m-%d")]
  tabela
}



#numery tygodni i miesiecy
numery_dat<-function(tabela) { 
#  tabela[,Tydzien:=format(Data, format="%Y-%U")]
  nazwy<-names(tabela)[2:ncol(tabela)]
  library(lubridate)
  #uses lubridate; correction to make the week start Monday
  tabela[,startTyg:=floor_date(Data-days(1), "week")+days(1)]
  tabela[,startM:=floor_date(Data, "month")]
  setcolorder(tabela, c("Data", "startTyg", "startM", nazwy))
  
  tabela
}

wide_to_long<-function(dane) {
  #print(str(dane))
  tabela<-melt(dane, id.vars = c("Data","startTyg","startM"), variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

#podsumowanie po stratTyg danych w formacie long
podsumuj.tygodnie <- function(tabela_long) { 
  podsumowanie <- tabela_long[,sum(Liczba_rowerow), by=.(Miejsce,startTyg)]
  setnames(podsumowanie, c("startTyg","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie
}

#podsumowanie po stratM danych w formacie long
podsumuj.miesiace <- function(tabela_long) { 
  podsumowanie <- tabela_long[,sum(Liczba_rowerow), by=.(Miejsce,startM)]
  setnames(podsumowanie, c("startM","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie
}