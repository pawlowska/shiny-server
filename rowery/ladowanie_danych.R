library(data.table)

zaladuj_dane<-function(plik, sep=';', zwirki_i_wigury=TRUE) {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8", header = TRUE)
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols] 
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setorder(tabela, Data)
  setnames(tabela, "Dworzec Wileński Nowy( Targowa)", enc2utf8("Dworzec Wileński (Targowa)"))
  
  if (zwirki_i_wigury) {
    setnames(tabela, 19:20, c("Żwirki i Wigury/Trojdena, wsch. Rower", "Żwirki i Wigury/Trojdena, zach. Rower"))
  }
  nazwy<-names(tabela)
  nowe_nazwy<-gsub(" Rower", "", nazwy)
  setnames(tabela, nazwy, nowe_nazwy)
  tabela
}

zaladuj_dane_new<-function(plik, sep=';', bez_kierunkow=TRUE) {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8", header = TRUE)
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols] 
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  setorder(tabela, Data)

  tabela[,Piesi:=NULL]
  tabela[,'Praska ścieżka rekreacyjna':=Rowery]
  tabela[,Rowery:=NULL]
  
  #odrzuć kierunki
  if (bez_kierunkow) {tabela<-filtruj_in_out(tabela)}

  tabela
}

zaladuj_dane_godzinowe<-function(plik, sep=';', format="%d-%m-%y %H:%M", bez_kierunkow=TRUE, ziw=TRUE) {
  library(data.table)
  
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8", header = TRUE)
  #kolumny od 2 do końca to liczby
  cols<-2:ncol(tabela) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols]
  
  #bledne nazwy Zwirki i Wigury
  if (ziw) {
    indeksy<-grep("Wigury", names(tabela))
    nazwy_ziw<-names(tabela)[indeksy]
    nazwy_ziw<-c(sub("Trojdena", "Trojdena wsch.", nazwy_ziw[1:3]),
                 sub("Trojdena", "Trojdena zach.", nazwy_ziw[4:6]))
    setnames(tabela, indeksy, nazwy_ziw)
  }
  
  #odrzuć kierunki
  if (bez_kierunkow) {tabela<-filtruj_in_out(tabela)}
  
  #podziel na daty i godziny
  nazwy_licznikow<-names(tabela)[2:ncol(tabela)]
  nowe_nazwy<-gsub(" Rower", "", nazwy_licznikow)
  setnames(tabela, nazwy_licznikow, nowe_nazwy)
  nazwy_licznikow<-names(tabela)[2:ncol(tabela)]
  tabela[,Czas := as.POSIXct(Data, tz="Europe/Berlin", format=format)]
  tabela[,Data := as.Date(Czas, tz="Europe/Berlin")]
  tabela[,Godzina:=hour(Czas)]
  
  setcolorder(tabela, c("Czas", "Data", "Godzina", nazwy_licznikow))
  tabela[,'Praska sciezka rekreacyjna':=NULL]
  tabela[,'Piesi':=NULL]
  setnames(tabela, 'Rowery', "Praska ścieżka rekreacyjna")
  setnames(tabela, "Dworzec Wileński Nowy( Targowa)", enc2utf8("Dworzec Wileński (Targowa)"))
  
  tabela
}

filtruj_in_out<-function(tabela) {
  nazwy<-names(tabela)
  nazwy_in_out<-c(grep("IN", nazwy, value = TRUE), grep("OUT", nazwy, value = TRUE))
  tabela[,(nazwy_in_out):=NULL]
  tabela
}

wczytaj_dane_godzinowe<-function(plik) {
  library(data.table)
  tabela <- fread(plik, encoding = "UTF-8", header = TRUE)
  tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  tabela
}

wczytaj_dane<-function(plik = "dane_polaczone.csv") {
  tabela<-fread(plik, header = TRUE, encoding = "UTF-8", drop=1) #1st column is just row numers, drop it
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
  setcolorder(tabela, c("Data", "startTyg", "startM", nazwy[order(nazwy)]))
  
  tabela
}

wide_to_long<-function(dane, nazwy_zmiennych=c("Data","startTyg","startM", "temp_min", "temp_avg", "temp_max", "deszcz", "snieg", "Jaki_dzien")) {
  #print(str(dane))
  tabela<-melt(dane, 
               id.vars = nazwy_zmiennych, 
               variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
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


weekend<-function(data) {
  dzien<-weekdays(data)
  ifelse (dzien %in% c("niedziela","sobota", "Sunday", "Sun", "Saturday", "Sat"), "weekend", "roboczy")
}

dodaj_pogode<-function(tabela, 
                       plik_temperatura="pliki/IMGW_temp_20170630.csv", 
                       plik_opady="pliki/IMGW_opady_20170630.csv") {
  temperatura<-fread(plik_temperatura, header = TRUE, encoding = "UTF-8", drop=1)
  opady<-fread(plik_opady, header = TRUE, encoding = "UTF-8", drop=1)
  pogoda<-merge(temperatura, opady, by="Data")
  pogoda[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  pogoda[,Jaki_dzien:=weekend(Data)]
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane
}