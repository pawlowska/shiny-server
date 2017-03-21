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

suma_licznikow<-function(tabela) {
  nazwy_x<-names(tabela)[1:3]
  nazwy<-names(tabela)[4:ncol(tabela)]
  podw1 <- c("Al. USA - południe", "Al. USA - północ")
  podw2 <- c("Świętokrzyska - Emilii Plater, płd", "Świętokrzyska - Emilii Plater, płn")
  podw3 <- c("Żwirki i Wigury/Trojdena1", "Żwirki i Wigury/Trojdena2")
  sumy <-c("Al. USA - suma","Świętokrzyska - Emilii Plater - suma", "Żwirki i Wigury/Trojdena - suma")
  tabela[,sumy[1]:=get(podw1[1])+get(podw1[2])]
  tabela[,sumy[2]:=get(podw2[1])+get(podw2[2])]
  tabela[,sumy[3]:=get(podw3[1])+get(podw3[2])]
  kolejnosc <-c(nazwy_x, sumy[1], nazwy[3:15], sumy[2], sumy[3], podw1, podw2, podw3)
  setcolorder(tabela, kolejnosc)
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