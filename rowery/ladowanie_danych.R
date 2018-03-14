library(data.table)
library(RCurl)
#library(jsonlite)

source('hasloA.R', encoding = 'UTF-8')


zaladuj_nowe_z_api<-function(ostatnia_data, plik_pogoda, lokacje) {
  klucz <- lokacje[,c("id", "Miejsce")]
  
  nowe_dane<-wczytaj_z_api(klucz=klucz, od=ostatnia_data)
  nowe_dane<-suma_licznikow(numery_dat(nowe_dane))
  nowe_long<-wide_to_long(dodaj_pogode(nowe_dane, plik_pogoda))
}

wczytaj_z_api<-function(klucz=klucz, od="2018-02-01", do=Sys.Date()) {
  link <- paste('http://greenelephant.pl/rowery/api/v1/?start=',od,'&end=',do)
  #print(link)
  txt<- getURL(link, userpwd = credentials)
  tabela<-data.table(read.csv(text=txt, sep=',', header=FALSE))
  setnames(tabela, c("id", "Data", "Liczba_rowerow"))
  tabela[,Data:=as.Date(Data)]
  tabela<-merge(tabela, klucz, by="id")
  tabela_wide<-dcast(tabela, Data ~Miejsce, value.var="Liczba_rowerow")
  tabela_wide
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
  nazwy<-names(tabela)[2:ncol(tabela)]
  library(lubridate)
  #uses lubridate; correction to make the week start Monday
  tabela[,startTyg:=floor_date(Data-days(1), "week")+days(1)]
  tabela[,startM:=floor_date(Data, "month")]
  setcolorder(tabela, c("Data", "startTyg", "startM", nazwy[order(nazwy)]))
  
  tabela
}

wide_to_long<-function(dane, nazwy_zmiennych=c("Data","startTyg","startM", "temp_min", "temp_avg", "temp_max", "deszcz", "snieg", "Jaki_dzien","Rodzaj_opadu")) {
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

#podsumowanie po stratY danych w formacie long
podsumuj.lata <- function(tabela_long) {
  podsumowanie <- tabela_long[,sum(Liczba_rowerow), by=.(Miejsce, cut.POSIXt(as.POSIXct(Data), breaks="year"))]
  setnames(podsumowanie, c("cut.POSIXt","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie[,Data:=as.Date(as.character(Data))]
  podsumowanie
}

#PZ
podsumuj.procentowo <- function(tabela_long) { 
  podsumowanie <- tabela_long[,100*Liczba_rowerow/sum(tabela_long$Liczba_rowerow[tabela_long$Data == Data]), by=.(Miejsce,Data)]
  setnames(podsumowanie, c("Data","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie
}
#PZ koniec

weekend<-function(data) {
  dzien<-weekdays(data)
  ifelse (dzien %in% c("niedziela","sobota", "Sunday", "Sun", "Saturday", "Sat"), "weekend", "roboczy")
}

rodzaj_opadu<-function(d,s) {
  ifelse (d>0,'d','s')
}

dodaj_pogode<-function(tabela, 
                       plik_pogoda="pliki/IMGW_pogoda_20171231.csv") {
  pogoda<-fread(plik_pogoda, header = TRUE, encoding = "UTF-8")
  pogoda[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
  pogoda[,Jaki_dzien:=weekend(Data)]
  pogoda[,Rodzaj_opadu:=rodzaj_opadu(deszcz, snieg)]
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane
}