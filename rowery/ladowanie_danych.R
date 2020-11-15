library(data.table)
library(RCurl)
library(timeDate)
library(lubridate)
library(dplyr)
library(tidyr)

source('hasloA.R', encoding = 'UTF-8')


dodaj_nowe_dane<-function(stare, p="pliki/nowe_long.csv", plik_pogoda, lokacje, zakresDo, miasto = "Warszawa") {
  #dane zaladowane od ostatniego git commit
  if (file.exists(p)) {
    ostatnie_nowe_long<-wczytaj_dane(p)
    ostatnia_data<-max(ostatnie_nowe_long[,Data])
    tekst="ostatnia data w nowe_long"
  } else {
    ostatnie_nowe_long<-stare[0,]
    ostatnia_data<-max(stare[,Data])
    tekst="brak nowe_long, ostatnia data w dane_long"
  }
  cat(file=stderr(), tekst, as.character(ostatnia_data), "\n")
  
  #czy są nowsze dane?  
  if (ostatnia_data<Sys.Date()-1) {
    #zaladuj
    nowe_long<-zaladuj_nowe_z_api(credentials, ostatnia_data, plik_pogoda, lokacje, miasto)
    cat(file=stderr(), "zaladowane dane z api do:", as.character(max(nowe_long[,Data])), "\n")
    
    #polacz
    ostatnie_nowe_long<-rbind(ostatnie_nowe_long[Data<ostatnia_data], nowe_long)
    setorder(ostatnie_nowe_long, "Data")
    #uaktualnij "nowe" dane
    write.csv(ostatnie_nowe_long[Data>zakresDo], file = p, fileEncoding = 'UTF-8')
  }
  
  #polacz ze "starymi" danymi
  dane_long<-rbind(stare, ostatnie_nowe_long[Data>zakresDo])
  cat(file=stderr(), "ostatnia uaktualniona data", as.character(max(dane_long[,Data])), "\n")
  dane_long
}

zaladuj_nowe_z_api<-function(credentials, ostatnia_data, plik_pogoda, lokacje, miasto = "Warszawa") {
  klucz <- lokacje[,c("id", "Miejsce")]
  
  nowe_dane<-wczytaj_z_api(credentials, klucz=klucz, od=ostatnia_data, miasto = miasto)
  nowe_dane<-suma_licznikow(numery_dat(nowe_dane))
  nowe_long<-wide_to_long(dodaj_pogode(nowe_dane, plik_pogoda))
}


wczytaj_z_api_v2<-function(credentials, klucz=klucz, od="2019-01-01", do=Sys.Date(), miasto="Warszawa") {
  link <- URLencode(paste('http://greenelephant.pl/rowery/api/v2/index.php?od=',od,'&do=',do,sep=""))
  json<- jsonlite::fromJSON(getURL(link, userpwd = credentials))
  json
}

json_do_tabeli<-function(big_json) {
  big_json %>% 
    unnest(counters) %>% 
    select(-c(installationDate, mapLatitude, mapLongitude)) %>% 
    unnest(dailyCounts)  %>% 
    group_by(deviceId, description, date) %>% 
    summarise(all=sum(cumulative, na.rm=T), today=sum(sincePrevious, na.rm=T)) %>%
    ungroup() %>%
    na_if(0) %>%
    filter(description != 'Testowy') %>%
    rename(Data= date, Liczba_rowerow=today, zdm_id=deviceId)
}

uzupelnij_tabele<-function(tab_in, metadane, plik_pogoda) {
  tab_in %>%
    mutate(Data=as.Date(Data)) %>%
    mutate(startTyg=as.Date(lubridate::floor_date(Data-days(1), "week")+days(1))) %>%
    left_join(select(metadane, c('Miejsce', 'zdm_id')), by='zdm_id') %>%
    select(-c(description, all, zdm_id)) %>%
    spread(Miejsce, Liczba_rowerow) %>%
    data.table() %>%
    suma_licznikow() %>% 
    gather(key="Miejsce", value="Liczba_rowerow", -Data, -startTyg) %>%
    data.table() %>%
    dodaj_pogode(plik_pogoda)

    #teraz można ić za ciosem i dodać pogode  
}

# wczytaj_dane_godzinowe<-function(plik) {
#   library(data.table)
#   tabela <- fread(plik, encoding = "UTF-8", header = TRUE)
#   tabela[,Data := as.Date(Data, tz="Europe/Berlin", format="%Y-%m-%d")]
#   tabela
# }

wczytaj_dane<-function(plik = "dane_polaczone.csv") {
  tabela<-fread(plik, header = TRUE, encoding = "UTF-8") %>%
    mutate(Data=as.Date(Data), startTyg=as.Date(startTyg))
}


#long_to_wide<-function(dane, nazwa_zmiennej="Liczba_rowerow") {
#  tabela_wide<-dcast(dane, Data ~Miejsce, value.var=nazwa_zmiennej)
#  tabela_wide
#}


wide_to_long<-function(dane, nazwy_zmiennych=c("Data","startTyg", "temp_min", "temp_avg", "temp_max", "deszcz", "snieg", "Jaki_dzien","Wolne","Rodzaj_opadu")) {
  tabela<-melt(dane, 
               id.vars = nazwy_zmiennych, 
               variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
}

podsumuj.okresy<-function(tabela, kolumna) {
  podsumowanie <- tabela[,sum(Liczba_rowerow), by=c("Miejsce", kolumna)]
  setnames(podsumowanie, c(kolumna,"V1"), c("Data", "Liczba_rowerow"))
  podsumowanie
}

#podsumowanie po roku danych w formacie long
podsumuj.lata <- function(tabela_long) {
  podsumowanie <- tabela_long[,sum(Liczba_rowerow), by=.(Miejsce, cut.POSIXt(as.POSIXct(Data), breaks="year"))]
  setnames(podsumowanie, c("cut.POSIXt","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie[,Data:=as.Date(as.character(Data))]
  podsumowanie
}

#podsumowanie po miesiącu danych w formacie long
podsumuj.miesiace <- function(tabela_long) {
  podsumowanie <- tabela_long[,sum(Liczba_rowerow), by=.(Miejsce, cut.POSIXt(as.POSIXct(Data), breaks="month"))]
  setnames(podsumowanie, c("cut.POSIXt","V1"), c("Data", "Liczba_rowerow"))
  podsumowanie[,Data:=as.Date(as.character(Data))]
  podsumowanie
}

#PZ
podsumuj.procentowo <- function(tabela_long) { 
  podsumowanie <- tabela_long[,100*Liczba_rowerow/sum(tabela_long$Liczba_rowerow[tabela_long$Data == Data]), by=.(Miejsce,Data)]
  setnames(podsumowanie, c("Data","V1"), c("Data", "Liczba_rowerow"))
  if("Wolne" %in% colnames(tabela_long)) {
    podsumowanie<-merge(podsumowanie, unique(tabela_long[,c("Data", "Wolne")]), by="Data")
  }
  podsumowanie
}
#PZ koniec

weekend<-function(data) {
  dzien<-weekdays(data)
  ifelse (dzien %in% c("niedziela","sobota", "Sunday", "Sun", "Saturday", "Sat"), "weekend", "roboczy")
}

swieto<-function(data) {
  rok<-year(data)
  swieta<-c(as.Date(c(
    NewYearsDay(rok),
    Epiphany(rok),
    EasterSunday(rok),
    EasterMonday(rok),
    LaborDay(rok),
    Pentecost(rok),
    CorpusChristi(rok),
    AssumptionOfMary(rok),
    AllSaints(rok),
    ChristmasDay(rok),
    BoxingDay(rok)
  )),as.Date(paste(rok,"-05-03", sep="")),as.Date(paste(rok,"-11-11", sep="")))
  ifelse (as.Date(data) %in% swieta, "swieto", "nieswieto")
}

wolne<-function(data) {
  (weekend(data)=="weekend")|(swieto(data)=="swieto")
}

rodzaj_opadu<-function(d,s) {
  ifelse (d>0,'d','s')
}

dodaj_pogode<-function(tabela, 
                       plik_pogoda="pliki/IMGW_pogoda_20171231.csv") {
  pogoda<-fread(plik_pogoda, header = TRUE, encoding = "UTF-8")
  pogoda[,Data := as.Date(Data, format="%Y-%m-%d")]
  pogoda[,Rodzaj_opadu:=rodzaj_opadu(deszcz, snieg)]
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane[,Jaki_dzien:=weekend(Data)]
  dane[,Wolne:=wolne(Data)]
  
  dane
}

wczytaj_style<-function(katalog) {
  #reading colors etc
  listy_stylow<-data.table(read.csv(file = paste(katalog, "listy_stylow.csv", sep="/"), 
                                    fileEncoding = 'UTF-8', colClasses = "character"))
  koloryLicznikow<-listy_stylow$kolor
  names(koloryLicznikow)<-listy_stylow$Miejsce
  linieLicznikow<-listy_stylow$linia
  names(linieLicznikow)<-listy_stylow$Miejsce
  alfyLicznikow<-listy_stylow$alfa
  names(alfyLicznikow)<-listy_stylow$Miejsce
  fonty<-listy_stylow$font
  names(fonty)<-listy_stylow$Miejsce
  style<-list(kolory=koloryLicznikow, linie=linieLicznikow, alfy=alfyLicznikow, fonty=fonty)
  style
}