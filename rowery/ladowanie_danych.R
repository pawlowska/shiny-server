library(data.table)
library(RCurl)
library(timeDate)
library(lubridate)
library(dplyr)
library(tidyr)

source('hasloA.R', encoding = 'UTF-8')


dodaj_nowe_dane<-function(stare, p="pliki/nowe_long.csv", plik_pogoda, metadane, zakresDo, miasto = "Warszawa") {
  ostatnia_data<-max(stare$Data)
  #dane zaladowane od ostatniego git commit
  if (file.exists(p)) {
    nowe_z_pliku<-wczytaj_dane(p)
    ostatnia_data<-max(nowe_z_pliku$Data)
    tekst="ostatnia data w pliku nowe_long"
  } else {
    nowe_z_pliku<-stare[0,]
    tekst="brak nowe_long, ostatnia data w dane_long"
  }
  cat(file=stderr(), tekst, as.character(ostatnia_data), "\n")
  
  #czy są nowsze dane?  
  if (ostatnia_data<Sys.Date()-1) {
    cat(file=stderr(), "probuje pobrac nowe dane od:", as.character(ostatnia_data), "\n")
    
    #zaladuj
    nowe_z_api<-wczytaj_z_api_v2(credentials, od=as.character(ostatnia_data)) %>%
      json_do_tabeli()
    
    cat(file=stderr(), str(metadane), "\n")
    
    nowe_z_api <- nowe_z_api %>%
      uzupelnij_tabele(metadane, plik_pogoda) 
    cat(file=stderr(), "zaladowane dane z api do:", as.character(max(nowe_z_api$Data)), "\n")

    nowe<-bind_rows(nowe_z_pliku, nowe_z_api)
    #uaktualnij "nowe" dane w pliku
    write.csv(nowe %>% filter(Data>zakresDo), file = p, fileEncoding = 'UTF-8', row.names = F)
  } else {
    nowe<-nowe_z_pliku
  }
  
  
  #polacz ze "starymi" danymi
  dane_long<-bind_rows(stare, nowe %>% filter(Data>zakresDo))
  cat(file=stderr(), "ostatnia uaktualniona data", as.character(max(dane_long$Data)), "\n")
  dane_long
}

wczytaj_z_api_v2<-function(credentials, od="2019-01-01", do=Sys.Date(), miasto="Warszawa") {
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
    summarise(all=sum(cumulative, na.rm=T), today=sum(sincePrevious, na.rm=T), .groups="drop") %>%
    na_if(0) %>%
    filter(description != 'Testowy') %>%
    rename(Data= date, Liczba_rowerow=today, zdm_id=deviceId)
}

#dplyr-style reformatting of data output from json_do_tabeli()
uzupelnij_tabele<-function(tab_in, metadane, plik_pogoda) {
   tab_in %>%
    mutate(Data=as.Date(Data)) %>%
    left_join(select(metadane, c('Miejsce', 'zdm_id')), by='zdm_id') %>%
    select(-c(description, all)) %>%
    sumuj_pary_licznikow(metadane) %>%
    select(-zdm_id) %>%
    mutate(startTyg=as.Date(lubridate::floor_date(Data-days(1), "week")+days(1))) %>%
    data.table() %>%
    dodaj_pogode(plik_pogoda) %>%
    mutate(Jaki_dzien=weekend(Data)) %>%
    mutate(Wolne=wolne(Data))
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


#wide_to_long<-function(dane, nazwy_zmiennych=c("Data","startTyg", "temp_min", "temp_avg", "temp_max", "deszcz", "snieg", "Jaki_dzien","Wolne","Rodzaj_opadu")) {
#  tabela<-melt(dane, 
#               id.vars = nazwy_zmiennych, 
#               variable.name = "Miejsce", value.name = "Liczba_rowerow", na.rm = TRUE)
#}

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
  #takes: date
  #returns: string
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

czytaj_pogode<-function(plik_pogoda) {
  pogoda<-fread(plik_pogoda, header = TRUE, encoding = "UTF-8")
  pogoda[,Data := as.Date(Data, format="%Y-%m-%d")]
  pogoda[,Rodzaj_opadu:=rodzaj_opadu(deszcz, snieg)]
  pogoda
}

dodaj_pogode<-function(tabela, 
                       plik_pogoda="pliki/IMGW_2014_do2020_12.csv") {
  pogoda<-czytaj_pogode(plik_pogoda)
  dane<-merge(tabela, pogoda, by="Data", all.x=TRUE)
  dane
}

### STYLE (kolory, pogrubienia etc)

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


a2=0.7
a3=0.4

zrob_listy_stylow<-function(lokacje, paleta=paleta30) {
  unikaty<-lokacje[has_pair==F]$Miejsce
  pary<-lokacje[has_pair==T]$Miejsce
  ile_unikatow<-length(unikaty)
  
  kolory<-paleta[1:ile_unikatow]
  alfy<-c(rep(1, ile_unikatow))
  
  lista<-data.table(Miejsce=unikaty, kolor=kolory, linia="solid", font="bold", alfa=alfy)
  
  if (length(pary)>0) {
    lista<-rbind(lista, data.table(Miejsce=pary, font="normal"), fill=T)
    lista<-lista[base::order(Miejsce)]
    
    ktory<-1
    kolor<-lista[1]$kolor
    linia<-"dashed"
    alfa<-0.7
    for (i in 1:nrow(lista)) {
      if (lista[i, 'font']=='normal') {
        lista[i, 'kolor']<-kolor
        lista[i]$linia<-linia
        lista[i]$alfa<-alfa
        if(ktory==1) {
          ktory=2
          linia<-"dotdash"
          alfa<-0.4
        }
      } else {
        ktory<-1
        kolor<-lista[i]$kolor
        linia<-"dashed"
        alfa<-0.7
      }
    }
  }
  
  lista
}

css_list<-function(what="#liczniki div.checkbox:nth-child(", style, iterator) {
  paste0(what,
         iterator,
         ") span{color: ", 
         style$kolory[iterator],
         "; font-weight : ",
         style$fonty[iterator],"}")
}