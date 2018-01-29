library(jsonlite)
#library(data.table)
library(RCurl)

source('hasloA.R', encoding = 'UTF-8')

read_counterids<-function(filename="pliki/counterids.json") {
  ids<-read_json(filename,  simplifyVector = TRUE)
  ids
}

zaladuj_dane_api<-function(ids=ids, od="2018-01-01", do=Sys.Date()) {
  link <- paste('http://greenelephant.pl/rowery/api/v1/?start=',od,'&end=',do)
  txt<- getURL(link, userpwd = credentials)
  tabela<-data.table(read.csv(text=txt, sep=',', header=FALSE))
  setnames(tabela, c("Licznik", "Data", "Liczba_rowerow"))
  #tabela<-tabela[Licznik!=100042112 & Licznik!=100042111] #powsinska
  tabela[,Data:=as.Date(Data)]
  tabela[,Miejsce:=as.character(Licznik)]
  tabela[,Miejsce:=unlist(ids[Miejsce])]
  tabela_wide<-dcast(tabela, Data ~Miejsce, value.var="Liczba_rowerow")
  tabela_wide
}