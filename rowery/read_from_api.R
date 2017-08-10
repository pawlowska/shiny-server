library(jsonlite)
library(data.table)
library(RCurl)

source('hasloA.R', encoding = 'UTF-8')

read_counterids<-function(filename="counterids.json") {
  ids<-read_json(filename,  simplifyVector = TRUE)
  ids
}

zaladuj_dane_api<-function(filename="bike_counts.csv") {
  link <- 'http://greenelephant.pl/rowery/api/v1/'
  txt<- getURL(link, userpwd = credentials)
  #txt<- getURL(link)
  tabela<-data.table(read.csv(text=txt, sep=',', header=FALSE))
  setnames(tabela, c("Licznik", "Data", "Liczba_rowerow"))
  tabela[,Data:=as.Date(Data)]
  tabela[,Miejsce:=as.character(Licznik)]
  tabela[,Miejsce:=unlist(ids[Miejsce])]
  tabela_wide<-dcast(tabela, Data ~Miejsce, value.var="Liczba_rowerow")
  tabela_wide
}