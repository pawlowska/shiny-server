library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')

godz2014<-zaladuj_dane_godzinowe('dane/godz-2014.csv')
godz2015<-zaladuj_dane_godzinowe('dane/godz-2015.csv')
godz2016<-zaladuj_dane_godzinowe('dane/godz-2016.csv', format="%y-%m-%d %H:%M")
godz2017<-zaladuj_dane_godzinowe('dane/godz-2017-old.csv')
#godz2017_new<-zaladuj_dane_godzinowe('dane/godz-2017.csv', format="%y-%m-%d %H:%M", bez_kierunkow=F, ziw=F)

godz<-rbind(godz2014, godz2015, godz2016, godz2017)
#godz[,Jaki_dzien:=weekend(Data)]
write.csv(godz, file = "godz2014-2017.csv", fileEncoding = 'UTF-8', row.names = FALSE)
#wide to long
godz_long<-wide_to_long(godz, c("Czas", "Data", "Godzina"))
dane_dobowe_long<-godz_long[,sum(Liczba_rowerow), by=c("Data", "Miejsce")]
setnames(dane_dobowe_long, 'V1',"Liczba_rowerow")

dane_dobowe<-dcast(dane_dobowe_long, Data~Miejsce, value.var = "Liczba_rowerow")
dane_dobowe<-dane_dobowe[1:(nrow(dane_dobowe)-1)]
write.csv(dane_dobowe, file = "dane-20140813-20170202.csv", fileEncoding = 'UTF-8', row.names = FALSE)

