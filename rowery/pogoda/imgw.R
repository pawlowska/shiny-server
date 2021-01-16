library(data.table)

warszawaOkecie="352200375"
krakowBalice="350190566"

nazwy_kolumn<-c('r', 'm', 'd', 'temp_max', 'temp_min', 'temp_avg', 'opad', 'rodzaj')


czytaj_dane_zipy<-function(od=1, do=1, save= T, nazwa_out="pogoda/IMGW_2020_0104.csv", 
                           format='pogoda/raw/s_d_%02d_2020.csv', stacja=warszawaOkecie) {
  pogoda<-data.table(matrix(nrow = 0, ncol = 8))
  setnames(pogoda, names(pogoda), nazwy_kolumn)
  for (i in od:do) {
    nazwa<-sprintf(format,i)
    dane<-fread(nazwa)[V1==stacja]
    dane<-dane[,c('V3', 'V4', 'V5', 'V6', 'V8', 'V10', 'V14', 'V16') ]
    setnames(dane, names(dane), nazwy_kolumn)
    pogoda<-rbind(pogoda, dane)
  }
  pogoda[,Data:=paste(r,m,d, sep='-')]
  pogoda[,c('r','m','d'):=NULL]
  pogoda[,deszcz:=ifelse(rodzaj=='W', opad, 0)]
  pogoda[,snieg:= ifelse(rodzaj=='S', opad, 0)]
  setcolorder(pogoda, c('Data', nazwy_kolumn[4:8], 'deszcz', 'snieg'))
  pogoda[,c('opad','rodzaj'):=NULL]
  if (save) {write.csv(pogoda, file = nazwa_out, fileEncoding = 'UTF-8', row.names = F)}
  pogoda
}

czytaj_dane_po_stacji<-function(nazwa_out="pogoda/IMGW_2019.csv", nazwa='pogoda/raw/s_d_t_375_2019.csv') {
  dane<-fread(nazwa,colClasses="numeric")
  pogoda<-dane[,c('V3', 'V4', 'V5', 'V6', 'V8', 'V10', 'V14', 'V16') ]
  
  print(str(pogoda))
  
  setnames(pogoda, names(pogoda), nazwy_kolumn)
 
  pogoda[,Data:=as.Date(paste(r,m,d, sep='-'))]
  pogoda[,c('r','m','d'):=NULL]
  pogoda[,deszcz:=ifelse(rodzaj=='W', opad, 0)]
  pogoda[,snieg:= ifelse(rodzaj=='S', opad, 0)]
  setcolorder(pogoda, c('Data', nazwy_kolumn[4:8], 'deszcz', 'snieg'))
  write.csv(pogoda, file = nazwa_out, fileEncoding = 'UTF-8', row.names = F)
  pogoda
}