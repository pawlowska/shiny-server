library(data.table)
source('palety_kolorow.R')

zrob_sumy<-function(metadane) {
  find.string <- paste(c('N$', 'S$', 'E$', 'W$', 'CPR$', 'DDR$'), collapse = "|")
  metadane[,has_pair:=!is.na(pair_id)]
  do_sumowania<-metadane[has_pair==T,]
  substrings<-unique(gsub(find.string, replacement = "", x = do_sumowania$Miejsce))
  sumy<-sapply(substrings, function(x) paste(x, '- suma', sep=""), USE.NAMES = F)
  tabela<-metadane[,c('id', 'Miejsce', 'latitude', 'longitude', 'has_pair')]
  tabela_sumy<-data.table(Miejsce = sumy, has_pair=F)
  tabela<-rbind(tabela, tabela_sumy, fill=T)
  tabela<-tabela[base::order(Miejsce)]
  tabela
}

czytaj_nazwy<-function(plik="pliki/polozenie_licznikow.csv") {
  lokacje <- data.table(read.csv("pliki/polozenie_licznikow.csv", encoding='UTF-8'))
  miejsca<-lokacje[,Miejsce]
}

znajdz_podwojne<-function() {
  miejsca<-czytaj_nazwy()
  grep("suma", miejsca, value = T)
  
}

znajdz_prefix<-function(s) {
  substring(s, 0, regexpr(" - suma", s)-1)
}

#sumy_zwykle<-znajdz_podwojne()
#podwojne_prefix<-znajdz_prefix(sumy_zwykle)
#podwojne_i_sumy<-grep(paste(podwojne_prefix,collapse="|"), czytaj_nazwy(), value = TRUE)


# podw_in_out<-enc2utf8(rbind(podwojne,
#                    c("Al. USA - południe IN", "Al. USA - północ IN"),
#                    c("Al. USA - południe OUT", "Al. USA - północ OUT"),
#                    c("Świętokrzyska - Emilii Plater, płd IN", "Świętokrzyska - Emilii Plater, płn IN"),
#                    c("Świętokrzyska - Emilii Plater, płd OUT", "Świętokrzyska - Emilii Plater, płn OUT"),
#                    c("Żwirki i Wigury/Trojdena wsch. IN", "Żwirki i Wigury/Trojdena zach. IN"),
#                    c("Żwirki i Wigury/Trojdena wsch. OUT","Żwirki i Wigury/Trojdena zach. OUT")))
# 
# sumy_in_out<-enc2utf8(c(sumy_zwykle,
#          "Al. USA - suma IN", "Al. USA - suma OUT", 
#          "Świętokrzyska - Emilii Plater - suma IN", "Świętokrzyska - Emilii Plater - suma OUT",
#          "Żwirki i Wigury/Trojdena - suma IN", "Żwirki i Wigury/Trojdena - suma OUT"))

suma_licznikow<-function(tabela, podw=podwojne_prefix) {
  nazwy_x<-names(tabela)[1:2] #Data, startTyg
  nazwy<-names(tabela)[3:ncol(tabela)] #reszta
  
  i<-1
  for (p in podw) {
    s<-paste(p, "- suma")
    sumuj<-grep(podw[i], nazwy, value = T)
    tabela[,(s):=get(sumuj[1])+get(sumuj[2])]
    i<-i+1
  }

  nazwy<-names(tabela)[3:ncol(tabela)] #reszta
  nazwy<-sort(nazwy)
  
  kolejnosc <-c(nazwy_x, nazwy)
  setcolorder(tabela, kolejnosc)
  tabela
}

# in_out_ratio<-function(tabela) {
#   nazwy<-names(tabela)
#   nazwy_in <-grep("IN", nazwy, value = TRUE)
#   nazwy_out<-grep("OUT", nazwy, value = TRUE)
#   i=1
#   for (n_in in nazwy_in) {
#     n_ratio<-gsub(" IN", " ratio", n_in)
#     tabela[,(n_ratio):=(get(nazwy_in[i])-get(nazwy_out[i]))/(get(nazwy_in[i])+get(nazwy_out[i]))]
#     i<-i+1
#   }
#   tabela
# }


a2=0.7
a3=0.4

zrob_listy_stylow<-function(lokacje, paleta=paleta30) {
  unikaty<-lokacje[has_pair==F]$Miejsce
  pary<-lokacje[has_pair==T]$Miejsce
  ile_unikatow<-length(unikaty)
  
  kolory<-paleta[1:ile_unikatow]
  alfy<-c(rep(1, ile_unikatow))
  
  lista<-data.table(Miejsce=unikaty, kolor=kolory, linia="solid", font="bold", alfa=alfy)
  reszta_listy<-data.table(Miejsce=pary, font="normal")
  
  lista<-rbind(lista, reszta_listy, fill=T)
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