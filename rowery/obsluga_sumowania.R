library(data.table)
source('palety_kolorow.R')


czytaj_nazwy<-function(plik="pliki/polozenie_licznikow.csv") {
  lokacje <- data.table(read.csv("pliki/polozenie_licznikow.csv",dec=",", encoding='UTF-8'))
  miejsca<-lokacje[,Miejsce]
}

znajdz_podwojne<-function() {
  miejsca<-czytaj_nazwy()
  grep("suma", miejsca, value = T)
  
}

znajdz_prefix<-function(s) {
  substring(s, 0, regexpr(" - suma", s)-1)
}

sumy_zwykle<-znajdz_podwojne()
podwojne_prefix<-znajdz_prefix(sumy_zwykle)
podwojne_i_sumy<-grep(paste(podwojne_prefix,collapse="|"), czytaj_nazwy(), value = TRUE)


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


a1=1
a2=0.7
a3=0.4

zrob_listy_stylow<-function(nazwy, podw_sumy=podwojne_i_sumy, sumy=sumy_zwykle, paleta=paleta20_sorted) {
  nazwy_bez_podw<-sort(c(nazwy[!(nazwy %in% podw_sumy)], sumy))
  ile_unikatow<-length(nazwy_bez_podw)
  
  kolory_bez_podw<-paleta
  kolory<-kolory_bez_podw[1:ile_unikatow]
  linie<-c(rep("solid", ile_unikatow))
  fonty<-c(rep("bold", ile_unikatow))
  alfy<-c(rep(a1, ile_unikatow))
  
  for (s in sumy) {
    pos<-match(s, nazwy_bez_podw)

    kolory<-append(kolory, rep(kolory[pos], 2), after = pos)
    linie<-append(linie, c("dashed", "dotdash"), after = pos)
    alfy<-append(alfy, c(a2,a3), after = pos)
    
    fonty<-append(fonty, rep("normal", 2), after = pos)
    nazwy_bez_podw<-append(nazwy_bez_podw, c('x','x'), after = pos)
    
  }
  
  cbind(nazwy, kolory, linie, fonty, alfy)
}

css_list<-function(what="#liczniki div.checkbox:nth-child(", listy_stylow, iterator) {
  paste0(what,
         iterator,
         ") span{color: ", 
         listy_stylow$kolory[iterator],
         "; font-weight : ",
         listy_stylow$fonty[iterator],"}")
}