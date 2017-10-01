podwojne_prefix<-enc2utf8(c("Al. Jerozolimskie", 
                            "Al. USA",
                            "NSR Most Gdański",
                            "NSR Solec",
                            "Świętokrzyska/Emilii Plater",
                            "Żwirki i Wigury/Trojdena"))

sumy_zwykle<-paste(podwojne_prefix,"- suma")

podwojne<-enc2utf8(rbind(c("Al. USA - południe", "Al. USA - północ"),
            c("Most Gdański - ciąg pieszo-rowerowy", "Most Gdanski - ścieżka rowerowa"),
            c("NSR - Solec - ciąg pieszo-rowerowy", "NSR-Solec - ścieżka rowerowa"),
            c("Świętokrzyska - Emilii Plater, płd", "Świętokrzyska - Emilii Plater, płn"),
            c("Żwirki i Wigury/Trojdena zach.", "Żwirki i Wigury/Trojdena wsch."))
)

podw_in_out<-enc2utf8(rbind(podwojne,
                   c("Al. USA - południe IN", "Al. USA - północ IN"),
                   c("Al. USA - południe OUT", "Al. USA - północ OUT"),
                   c("Świętokrzyska - Emilii Plater, płd IN", "Świętokrzyska - Emilii Plater, płn IN"),
                   c("Świętokrzyska - Emilii Plater, płd OUT", "Świętokrzyska - Emilii Plater, płn OUT"),
                   c("Żwirki i Wigury/Trojdena wsch. IN", "Żwirki i Wigury/Trojdena zach. IN"),
                   c("Żwirki i Wigury/Trojdena wsch. OUT","Żwirki i Wigury/Trojdena zach. OUT")))

sumy_in_out<-enc2utf8(c(sumy_zwykle,
         "Al. USA - suma IN", "Al. USA - suma OUT", 
         "Świętokrzyska - Emilii Plater - suma IN", "Świętokrzyska - Emilii Plater - suma OUT",
         "Żwirki i Wigury/Trojdena - suma IN", "Żwirki i Wigury/Trojdena - suma OUT"))

suma_licznikow<-function(tabela, podw=podwojne_prefix) {
  nazwy_x<-names(tabela)[1:3] #Czas, Data, Godzina
  nazwy<-names(tabela)[4:ncol(tabela)] #reszta
  
  i<-1
  for (p in podw) {
    s<-paste(p, "- suma")
    sumuj<-grep(podw[i], nazwy, value = T)
    tabela[,(s):=get(sumuj[1])+get(sumuj[2])]
    i<-i+1
  }

  nazwy<-names(tabela)[4:ncol(tabela)] #reszta
  nazwy<-sort(nazwy)
  
  kolejnosc <-c(nazwy_x, nazwy)
  setcolorder(tabela, kolejnosc)
  tabela
}


suma_licznikow_old<-function(tabela, podw=podwojne, sumy=sumy_zwykle) {
  nazwy_x<-names(tabela)[1:3] #Czas, Data, Godzina
  nazwy<-names(tabela)[4:ncol(tabela)] #reszta
  
  missing <- setdiff(podw, names(tabela))
  tabela[,(missing):=as.integer(NA)]

  nazwy_bez_podw<-nazwy[!(nazwy %in% podw)] #tych sumowanie nie dotyczy
  kolej_unikat<-sort(c(nazwy_bez_podw, sumy))
  kolej<-kolej_unikat
  i<-1
  for (s in sumy) {
    tabela[,(s):=get(podw[i,1])+get(podw[i,2])]
    pos<-match(s, kolej)
    n<-length(kolej)
    kolej<-append(kolej, podw[i,], after = pos)
    i<-i+1
  }
  kolejnosc <-c(nazwy_x, kolej)
  setcolorder(tabela, kolejnosc)
  tabela
}


in_out_ratio<-function(tabela) {
  nazwy<-names(tabela)
  nazwy_in <-grep("IN", nazwy, value = TRUE)
  nazwy_out<-grep("OUT", nazwy, value = TRUE)
  i=1
  for (n_in in nazwy_in) {
    n_ratio<-gsub(" IN", " ratio", n_in)
    tabela[,(n_ratio):=(get(nazwy_in[i])-get(nazwy_out[i]))/(get(nazwy_in[i])+get(nazwy_out[i]))]
    i<-i+1
  }
  tabela
}

source('palety_kolorow.R')

ile_unikatow<-16

zrob_listy_stylow<-function(nazwy, podw=podwojne_prefix, sumy=sumy_zwykle) {
  podwojne_i_sumy<-grep(paste(podwojne_prefix,collapse="|"), nazwy, value = TRUE)
  nazwy_bez_podw<-sort(c(nazwy[!(nazwy %in% podwojne_i_sumy)], sumy))
  
  kolory_bez_podw<-paleta16_sorted
  kolory<-kolory_bez_podw[1:ile_unikatow]
  linie<-c(rep("solid", ile_unikatow))
  fonty<-c(rep("bold", ile_unikatow))
  
  for (s in sumy) {
    pos<-match(s, nazwy_bez_podw)

    kolory<-append(kolory, rep(kolory[pos], 2), after = pos)
    linie<-append(linie, c("dashed", "dotdash"), after = pos)
    fonty<-append(fonty, rep("normal", 2), after = pos)
    nazwy_bez_podw<-append(nazwy_bez_podw, c('x','x'), after = pos)
    
  }
  
  cbind(kolory, linie, fonty)
}