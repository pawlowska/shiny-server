podw<-rbind(c("Al. USA - południe", "Al. USA - północ"),
            c("Most Gdański - ciąg pieszo-rowerowy", "Most Gdanski - ścieżka rowerowa"),
            c("NSR - Solec - ciąg pieszo-rowerowy", "NSR-Solec - ścieżka rowerowa"),
            c("Świętokrzyska - Emilii Plater, płd", "Świętokrzyska - Emilii Plater, płn"),
            c("Żwirki i Wigury/Trojdena, zach", "Żwirki i Wigury/Trojdena, wsch")
)
sumy <-c("Al. USA - suma",
         "Most Gdański - suma",
         "NSR - Solec - suma",
         "Świętokrzyska - Emilii Plater - suma", 
         "Żwirki i Wigury/Trojdena - suma")

suma_licznikow<-function(tabela) {
  nazwy_x<-names(tabela)[1:3]
  nazwy<-names(tabela)[4:ncol(tabela)]

  nazwy_bez_podw<-nazwy[!(nazwy %in% podw)]
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


source('palety_kolorow.R')

ile_unikatow<-14

zrob_listy_stylow<-function(tabela) {
  nazwy<-names(tabela)[4:ncol(tabela)]
  nazwy_bez_podw<-nazwy[!(nazwy %in% podw)]

  
  kolory_bez_podw<-paleta14_sorted
  kolory<-kolory_bez_podw
  linie<-c(rep("solid", ile_unikatow))
  fonty<-c(rep("bold", ile_unikatow))
  
  for (s in sumy) {
    pos<-match(s, nazwy_bez_podw)

    kolory<-append(kolory, rep(kolory[pos], 2), after = pos)
    linie<-append(linie, c("dashed", "dotdash"), after = pos)
    fonty<-append(fonty, rep("normal", 2), after = pos)
    nazwy_bez_podw<-append(nazwy_bez_podw, c('x','x'), after = pos)
    
  }
  
  rbind(kolory, linie, fonty)
}