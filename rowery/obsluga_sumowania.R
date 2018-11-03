library(data.table)
source('palety_kolorow.R')

zrob_sumy<-function(metadane) {
  metadane[,has_pair:=!is.na(pair_id)]
  do_sumowania<-metadane[has_pair==T,]
  tabela<-metadane[,c('id', 'Miejsce', 'latitude', 'longitude', 'has_pair')]
  if (nrow(do_sumowania)>0) {
    find.string <- paste(c('N$', 'S$', 'E$', 'W$', 'CPR$', 'DDR$'), collapse = "|")
    substrings<-unique(gsub(find.string, replacement = "", x = do_sumowania$Miejsce))
    sumy<-sapply(substrings, function(x) paste(x, '- suma', sep=""), USE.NAMES = F)
    tabela_sumy<-data.table(Miejsce = sumy, has_pair=F)
    tabela<-rbind(tabela, tabela_sumy, fill=T)
  }
  tabela<-tabela[base::order(Miejsce)]
  tabela
}


znajdz_prefix<-function(s, koncowka=" - suma") {
  substring(s, 0, regexpr(koncowka, s)-1)
}

suma_licznikow<-function(tabela) {
  nazwy_x<-names(tabela)[1:2] #Data, startTyg
  nazwy<-names(tabela)[3:ncol(tabela)] #reszta
  
  find.string <- paste(c('N$', 'E$', 'CPR$'), collapse = "|")
  podwojne_liczniki<-grep(find.string, nazwy, value = T)
  podw<-znajdz_prefix(grep(find.string, nazwy, value = T), find.string)

  if(length(podw)>0) {
    
    i<-1
    for (p in podw) {
      s<-paste(p, "- suma", sep="")
      sumuj<-grep(podw[i], nazwy, value = T)
      tabela[,(s):=get(sumuj[1])+get(sumuj[2])]
      i<-i+1
    }
    
    nazwy<-names(tabela)[3:ncol(tabela)] #reszta
    nazwy<-sort(nazwy)
    
    kolejnosc <-c(nazwy_x, nazwy)
    setcolorder(tabela, kolejnosc)
  }
  
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