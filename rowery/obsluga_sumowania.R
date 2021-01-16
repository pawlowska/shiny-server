library(data.table)
source('palety_kolorow.R')

sumuj_jeden_dzien<-function(dane_dzienne, wirtualne_liczniki) {
  wirtualne_liczniki %>%
    left_join(dane_dzienne %>% select(-"Miejsce"), by=("zdm_id")) %>%
    group_by(Miejsce) %>%
    summarise(Liczba_rowerow=sum(Liczba_rowerow, na.rm=TRUE), .groups="drop")
}

sumuj_pary_licznikow<-function(dane, metadane) {
  #find virtual counters' names: those with zdm_id==NA
  wirtualne_liczniki<-metadane %>% 
    filter(is.na(zdm_id)) %>% 
    select(-c("latitude", "longitude", "has_pair", "zdm_id", "id"))
  wirtualne_liczniki<-bind_rows(
    wirtualne_liczniki %>%
      mutate(zdm_id=id1),
    wirtualne_liczniki %>%
      mutate(zdm_id=id2)
  ) %>% select(-c("id1", "id2"))
  
  #sum up values for virtual counters and bind with all real counters data
  wszystkie_liczby<-dane %>% 
    group_by(Data) %>%
    group_modify(~sumuj_jeden_dzien(.x, wirtualne_liczniki)) %>%
    rbind(dane) 
}

zrob_sumy<-function(metadane) {
  #tworzy nazwy sum na podstawie nazw licznikow ktore maja pare (has_pair)
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