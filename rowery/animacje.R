library(dplyr)
library(tidyr)
library(stringr)
library(leaflet.minicharts)


dane_do_animacji <- function (){
  
  #wczytujemy lokacje
  lokacje <- read.csv("pliki/polozenie_licznikow.csv",dec=",", encoding='UTF-8')

  #to jest jedyny plik z danymi kierunkowymi - godzinowymi. 
  dane_kierunkowe_godzinowe <- zaladuj_dane_godzinowe('dane/2017_08_05-14_06.csv', format="%y-%m-%d %H:%M", ziw=F, bez_kierunkow = F)

  #wektor uzgadniający różnice między nazwami czujników w danych z lokacją i danych godzinowych
  translate <- c("Al. USA - południe" = "Al. USA płd.", 
                 "Al. USA - północ" = "Al. USA płn.",
                 "Dworzec Wileński \\(Al. Solidarności\\)" = "Dworzec Wileński (al. Solidarności)",
                 "Dworzec Wileński Nowy" = "Dworzec Wileński (Targowa)",
                 "Świętokrzyska - Emilii Plater, płd" = "Świętokrzyska/Emilii Plater płd.",
                 "Świętokrzyska - Emilii Plater, płn" = "Świętokrzyska/Emilii Plater płn.")

  #Modyfikacja danych: wyszukanie znaczników IN/OUT; policzenie proporcji;
  dane_modyfikacja <- gather(dane_kierunkowe_godzinowe, key = 'Miejsce', value = 'Liczba',
                    names(dane_kierunkowe_godzinowe)[names(dane_kierunkowe_godzinowe) %like% '^.+IN|OUT$']) %>%
    select("Czas","Liczba","Miejsce")%>%
    mutate (in_out = sub('[ _]','',str_sub(Miejsce,start=-3)),  Miejsce = sub('[ _]IN|[ _]OUT$', '', Miejsce))%>%
    spread (in_out,Liczba) %>% mutate (Miejsce = str_replace_all(Miejsce,translate)) %>% 
    mutate(opis = Miejsce) %>%
    merge(lokacje,by = 'Miejsce', all.x=T) %>%
    mutate (perc_ruch =  (IN+OUT)/max(IN+OUT)) %>%
    plyr::rename(replace = c('IN' = 'Do centrum', 'OUT' = 'Od centrum'))
  
  return(dane_modyfikacja)
}
