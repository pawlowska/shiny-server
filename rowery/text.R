library(shiny)

tekst_zdm1<-tags$p('W 2014 r. zainstalowane zostały pierwsze automatyczne liczniki rowerów. 
                   Urządzenia te funkcjonują na zasadzie pętli indukcyjnej zamieszczonej w nawierzchni, która wyczuwa przejeżdżające rowery i wysyła informacje na temat ich liczby. 
                   Dane pozwalają precyzyjnie określić m. in. wpływ pory dnia, pogody i pory roku na natęzenie ruchu rowerowego. 
                   Sieć liczników jest stale rozbudowywana o kolejne urządzenia montowane w ramach nowych inwestycji rowerowych oraz przy okazji remontów istniejącej infrastruktury.')

tekst_zdm2<-tags$p('Informacje o pogodzie dla Warszawy pochodzą z ',
                   tags$a(href='https://dane.imgw.pl', 'Instytutu Meteorologii i Gospodarki Wodnej.'))

tekst_zdm3<-tags$p('Aplikacja do wizualizacji danych z liczników opracowana przez in1woord (więcej informacji',
                   tags$a(href='https://greenelephant.pl/rowery/', 'tutaj'),'). 
                   Kontakt w sprawach techicznych: ',
                   tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl."))

tekst_zdm4<-tags$p('Użyte skróty: DDR - droga dla rowerów, CPR - ciąg pieszo-rowerowy, 
                   NSR - Nadwiślański Szlak Rowerowy, N - północ, S - południe, E - wschód, W - zachód.')


tekst1<-tags$p('Aplikacja', tags$b('Rowery'),' przedstawia dane z automatycznych liczników rowerów w Warszawie
                        od początku ich funkcjonowania. Źródłem danych jest: ',
               tags$a(href='https://zdm.waw.pl', "Zarząd Dróg Miejskich w Warszawie"),
               '.')

tekst2<-tags$p(
  'Dane o pogodzie w Warszawie (a dokładniej - na stacji meteorologicznej na Lotnisku Chopina) pochodzą z ',
  tags$a(href='https://dane.imgw.pl', 'Instytutu Meteorologii i Gospodarki Wodnej.')
)

tekst3<-tags$p(
  'Autorka aplikacji: Monika Pawłowska (kontakt:',
  tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl"),
  '), współpraca: Adam Kolipiński (mapa) oraz Rowerozofia. Kod i dane źródłowe dostępne są',
  tags$a(href='https://github.com/pawlowska/shiny-server/tree/master/rowery', 'tu.'))