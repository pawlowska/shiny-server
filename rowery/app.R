library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')
source('wykresy_pogody.R', encoding = 'UTF-8')

dane_polaczone<-wczytaj_dane() #wczytuje wstepnie obrobione dane z csv

dane_polaczone<-suma_licznikow(dane_polaczone)

listy_stylow<-zrob_listy_stylow(dane_polaczone)
kolory<-listy_stylow[1,]
lista_linii<-listy_stylow[2,]
lista_fontow<-listy_stylow[3,]

nazwy<-names(dane_polaczone)[4:ncol(dane_polaczone)]

dane_polaczone<-dodaj_pogode(dane_polaczone)

dane_long<-wide_to_long(dane_polaczone)
dane_tyg<-podsumuj.tygodnie(dane_long)
dane_m<-podsumuj.miesiace(dane_long)

zakresOd=  '2014-08-01'
zakresOdPokaz='2017-01-01'
zakresDo = '2017-03-19'
zakresDoPogoda= '2017-03-31'


okresy = c('dobowo', 'tygodniowo', 'miesięcznie')

ile_licznikow<-19
ile_sum<-3
ile<-ile_licznikow-ile_sum

nazwy_licznikow<-nazwy[1:ile_licznikow]
nazwy_sum<-nazwy[(ile_licznikow+1):(ile_licznikow+ile_sum)]

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    h1 {
                      margin-top:0px;
                      margin-bottom: 5px;
                    }
                    ")),
    tags$style(HTML("
                    h5 {
                      padding-left: 16px;
                      margin-top: 3px;
                    };
                    "))),
  h5('Autorka: Monika Pawłowska', align = 'right'),
  headerPanel('Liczba rowerów'),
  h5('Dane z liczników rowerowych w Warszawie'),
  sidebarLayout(
    sidebarPanel(
      lapply(1:length(nazwy), function(x) {
        n <- length(nazwy)
        css_col <- paste0("#liczniki div.checkbox:nth-child(",x,
                          ") span{color: ", kolory[x],"; font-weight : ",lista_fontow[x],"}")
        tags$style(type="text/css", css_col)
      }),
      checkboxGroupInput('liczniki', 'Wybierz miejsca', nazwy, 
                         selected = nazwy[c(4,20)], inline = FALSE, width = NULL),
      style= "padding: 10px 0px 0px 20px;"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(
                   column(5, #daty
                          dateRangeInput('zakres', 'Wybierz zakres dat', 
                                         start=zakresOdPokaz, end=zakresDo, min=zakresOd, max=zakresDo,
                                         separator = 'do', weekstart = 0, language = "pl")
                   ),
                   column(7, #dobowo/tygodniowo/miesiecznie
                          radioButtons('okres', 'Podsumuj', okresy, selected = okresy[1], 
                                       inline = TRUE, width = NULL)        
                   )
                 ), style= "padding: 10px 0px 0px 20px;"), #end wellPanel
                 div(id = "plotDiv", #wykres
                     style = "position:relative",
                     alt = "Ile rowerów jeździ w Warszawie",
                   plotOutput('plot1', height=480, hover = hoverOpts(id = "plot_hover", delay = 100)),
                   uiOutput("bike_date_tooltip")
                 )
        ),
        tabPanel("Pogoda",
                 tags$p(), 
                 div(id = "weatherPlotDiv", 
                     style = "position:relative",
                     alt = "Ile rowerów w zależności od pogody",
                     plotOutput('plot2', height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
                     uiOutput("bike_weather_tooltip")
                 )
        ),
        tabPanel("O aplikacji",
                 tags$p(),
                 tags$p('Aplikacja', tags$b('Rowery'),' przedstawia dane z automatycznych liczników rowerów w Warszawie
                        od początku ich funkcjonowania, czyli od ', 
                        zakresOd, ', do ', zakresDo, 
                        '. Źródłem danych jest: ',
                   tags$a(href='https://zdm.waw.pl', "Zarząd Dróg Miejskich w Warszawie"),
                   '(otrzymane mailem).'),
                 tags$p(
                   'Średnią dobową temperaturę w Warszawie (a dokładniej - na stacji meteorologicznej na Lotnisku Chopina) wzięłam ze strony ',
                   tags$a(href='https://dane.imgw.pl', 'https://dane.imgw.pl.'),
                   'Źródłem pochodzenia danych jest Instytut Meteorologii i Gospodarki Wodnej – Państwowy Instytut Badawczy.'
                 ),
                 tags$p(
                   'Autorka aplikacji: Monika Pawłowska (kontakt:',
                   tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl"),
                   '). Kod i dane źródłowe dostępne są',
                   tags$a(href='https://github.com/pawlowska/shiny-server/tree/master/rowery', 'tu.'))
        ) #end of "O..."
        
      ) #end tabsetPanel
    ) #end mainPanel
  )
) #end ui

server <- function(input, output) {
  indeksy<-reactive({ #ktore kolory beda uzyte
    match(input$liczniki, nazwy)
    })
  
  data <- reactive({
    zakres_dat=interval(input$zakres[1], input$zakres[2])
    if (input$okres==okresy[2]) {
      dane_tyg[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
    else if (input$okres==okresy[3]) {
      dane_m[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
    else {
      dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
  })
  
  data_with_weather <- reactive({
    zakres_dat=interval(zakresOd, zakresDoPogoda)
    dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
  })
  
  uzyte_kolory<-reactive({
    kolory[indeksy()]
  })
  
  output$plot1 <- renderPlot({
    uzyte_linie <-lista_linii[indeksy()]
    wykres_kilka(data(), 
                 start=input$zakres[1], stop=input$zakres[2], paleta=uzyte_kolory(), linie = uzyte_linie)
  })
  
  output$plot2 <- renderPlot({
    #print(tail(data_with_weather()))
    pogoda_basic(data_with_weather(),
                 paleta=uzyte_kolory())
  })
  
  output$bike_date_tooltip <- renderUI({
    #based on: http://stackoverflow.com/questions/38992270/r-shiny-tooltip-in-ggplot
    
    hover <- input$plot_hover
    #is mouse close to a point?
    point <- nearPoints(data(), hover, threshold = 8, maxpoints = 1)[ ,c("Data", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu

    #else add to UI
    wellPanel(
      style = tooltip_html(tooltip_position(hover)),
      p(HTML(paste0( point$Data,": ", point$Liczba_rowerow)))
    )
  })
  
  output$bike_weather_tooltip <- renderUI({
    
    hover <- input$plot_hover
    #is mouse close to a point?
    point <- nearPoints(data_with_weather(), hover, threshold = 8, maxpoints = 1)[ ,c("Data","temp_avg", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu
    
    #else add to UI
    wellPanel(
      style = tooltip_html(tooltip_position(hover, w=180)),
      p(HTML(paste0( point$Data,": ",point$temp_avg, '&degC, ', point$Liczba_rowerow)))
    )
  })
}

tooltip_position<-function(hover, w=120) {   #calculate the position of the tooltip
  #in relative units
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #in pixel units
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  if(hover$range$right-left_px<w) left_px<-left_px-(w+5) #tooltips too far right were hidden
  
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  c(left_px, top_px)
}

tooltip_html<-function(pos_px) {
  left_px<-pos_px[1]
  top_px<-pos_px[2]
  paste0("position:absolute; z-index:100; padding: 0 6px 0 6px; height: 22px; overflow: hidden;
                    background-color: rgba(245, 245, 245, 0.85); ",
  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
}

shinyApp(ui = ui, server = server)