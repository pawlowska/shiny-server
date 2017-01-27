library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')

nazwa_pliku = "Zapytanie2016utf8.csv"
dane<-zaladuj_dane(nazwa_pliku)
nazwy<-names(dane)[2:20]
dane_long<-raw_to_long(dane)

zakresOd=  '2016-01-01'
zakresDo = '2016-12-19'

ui <- fluidPage(
  headerPanel('Liczba rowerów'),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('zakres', 'Wybierz zakres dat', 
                     start=zakresOd, end=zakresDo, min=zakresOd, max=zakresDo,
                     separator = 'do', weekstart = 1),
      checkboxGroupInput('liczniki', 'Wybierz miejsca', nazwy, 
                         selected = nazwy[sample(1:length(nazwy),5)], inline = FALSE, width = NULL)
    ),
    mainPanel(
      tags$p(paste('Dane z automatycznych liczników rowerów ZDM z okresu od ', zakresOd, ' do ', zakresDo)),
      tags$p(""),
      plotOutput('plot1', height=500),
      hr(),
      tags$p(
        'Dane: ',
        tags$a(href='https://zdm.waw.pl', "Zarząd Dróg Miejskich w Warszawie"),
        '(otrzymane 20.12.2016). Aplikacja: Monika Pawłowska',
        tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl")
        )
    ) #end mainPanel
  )
) #end ui

server <- function(input, output) {
  indeksy<-reactive({ #ktore kolory beda uzyte
    match(input$liczniki, nazwy)
    })
  output$plot1 <- renderPlot({
    uzyte_kolory<-kolory[indeksy()]
    wykres_kilka(dane_long, input$liczniki, 
                 start=input$zakres[1], stop=input$zakres[2], paleta=uzyte_kolory)
  })
}

shinyApp(ui = ui, server = server)