library(ggplot2)

source('wykresy.R', encoding = 'UTF-8')


# Module UI function
bikeCountPlotOutput <- function(id, label = "wykresLiczby") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    div(id = "plotDiv", #wykres
        style = "position:relative",
        alt = "Ile rowerów jeździ w Warszawie",
        plotOutput(ns('plotLiczba'), height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
        uiOutput("bike_date_tooltip")
    )
  )
}

# Module server function
bikeCountPlot <- function(input, output, session, zakres, zakresOd, zakresDo, liczniki, style, data, krok, wartosc) {
  output$plotLiczba <- renderPlot({
    shiny::validate(
      # need((input$zakres[1]>=zakresOd)&(input$zakres[2]>=zakresOd)), 
      #      paste("Data spoza zakresu - dostępne dane od", zakresOd)),
      # need((zakres[1]<=zakresDo)&(zakres[2]<=zakresDo), 
      #      paste("Data spoza zakresu - dostępne dane do", zakresDo)),
      need(zakres()[1]<zakres()[2], "Błędny zakres dat"), 
      need(liczniki(), 'Wybierz przynajmniej jedno miejsce!')
    )
    wykres_kilka(data(), 
                 start=zakres()[1], stop=zakres()[2], 
                 paleta=style$kolory, linie = style$linie, alfy=style$alfy,
                 krok=krok(), wartosc = wartosc())
  })
  
}
