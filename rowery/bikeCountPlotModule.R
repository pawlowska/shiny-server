library(ggplot2)

source('wykresy.R', encoding = 'UTF-8')
source('tooltip.R', encoding = 'UTF-8')


# Module UI function
bikeCountPlotOutput <- function(id, label = "wykresLiczby") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    div(id = "plotDiv", #wykres
        style = "position:relative",
        alt = "Ile rowerów jeździ w Warszawie",
        plotOutput(ns('plotLiczba'), height=500, hover = hoverOpts(id = ns("plot_hover"), delay = 100)),
        uiOutput(ns("bike_date_tooltip"))
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
  
  output$bike_date_tooltip <- renderUI({
    #based on: http://stackoverflow.com/questions/38992270/r-shiny-tooltip-in-ggplot
    #is mouse close to a point?
    point <- nearPoints(data(), input$plot_hover, threshold = 8, maxpoints = 1)[ ,c("Data", "Liczba_rowerow")]

    if (nrow(point) == 0) return(NULL)    #if no close points return null
    else { #else add wellpanel to UI
      tooltipWellPanel(input$plot_hover, paste0(point$Data,": ", round(point$Liczba_rowerow, digits=1)))
    }
  })
  
}
