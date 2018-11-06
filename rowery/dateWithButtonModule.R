dateWithButtonInput<-function(id, label="zakres dat") {
  ns <- NS(id)
  tagList(
    splitLayout(cellWidths =c("85%","15%"),
                cellArgs = list(style = " display: inline-block; vertical-align: bottom;"),
    #fluidRow(
                uiOutput(ns('zakres')),
                actionButton(inputId = ns("caly"), label="\U221E", style = "margin-bottom: 15px;"),
                bsTooltip(id = ns("caly"), title = "Pokaż cały zakres dat", 
                          placement = "left", trigger = "hover")
    )
  )
  
}

dateWithButton <- function(input, output, session, dane, liczniki, zakresMax) {
  output$zakres <- renderUI({
    ns <- session$ns
    dateRangeInput(ns('zakres'), 'Wybierz zakres dat',
                   start=as.character(zakresMax[['do']]-90), end=zakresMax[['do']],
                   min=zakresMax[['od']], max=zakresMax[['do']],
                   separator = 'do', weekstart = 0, language = "pl")
  })
  
  observeEvent(input$caly, { #caly zakres dat
    if (!is.null(liczniki())) {
      daty<-dane[Miejsce %in% liczniki()]$Data
      updateDateRangeInput(session, inputId='zakres', 
                           start=min(daty), 
                           end=min(zakresMax[['do']], max(daty)))
    }
  })
  
  z<-reactive({input$zakres})
  
  return(z)
}