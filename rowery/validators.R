validateLiczniki<-function(liczniki) {
  shiny::validate(
    need(liczniki, 'Wybierz przynajmniej jedno miejsce!')
  )
}

validateZakres<-function(zakres, zakresOd, zakresDo) {
  shiny::validate(
    need((zakres[1]>=zakresOd)&(zakres[2]>=zakresOd), 
         paste("Data spoza zakresu - dostępne dane od", zakresOd)),
    need((zakres[1]<=zakresDo)&(zakres[2]<=zakresDo), 
         paste("Data spoza zakresu - dostępne dane do", zakresDo)),
    need(zakres[1]<zakres[2], "Błędny zakres dat")
  )
  
}