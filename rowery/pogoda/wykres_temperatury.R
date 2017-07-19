library(ggplot2)
library(data.table)

t_long<-melt(temperatura, id.vars="Data", value.name = "Temperatura")
t_long[,Data:=as.Date(Data)]

g<-ggplot()+geom_line(data=t_long[Data>=as.Date("2015-05-01") & Data<as.Date("2015-06-01")], aes(Data, Temperatura, colour=variable))

print(g)