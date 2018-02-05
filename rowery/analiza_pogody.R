library(data.table)

temp<-dane_polaczone_kopia[,c(1:3,7,28:33)] #7=Banacha
dane_a<-temp[complete.cases(temp),]
dane_a[,Jaki_dzien:=as.numeric(as.factor(Jaki_dzien))-1]
dane_a[,czy_opady:=as.numeric((dane_a[,deszcz]+dane_a[,snieg])>0)]

library(corrgram)
corrgram(dane_a[,c(1,4,6,10:11)],
         lower.panel = panel.pts,
         upper.panel = panel.cor)

x_data<-dane_a[,temp_avg]
x<-x_data
y_data<-dane_a[,Banacha]
y<-y_data

x2<-dane_a[,czy_opady]
x3<-dane_a[,Jaki_dzien]

lin_1param<-lm(Banacha~temp_avg, data=dane_a)
print(summary(lin_1param))
# plot(y~x2)
# abline(lin, col="red", lw=2)


lin2<-lm(Banacha~temp_avg+czy_opady+Jaki_dzien+temp_avg*Jaki_dzien, data=dane_a)
print(summary(lin2))


weekend=dane_a[Jaki_dzien==1]
robocze=dane_a[Jaki_dzien==0]
tab=robocze

gauss = nls( Banacha ~ Rmax*exp(-1/2*(temp_avg-temp_opt)^2/sigma^2), 
            start=c(temp_opt=25,sigma=10,Rmax=5000), 
            data = tab)
summary(gauss)
v <- summary(gauss)$parameters[,"Estimate"]
plot(Banacha~temp_avg, data=tab)
plot(function(temp_avg) v[3]*exp(-1/2*(temp_avg-v[1])^2/v[2]^2),col=2,add=T,xlim=range(dane_a$temp_avg) )


lin_1param<-lm(Banacha~temp_avg, data=tab)
print(summary(lin_1param))

gauss2 = nls( Banacha ~ Rmax*(1+b*czy_opady)*exp(-1/2*(temp_avg-temp_opt)^2/sigma^2), 
             start=c(temp_opt=25,sigma=10,Rmax=5000, b=-1), 
             data = tab)
summary(gauss2)
v <- summary(gauss2)$parameters[,"Estimate"]
