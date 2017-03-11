library(quantmod)
library(ggplot2)

#sprawdzenie dostepnych akcji (stockSymbols)
akcje<-stockSymbols()

#pobranie danych (getSymbols)
apple<-getSymbols("AAPL",src = "yahoo",env = NULL)

#przykladowa wizualizacja - xts
plot(apple)

#przykladowa wizualizacja - quantmod (barChart, candleChart)
barChart(apple)
candleChart(apple[1:100,])

#obrobka danych do potrzebnego formatu, sprawdzenie braków
apple_df<-data.frame(apple)
apple_df<-data.frame(daty=row.names(apple_df),apple_df)
row.names(apple_df)<-1:nrow(apple_df)

apple_df<-apple_df[,c(1,7)]
apple_df$daty<-as.Date(apple_df$daty)
colnames(apple_df)<-c("date","close")

#wizualizacja ggplot
ggplot(apple_df,aes(x=date,y=close))+
  geom_line()+
  theme_bw()

#pobranie przykladowego portfela zlozonego z kilku akcji
library(sqldf)


pobierz_portfel<-function(n){
  akcje1<-sample(akcje$Symbol,n)
  for(i in 1:n){
    if(i==1){

      ceny1<-getSymbols(akcje1[i],src = "yahoo",env = NULL,from="2015-01-01")
      ceny1<-data.frame(ceny1)
      ceny1<-data.frame(daty=row.names(ceny1),ceny1)
      row.names(ceny1)<-1:nrow(ceny1)
      
      ceny1<-ceny1[,c(1,7)]
      ceny1$daty<-as.Date(ceny1$daty)
      colnames(ceny1)<-c("date","close")
      
      ceny<-ceny1
      
    } else{
      
      ceny1<-getSymbols(akcje1[i],src = "yahoo",env = NULL,from="2015-01-01")
      ceny1<-data.frame(ceny1)
      ceny1<-data.frame(daty=row.names(ceny1),ceny1)
      row.names(ceny1)<-1:nrow(ceny1)
      
      ceny1<-ceny1[,c(1,7)]
      ceny1$daty<-as.Date(ceny1$daty)
      colnames(ceny1)<-c("date","close")
      
      ceny<-sqldf("select a.*, b.close from ceny a inner join ceny1 b on a.date=b.date")
          
    }
  }
  return(ceny)
}

portfel<-pobierz_portfel(2)

#policzenie zwrotów cen (Delt), średniego zwrotu dla instrumentu i odchylenia standardowego
zwroty<-apply(portfel[,c(2,3)],2,Delt)
zwroty<-zwroty[-1,]

portfel_zwroty<-data.frame(daty=portfel[-1,1],zwroty)

srednia<-apply(zwroty,2,mean)
odchylenie<-apply(zwroty,2,sd)

#przeliczenie macierzy korelacji (cor, pearson)
korelacja<-cor(zwroty,method = "pearson")



#praca domowa
#1. instalcja git-a, zalozenie konta na githubie
#2. zastanowic sie nad mechanizmem pobieranie cen




