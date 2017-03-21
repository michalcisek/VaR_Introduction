# argumenty: akcje, data poczatkowa, data koncowa
library(tidyr)
library(magrittr)
library(sqldf)

# spolki <- c("AGORA","KGHM","JSW")
# start<-'2015-02-01'
# stop<-'2015-02-06'


funkcja <- function(nazwy_akcji, data_poczatkowa, data_koncowa){
  
  db <- dbConnect(SQLite(), dbname="notowania_gpw_2020.sqlite")
  
  query<- paste("select data, nazwa, zamkniecie from notowania_2020
              where nazwa in ('", paste(nazwy_akcji,collapse="' , '"),"') and
              data between '", data_poczatkowa, "' and '", data_koncowa, "'", sep="")
  
  dane<-dbGetQuery(db, query,row.names=F)
  dane %>%
    spread(., data, zamkniecie) %>%
    t() -> dane
  
  colnames(dane) <- dane[1, ]
  dane <- dane[-1,]
  dane <- data.frame(data = row.names(dane), dane)
  
  dbDisconnect(db)
  
  return(dane)
}

funkcja(c("AGORA", "KGHM", "CIGAMES","YOLO"),'2015-06-01','2017-03-20') -> fr


db <- dbConnect(SQLite(), dbname="notowania_gpw_2020.sqlite")
spolki <- dbGetQuery(db, "select distinct nazwa from notowania_2020")
dbDisconnect(db)
# View(spolki)



# podsumowanie dotyczace spolki -------------------------------------------
podsumowanie <- function(nazwy_akcji){
  query <- paste("select nazwa, count(*) l_dni, min(data) min, max(data) max from notowania_2020
                 where nazwa in('",paste(nazwy_akcji,collapse="' , '"),"')
                 group by nazwa",sep="")
  pods <- dbGetQuery(db, query)
  return(pods)
}

podsumowanie("YOLO")
podsumowanie(c("AGORA", "KGHM", "CIGAMES","YOLO"))


# POROWNANIE CZASOW WYKONANIA FUNKCJI DLA ROZNYCH POMYSLOW !!!!
# db <- dbConnect(SQLite(), dbname = "notowania_gpw_2020.sqlite")
# 
# podsumowanie <- function(nazwa_akcji){
#   query <- paste("select count(*) l_dni, min(data) min, max(data) max from notowania_2020 where nazwa ='",nazwa_akcji,"'",sep="")
#   pods <- dbGetQuery(db, query)
#   pods<- data.frame(nazwa = nazwa_akcji, pods)
#   return(pods)
# }
# 
# do.call(rbind, lapply(unlist(spolki)[sample(552,10)], function(x) podsumowanie(x)))
# 
# 
# podsumowanie1 <- function(nazwy_akcji){
#   query <- paste("select nazwa, count(*) l_dni, min(data) min, max(data) max from notowania_2020 
#                  where nazwa in('",paste(nazwy_akcji,collapse="' , '"),"')
#                  group by nazwa",sep="")
#   pods <- dbGetQuery(db, query)
#   # pods<- data.frame(nazwa = nazwy_akcji, pods)
#   return(pods)
# }
# 
# 
# microbenchmark::microbenchmark(
#   do.call(rbind, lapply(unlist(spolki)[sample(552,10)], function(x) podsumowanie(x))),
#   podsumowanie1(unlist(spolki)[sample(552,10)]),
#   times = 20
# )
# 
# dbDisconnect(db)



# 1. zastanowic sie splitami
# 2. zastanowic sie nad brakami danych




