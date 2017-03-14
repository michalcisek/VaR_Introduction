rm(list=ls())
library(rvest)
library(pbapply)
library(stringi)
library(sqldf)

#wygenerowanie sekwencji dat od pierwszego notowania
dni <- seq(as.Date("1991-04-16"),as.Date("2017-03-10"),by="days")
dni <- dni[-which(weekdays(dni) %in% c("sobota","niedziela"))]


pobierz_akcje<-function(date){
  
  web<-read_html(paste("https://www.gpw.pl/notowania_archiwalne_full?type=10&date=",date, sep=""))
  
  web %>%
    html_nodes("td , .left, th") %>% 
    html_text()  %>% 
    as.vector() %>%
    matrix(., nrow=11, ncol=length(.)/11) %>%
    t() %>%
    data.frame(.,stringsAsFactors=F) -> df
  
  colnames(df)<-df[1,]
  df<-df[-1,]
  return(df)
}


# akcje<-pblapply(dni,pobierz_akcje)
akcje<-vector("list",length(dni))
for(i in 1:length(dni)){
  tryCatch({
      akcje[[i]]<-pobierz_akcje(dni[i])
      print(i)
      flush.console()
    },
    error=function(err){
      cat("timeout: \n")
      print(err)
      akcje[[i]]<-pobierz_akcje(dni[i])
      flush.console()
    }
  )
}

#sprawdzenie brakow i proba dogrania na te dni
braki<-c()
for(i in 1:length(dni)){
  if(nrow(akcje[[i]])==0 | length(akcje[[i]])==0){
    braki[i] <- 1
  } else {
    braki[i] <- 0
  }
}
braki<-which(braki==1)

for(i in braki){
  print(i)
  akcje[[i]]<-pobierz_akcje(dni[i])
}

#zmiana listy na ramkę danych i dorzucenie daty (ZROBIC BENCHMARK ROZNYCH METOD)
l_akcji<-sapply(akcje,nrow)
names(l_akcji)<-dni
daty<-unlist(sapply(1:length(dni), function(x) rep(names(l_akcji)[x],l_akcji[x])))
notowania<-do.call(rbind,akcje)
notowania<-data.frame(daty,notowania)
colnames(notowania)<-c("data","nazwa","ISIN","waluta","otwarcie","maksimum","minimum","zamkniecie","zmiana_kursu","wolumen",
                       "transakcje","wartosc_obrotu")
notowania$data<-as.character(notowania$data)

# ZLY POMYSL !!!
# notowania<-data.frame("data"=character(0),"nazwa"=character(0),"ISIN"=character(0),"waluta"=character(0),"otwarcie"=character(0),"maksimum"=character(0),
#                       "minimum"=character(0),"zamkniecie"=character(0),"zmiana_kursu"=character(0),"wolumen"=character(0),
#                       "transakcje"=character(0),"wartosc_obrotu"=character(0))
# for(i in 1:length(dni)){
#   if(nrow(akcje[[i]])==0 | length(akcje[[i]])==0){
#     next
#   } else {
#     notowania <- rbind(notowania,data.frame(dni[i],akcje[[i]]))
#   }
# }
# colnames(notowania)<-c("data","nazwa","ISIN","waluta","otwarcie","maksimum","minimum","zamkniecie","zmiana_kursu","wolumen",
#                        "transakcje","wartosc_obrotu")
# notowania$data <- as.character(notowania$data)
# notowania$data <- as.Date(notowania$data)

#konwersja zmiennych znakowych do numerycznych
konwertuj_do_numerycznej<-function(kolumna){
  notowania[,kolumna] %>%
    gsub(",",".",., fixed=T) %>%
    stri_replace_all_charclass(., "\\p{WHITE_SPACE}","") %>%
    as.numeric() -> notowania[,kolumna]
}

notowania[,5:12]<-sapply(colnames(notowania)[5:12],konwertuj_do_numerycznej)

#1623715 rekordow

#zapis do bazy danych SQL
db <- dbConnect(SQLite(), dbname="notowania_gpw_2000.sqlite")
dbSendQuery(db, "create table notowania_2000
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db,"notowania_2000",notowania[notowania$data<="1999-12-31",],overwrite=T)
query<-dbGetQuery(db,"select * from notowania_2000 where nazwa='KROSNO'")
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname="notowania_gpw_2005.sqlite")
dbSendQuery(db, "create table notowania_2005
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db,"notowania_2005",notowania[notowania$data>"1999-12-31" & notowania$data<="2004-12-31",],overwrite=T)
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname="notowania_gpw_2010.sqlite")
dbSendQuery(db, "create table notowania_2010
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db,"notowania_2010",notowania[notowania$data>"2004-12-31" & notowania$data<="2009-12-31",],overwrite=T)
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname="notowania_gpw_2015.sqlite")
dbSendQuery(db, "create table notowania_2015
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db,"notowania_2015",notowania[notowania$data>"2009-12-31" & notowania$data<="2014-12-31",],overwrite=T)
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname="notowania_gpw_2020.sqlite")
dbSendQuery(db, "create table notowania_2020
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db,"notowania_2020",notowania[notowania$data>"2014-12-31" & notowania$data<="2019-12-31",],overwrite=T)
dbDisconnect(db)
