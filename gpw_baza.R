library(sqldf)
library(RSQLite)

db <- dbConnect(SQLite(), dbname="notowania_gpw_2020.sqlite")
dbListFields(db,"notowania_2020")

akcje<-dbGetQuery(db,"select distinct nazwa from notowania_2020")

yolo <- dbGetQuery(db,"select * from notowania_2020 where nazwa in ('YOLO','TXM')")

dbDisconnect(db)

View(yolo)
