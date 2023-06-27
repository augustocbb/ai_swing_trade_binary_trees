
##################################################################
#   Vovinha Project (Fibonacci retracement and Financial Multiples for Binary Trees)
#   File 2: fibonacci_retracements.r
#   Objective: Build Stock prices fibonacci retracements
#   Code Description:




# Libraries

library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(readxl)
library(Rcpp)
library(DBI)
library(RMySQL)
library(RMariaDB)
library(readxl)
library(BatchGetSymbols)
library(sqldf)

#### Define Access Constrains

PASSWORD <- 
DBUSER <- 
DBNAME <-


wd <- getwd()

setwd(wd&
  "/ProgramFiles"
)

# Connect do MYSQL database

localuserpassword <- PASSWORD
storiesDb <- dbConnect(
  RMariaDB::MariaDB(),
  user = DBUSER,
  password = localuserpassword,
  dbname = DBNAME,
  host = 'localhost'
)

dbDisconnect(storiesDb)
rm(storiesDb)
dbListTables(storiesDb) #REARRANGE IN CASE OF DIFFERENT TABLE NAMES



dbGetQuery("Select * from temp_tcotacao1 union ")


#######
calcula_fibo2 <- function(maximo, minimo, lfinal, periodo)
{
  inicial = lfinal - periodo
  
  if (inicial < 1) {
    inicial = 1
  }
  
  intervalo <- (maximo - minimo)
  fibo <- data.frame(nrow = lfinal)
  fibo$periodo <- periodo
  fibo$maximo <- maximo
  fibo$minimo <- minimo
  fibo$FR79 <- maximo - intervalo * 0.786
  fibo$FR62 <- maximo - intervalo * 0.618
  fibo$FR50 <- maximo - intervalo * 0.500
  fibo$FR38 <- maximo - intervalo * 0.382
  fibo$FR24 <- maximo - intervalo * 0.236
  
  return(fibo)
  
}


a <- cotacao



a <- split(a, a$ticker)


b <- lapply(a,function(x) count(x))

a[i][11]
x
b <- split(a, as.integer(gl(length(a), 3, length(a))))
i = 1
j = 150
x <- a[[i]]
aux <- x[j,]
aux <- cbind(aux, calcula_fibo2(maximo, minimo, j, nrow(x)), 
             calcula_fibo2(maximo90, minimo90,j,90),
             calcula_fibo2(maximo30, minimo30,j,30))
aux$cont <- j
base <- aux
base<-rbind(base,aux)

base 

b <- list()
for (i in 1:3) {
  
  x <- a[[i]]
  
  maximo <- x$price.close[1]
  minimo <- maximo
  
  maximo90 <- x$price.close[1]
  minimo <- maximo90
  
  maximo90 <- x$price.close[1]
  minimo <- maximo90
  
  for (j in 1:nrow(x)) {
    
    
    
    aux <- x[j,]
    
    if(aux$price.close > maximo){
      maximo <- aux$price.close
    }
    
    if(aux$price.close < minimo){
      minimo <- aux$price.close
    }
    
    if(j > 30){
      maximo30 <- max(x[(j-30):j,4])
      minimo30 <- min(x[(j-30):j,4])
    }
    
    if(j > 90){
      maximo90 <- max(x[(j-90):j,4])
      minimo90 <- min(x[(j-90):j,4])
    }
    
    aux <- cbind(aux, calcula_fibo2(maximo, minimo, j, nrow(x)), 
                 calcula_fibo2(maximo90, minimo90,j,90),
                 calcula_fibo2(maximo30, minimo30,j,30))
    
    aux$cont <- j
    
    base<- rbind(base, aux)
    
  }
  b[[i]] <- base
  rm(base)
  
}


base_fibo <- b

base_fibo <- unlist(b)


x <- base_fibo

x <- as.data.frame(x)
nomes <- c("price.open",
                      "price.high",
                      "price.low",
                      "price.close",
                      "volume",
                      "price.adjusted",
                      "ref.date",
                      "ticker",
                      "ret.adjusted.prices",
                      "ret.closing.prices",
                      "nrow",
                      "periodo",
                      "maximo",
                      "minimo",
                      "fibo79",
                      "fibo62",
                      "fibo50",
                      "fibo38",
                      "fibo24",
                      "nrow_90",
                      "periodo_90",
                      "maximo_90",
                      "minimo_90",
                      "fibo79_90",
                      "fibo62_90",
                      "fibo50_90",
                      "fibo38_90",
                      "fibo24_90",
                      "nrow_30",
                      "periodo_30",
                      "maximo_30",
                      "minimo_30",
                      "fibo79_30",
                      "fibo62_30",
                      "fibo50_30",
                      "fibo38_30",
                      "fibo24_30"
)


for (i in 1:330) {
  names(base_fibo[[i]]) <- nomes
}

base_fibo <-Map(setNames, base_fibo , nomes)


temporario <- base_fibo
temporario <- list2env(temporario  )

x <- as.data.frame(temporario)

names(temporario[[330]])

unlist()