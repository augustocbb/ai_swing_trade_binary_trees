###################################################################
#   Vovinha Project (Fibonacci retracement and Financial Multiples for Binary Trees)
#   File 1: data_ingest.r
#   Objective: Get Finnancial data of B3 API with finnancial statment of listed companies
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

##### GET DFP DATA AND SAVE AT   #####


 # install pkg if not found
 if (!require(devtools)) install.packages("devtools")
 if (!require(GetDFPData2)) devtools::install_github("msperlin/GetDFPData2")

 # load pkg
 library(GetDFPData2)

 # set input options
 my_ids <- c(NULL)
 first_year  <- lubridate::year(as.Date("2010-01-01"))
 last_year   <- lubridate::year(as.Date("2022-01-03"))
#
 # get data using get_dfp_data
 # This can take a while since the local data is not yet cached..
 l_dfp <- get_dfp_data(companies_cvm_codes = my_ids,
                       first_year = first_year,
                       last_year  = last_year,
                       type_docs = c("DRE"),
                       type_format = c("ind"))
#### https://www.msperlin.com/shiny/GetDFPData/
    

## Constru��o de base de tickers e CNPJ ####



write.xlsx(my_tickers, "my_tickers.xlsx")





### Abrir arquivo com todas as DRE's

filename <- file.choose()
dfs <- readRDS(filename)
temp_dfs <- dfs$`DF Individual - Demonstra��o do Resultado`

filename <- file.choose()
bp <- readRDS(filename)

filename <- file.choose()
dfc <- readRDS(filename)
view(bp)



base_dfs <-
  rbind(
    bp$`DF Individual - Balan�o Patrimonial Ativo`,
    bp$`DF Individual - Balan�o Patrimonial Passivo`,
    dfs$`DF Individual - Demonstra��o do Resultado`,
    dfc$`DF Individual - Demonstra��o do Fluxo de Caixa (M�todo Indireto)`
  )

names(base_dfs)

dbWriteTable(storiesDb, "base_dre", base_dfs, overwrite = TRUE)


a <- dbGetQuery(storiesDb, "select distinct ticker from bcotacao")


##### Obten��o de Cota��es  #####

library(BatchGetSymbols)

library(readxl)
# base_ticker <- read_excel("base_ticker.xlsx")
#
# dbWriteTable(storiesDb, "base_ticker",base_ticker, overwrite = TRUE)

## Constru��o da base de tickers ##

base_ticker <- dbGetQuery(storiesDb, "select * from base_ticker")
# set tickers
titulo <- names(base_ticker)
my_tickers <-
  cbind(
    base_ticker,
    paste(base_ticker$Ticker, '3.SA', sep = ""),
    paste(base_ticker$Ticker, '4.SA', sep = ""),
    paste(base_ticker$Ticker, '11.SA', sep = "")
  )
names(my_tickers) <- c(titulo, "ticker3", "ticker4", "ticker11")
view(my_tickers)

#### Input de Ticker na base DFS (MySql) ####

# create table tbase_dfs
# select b.ticker3 as ticker, b.`Ticker Aux` as cnpj_aux, a.*  from my_tickers as b
# left join (select * from base_dre) as a
# on b.CNPJ = a.CNPJ_CIA
#
# create table base_dfs
# select b.SETOR_ATIV as setor, a.*  from tbase_dfs as a
# left join (select * from base_cvm group by CNPJ) as b
# on b.CNPJ = a.cnpj_aux



### Base de Setores ###
base_cvm <- read_excel("base_cvm.xlsx")


dbWriteTable(storiesDb, "base_cvm", base_cvm)

base_cvm <- dbGetQuery(storiesDb, "select * from base_cvm")



# set dates and other inputs
first_date <- "2010-01-01"
last_date <- Sys.Date()
thresh_bad_data <- 0.95   # sets percent threshold for bad data
bench_ticker <- '^BVSP'   # set benchmark as ibovespa
cache_folder <- 'data/BGS_Cache' # set folder for cache



tcotacao <- BatchGetSymbols(
  tickers = my_tickers$ticker3,
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker,
  do.parallel = TRUE
)
dbWriteTable(storiesDb, "temp_tcotacao1", tcotacao$df.tickers, overwrite = TRUE)

rm(tcotacao)


tcotacao2 <- BatchGetSymbols(
  tickers = my_tickers$ticker3[201:463],
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker
)

dbWriteTable(storiesDb, "temp_tcotacao2", tcotacao2$df.tickers, overwrite = TRUE)

rm(tcotacao2)

tcotacao3 <- BatchGetSymbols(
  tickers = my_tickers$ticker4[1:200],
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker
)


dbWriteTable(storiesDb, "temp_tcotacao3", tcotacao3$df.tickers, overwrite = TRUE)

rm(tcotacao3)

tcotacao4 <- BatchGetSymbols(
  tickers = my_tickers$ticker4[201:463],
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker
)


dbWriteTable(storiesDb, "temp_tcotacao4", tcotacao4$df.tickers, overwrite = TRUE)


rm(tcotacao4)


tcotacao5 <- BatchGetSymbols(
  tickers = my_tickers$ticker11[1:200],
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker
)


dbWriteTable(storiesDb, "temp_tcotacao5", tcotacao5$df.tickers, overwrite = TRUE)



rm(tcotacao5)

tcotacao6 <- BatchGetSymbols(
  tickers = my_tickers$ticker11[201:463],
  first.date = first_date,
  last.date = last_date,
  bench.ticker = bench_ticker
)


dbWriteTable(storiesDb, "temp_tcotacao6", tcotacao6$df.tickers, overwrite = TRUE)

rm(tcotacao6)

