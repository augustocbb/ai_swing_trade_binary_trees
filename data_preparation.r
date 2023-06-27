
##################################################################
#   Vovinha Project (Fibonacci retracement and Financial Multiples for Binary Trees)
#   File 3: data_preparation.r
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

#### Build Target Prices based on multiples extracted from Damodaran


y <- dbGetQuery(storiesDb, "select * from base_arvore3")


a <- base_arvore

base_fund <- read_excel("base_multiplos.xlsx")


dbWriteTable(storiesDb, name = "base_multiplos", overwrite = TRUE, base_fund)



a <- sqldf( "select a.*, b.Pa_damodaran_ebitda, b.pa_EBITDA, b.pa_pl  from base_arvore as a 
           left join base_multiplos as b 
           on a.ticker = b.ticker and a.ano =  b.ano
           ")


names(base_arvore) <- c("price.open",
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


a <- y
head(a)


base_arvore$ano <- year(base_arvore$ref.date)


a <- left_join(base_arvore,base_fund, by = c("ano"= "ano", "ticker" = "ticker"), keep = FALSE)

x <- y

x <- y
x$fibo

x <- dbGetQuery(storiesDb, "select * from base_arvore_bkp")



x$fibo179 <- x$minimo + (x$maximo-x$minimo)*1.786
x$fibo162 <- x$minimo + (x$maximo-x$minimo)*1.618
x$fibo150 <- x$minimo + (x$maximo-x$minimo)*1.500
x$fibo138 <- x$minimo + (x$maximo-x$minimo)*1.382
x$fibo124 <- x$minimo + (x$maximo-x$minimo)*1.236

x$fibo179_90 <- x$minimo_90 + (x$maximo_90-x$minimo_90)*1.786
x$fibo162_90 <- x$minimo_90 + (x$maximo_90-x$minimo_90)*1.618
x$fibo150_90 <- x$minimo_90 + (x$maximo_90-x$minimo_90)*1.500
x$fibo138_90 <- x$minimo_90 + (x$maximo_90-x$minimo_90)*1.382
x$fibo124_90 <- x$minimo_90 + (x$maximo_90-x$minimo_90)*1.236

x$fibo179_30 <- x$minimo_30 + (x$maximo_30-x$minimo_30)*1.786
x$fibo162_30 <- x$minimo_30 + (x$maximo_30-x$minimo_30)*1.618
x$fibo150_30 <- x$minimo_30 + (x$maximo_30-x$minimo_30)*1.500
x$fibo138_30 <- x$minimo_30 + (x$maximo_30-x$minimo_30)*1.382
x$fibo124_30 <- x$minimo_30 + (x$maximo_30-x$minimo_30)*1.236

 a <- x

(names(x))



a <- data.frame(x$'fibo124',
                x$'fibo138',
                x$'fibo150',
                x$'fibo162',
                x$'fibo179',
                x$'fibo24',
                x$'fibo38',
                x$'fibo50',
                x$'fibo62',
                x$'fibo79',
                x$'maximo',
                x$'minimo',
                x$'fibo124_30',
                x$'fibo138_30',
                x$'fibo150_30',
                x$'fibo162_30',
                x$'fibo179_30',
                x$'fibo24_30',
                x$'fibo38_30',
                x$'fibo50_30',
                x$'fibo62_30',
                x$'fibo79_30',
                x$'maximo_30',
                x$'minimo_30',
                x$'fibo124_90',
                x$'fibo138_90',
                x$'fibo150_90',
                x$'fibo162_90',
                x$'fibo179_90',
                x$'fibo24_90',
                x$'fibo38_90',
                x$'fibo50_90',
                x$'fibo62_90',
                x$'fibo79_90',
                x$'maximo_90',
                x$'minimo_90',
                x$'...1',
                x$'ano',
                x$'CD_CVM',
                x$'cnpj_aux',
                x$'CNPJ_CIA',
                x$'COLUNA_DF',
                x$'DENOM_CIA',
                x$'DT_REFER',
                x$'EBITDA',
                x$'EBITDa Ajust',
                x$'fibo_Pa_damodaran_ebitda',
                x$'fibo_Pa_damodaran_ebitda_30',
                x$'fibo_Pa_damodaran_ebitda_90',
                x$'fibo_pa_ebitda',
                x$'fibo_pa_ebitda_30',
                x$'fibo_pa_ebitda_90',
                x$'fibo_pa_pl',
                x$'fibo_pa_pl_30',
                x$'fibo_pa_pl_90',
                x$'fibo_preco',
                x$'fibo_preco_30',
                x$'fibo_preco_90',
                x$'fibo_preco_900',
                x$'id',
                x$'id_aux10',
                x$'id_aux15',
                x$'id_aux20',
                x$'id_aux25',
                x$'id_aux30',
                x$'id_aux35',
                x$'id_aux40',
                x$'id_aux45',
                x$'id_aux5',
                x$'id_aux50',
                x$'id_aux55',
                x$'id_aux60',
                x$'LL',
                x$'mkt_cap',
                x$'Multiplo',
                x$'Multiplo EBITDA de Mercado',
                x$'Multiplo EV_EBITDA Damodaran',
                x$'Multiplo P/L de Mercado',
                x$'nrow',
                x$'nrow_30',
                x$'nrow_90',
                x$'P/L',
                x$'Pa_damodaran_ebitda',
                x$'pa_EBITDA',
                x$'pa_pl',
                x$'periodo',
                x$'periodo_30',
                x$'periodo_90',
                x$'preco',
                x$'Pre?o Fechamento',
                x$'price.adjusted',
                x$'price.close',
                x$'price.high',
                x$'price.low',
                x$'price.open',
                x$'ref.date',
                x$'ret.adjusted.prices',
                x$'ret.closing.prices',
                x$'setor',
                x$'ticker',
                x$'Valuation Multiplo EBITDA',
                x$'Valuation P/L',
                x$'Varia??o Pre?o Alvo EBITDA',
                x$'Varia??o Pre?o Alvo P/L',
                x$'VL_CONTA',
                x$'volume'
)




names(a) <- c("fibo124",
              "fibo138",
              "fibo150",
              "fibo162",
              "fibo179",
              "fibo24",
              "fibo38",
              "fibo50",
              "fibo62",
              "fibo79",
              "maximo",
              "minimo",
              "fibo124_30",
              "fibo138_30",
              "fibo150_30",
              "fibo162_30",
              "fibo179_30",
              "fibo24_30",
              "fibo38_30",
              "fibo50_30",
              "fibo62_30",
              "fibo79_30",
              "maximo_30",
              "minimo_30",
              "fibo124_90",
              "fibo138_90",
              "fibo150_90",
              "fibo162_90",
              "fibo179_90",
              "fibo24_90",
              "fibo38_90",
              "fibo50_90",
              "fibo62_90",
              "fibo79_90",
              "maximo_90",
              "minimo_90",
              "...1",
              "ano",
              "CD_CVM",
              "cnpj_aux",
              "CNPJ_CIA",
              "COLUNA_DF",
              "DENOM_CIA",
              "DT_REFER",
              "EBITDA",
              "EBITDa Ajust",
              "fibo_Pa_damodaran_ebitda",
              "fibo_Pa_damodaran_ebitda_30",
              "fibo_Pa_damodaran_ebitda_90",
              "fibo_pa_ebitda",
              "fibo_pa_ebitda_30",
              "fibo_pa_ebitda_90",
              "fibo_pa_pl",
              "fibo_pa_pl_30",
              "fibo_pa_pl_90",
              "fibo_preco",
              "fibo_preco_30",
              "fibo_preco_90",
              "fibo_preco_900",
              "id",
              "id_aux10",
              "id_aux15",
              "id_aux20",
              "id_aux25",
              "id_aux30",
              "id_aux35",
              "id_aux40",
              "id_aux45",
              "id_aux5",
              "id_aux50",
              "id_aux55",
              "id_aux60",
              "LL",
              "mkt_cap",
              "Multiplo",
              "Multiplo EBITDA de Mercado",
              "Multiplo EV_EBITDA Damodaran",
              "Multiplo P/L de Mercado",
              "nrow",
              "nrow_30",
              "nrow_90",
              "P/L",
              "Pa_damodaran_ebitda",
              "pa_EBITDA",
              "pa_pl",
              "periodo",
              "periodo_30",
              "periodo_90",
              "preco",
              "Pre?o Fechamento",
              "price.adjusted",
              "price.close",
              "price.high",
              "price.low",
              "price.open",
              "ref.date",
              "ret.adjusted.prices",
              "ret.closing.prices",
              "setor",
              "ticker",
              "Valuation Multiplo EBITDA",
              "Valuation P/L",
              "Varia??o Pre?o Alvo EBITDA",
              "Varia??o Pre?o Alvo P/L",
              "VL_CONTA",
              "volume"
)




head(a)
memory.limit (9999999999)


### PA EBITDA ###
x <- as.data.frame( a$pa_EBITDA > a[1:12])
x$maximo <- as.numeric(x$maximo )
x$minimo <- as.numeric(x$minimo )
x$fibo79 <- as.numeric(x$fibo79 )
x$fibo62 <- as.numeric(x$fibo62 )
x$fibo50 <- as.numeric(x$fibo50 )
x$fibo38 <- as.numeric(x$fibo38 )
x$fibo24 <- as.numeric(x$fibo24 )
x$fibo179 <- as.numeric(x$fibo179 )
x$fibo162 <- as.numeric(x$fibo162 )
x$fibo150 <- as.numeric(x$fibo150 )
x$fibo138 <- as.numeric(x$fibo138 )
x$fibo124 <- as.numeric(x$fibo124 )

a$fibo_pa_ebitda <- apply(x, 1, sum)

x <- as.data.frame( a$pa_EBITDA > a[25:36 ])
x$maximo_90 <- as.numeric(x$maximo_90 )
x$minimo_90 <- as.numeric(x$minimo_90 )
x$fibo79_90 <- as.numeric(x$fibo79_90 )
x$fibo62_90 <- as.numeric(x$fibo62_90 )
x$fibo50_90 <- as.numeric(x$fibo50_90 )
x$fibo38_90 <- as.numeric(x$fibo38_90 )
x$fibo24_90 <- as.numeric(x$fibo24_90 )
x$fibo179_90 <- as.numeric(x$fibo179_90 )
x$fibo162_90 <- as.numeric(x$fibo162_90 )
x$fibo150_90 <- as.numeric(x$fibo150_90 )
x$fibo138_90 <- as.numeric(x$fibo138_90 )
x$fibo124_90 <- as.numeric(x$fibo124_90 )
a$fibo_pa_ebitda_90 <- apply(x, 1, sum)


x <- as.data.frame( a$pa_EBITDA > a[13:24 ])
x$maximo_30 <- as.numeric(x$maximo_30 )
x$minimo_30 <- as.numeric(x$minimo_30 )
x$fibo79_30 <- as.numeric(x$fibo79_30 )
x$fibo62_30 <- as.numeric(x$fibo62_30 )
x$fibo50_30 <- as.numeric(x$fibo50_30 )
x$fibo38_30 <- as.numeric(x$fibo38_30 )
x$fibo24_30 <- as.numeric(x$fibo24_30 )
x$fibo179_30 <- as.numeric(x$fibo179_30 )
x$fibo162_30 <- as.numeric(x$fibo162_30 )
x$fibo150_30 <- as.numeric(x$fibo150_30 )
x$fibo138_30 <- as.numeric(x$fibo138_30 )
x$fibo124_30 <- as.numeric(x$fibo124_30 )
a$fibo_pa_ebitda_30 <- apply(x, 1, sum)



### PA PL ###
x <- as.data.frame( a$pa_pl > a[1:12])
x$maximo <- as.numeric(x$maximo )
x$minimo <- as.numeric(x$minimo )
x$fibo79 <- as.numeric(x$fibo79 )
x$fibo62 <- as.numeric(x$fibo62 )
x$fibo50 <- as.numeric(x$fibo50 )
x$fibo38 <- as.numeric(x$fibo38 )
x$fibo24 <- as.numeric(x$fibo24 )
x$fibo179 <- as.numeric(x$fibo179 )
x$fibo162 <- as.numeric(x$fibo162 )
x$fibo150 <- as.numeric(x$fibo150 )
x$fibo138 <- as.numeric(x$fibo138 )
x$fibo124 <- as.numeric(x$fibo124 )
a$fibo_pa_pl <- apply(x, 1, sum)


x <- as.data.frame( a$pa_pl > a[25:36 ])
x$maximo_90 <- as.numeric(x$maximo_90 )
x$minimo_90 <- as.numeric(x$minimo_90 )
x$fibo79_90 <- as.numeric(x$fibo79_90 )
x$fibo62_90 <- as.numeric(x$fibo62_90 )
x$fibo50_90 <- as.numeric(x$fibo50_90 )
x$fibo38_90 <- as.numeric(x$fibo38_90 )
x$fibo24_90 <- as.numeric(x$fibo24_90 )
x$fibo179_90 <- as.numeric(x$fibo179_90 )
x$fibo162_90 <- as.numeric(x$fibo162_90 )
x$fibo150_90 <- as.numeric(x$fibo150_90 )
x$fibo138_90 <- as.numeric(x$fibo138_90 )
x$fibo124_90 <- as.numeric(x$fibo124_90 )
a$fibo_pa_pl_90 <- apply(x, 1, sum)


x <- as.data.frame( a$pa_pl > a[13:24 ])
x$maximo_30 <- as.numeric(x$maximo_30 )
x$minimo_30 <- as.numeric(x$minimo_30 )
x$fibo79_30 <- as.numeric(x$fibo79_30 )
x$fibo62_30 <- as.numeric(x$fibo62_30 )
x$fibo50_30 <- as.numeric(x$fibo50_30 )
x$fibo38_30 <- as.numeric(x$fibo38_30 )
x$fibo24_30 <- as.numeric(x$fibo24_30 )
x$fibo179_30 <- as.numeric(x$fibo179_30 )
x$fibo162_30 <- as.numeric(x$fibo162_30 )
x$fibo150_30 <- as.numeric(x$fibo150_30 )
x$fibo138_30 <- as.numeric(x$fibo138_30 )
x$fibo124_30 <- as.numeric(x$fibo124_30 )
a$fibo_pa_pl_30 <- apply(x, 1, sum)


### PA Damodaran EBITDA ###
x <- as.data.frame( a$Pa_damodaran_ebitda > a[1:12])
x$maximo <- as.numeric(x$maximo )
x$minimo <- as.numeric(x$minimo )
x$fibo79 <- as.numeric(x$fibo79 )
x$fibo62 <- as.numeric(x$fibo62 )
x$fibo50 <- as.numeric(x$fibo50 )
x$fibo38 <- as.numeric(x$fibo38 )
x$fibo24 <- as.numeric(x$fibo24 )
x$fibo179 <- as.numeric(x$fibo179 )
x$fibo162 <- as.numeric(x$fibo162 )
x$fibo150 <- as.numeric(x$fibo150 )
x$fibo138 <- as.numeric(x$fibo138 )
x$fibo124 <- as.numeric(x$fibo124 )
a$fibo_Pa_damodaran_ebitda <- apply(x, 1, sum)


x <- as.data.frame( a$Pa_damodaran_ebitda > a[25:36 ])
x$maximo_90 <- as.numeric(x$maximo_90 )
x$minimo_90 <- as.numeric(x$minimo_90 )
x$fibo79_90 <- as.numeric(x$fibo79_90 )
x$fibo62_90 <- as.numeric(x$fibo62_90 )
x$fibo50_90 <- as.numeric(x$fibo50_90 )
x$fibo38_90 <- as.numeric(x$fibo38_90 )
x$fibo24_90 <- as.numeric(x$fibo24_90 )
x$fibo179_90 <- as.numeric(x$fibo179_90 )
x$fibo162_90 <- as.numeric(x$fibo162_90 )
x$fibo150_90 <- as.numeric(x$fibo150_90 )
x$fibo138_90 <- as.numeric(x$fibo138_90 )
x$fibo124_90 <- as.numeric(x$fibo124_90 )
a$fibo_Pa_damodaran_ebitda_90 <- apply(x, 1, sum)


x <- as.data.frame( a$Pa_damodaran_ebitda > a[13:24 ])
x$maximo_30 <- as.numeric(x$maximo_30 )
x$minimo_30 <- as.numeric(x$minimo_30 )
x$fibo79_30 <- as.numeric(x$fibo79_30 )
x$fibo62_30 <- as.numeric(x$fibo62_30 )
x$fibo50_30 <- as.numeric(x$fibo50_30 )
x$fibo38_30 <- as.numeric(x$fibo38_30 )
x$fibo24_30 <- as.numeric(x$fibo24_30 )
x$fibo179_30 <- as.numeric(x$fibo179_30 )
x$fibo162_30 <- as.numeric(x$fibo162_30 )
x$fibo150_30 <- as.numeric(x$fibo150_30 )
x$fibo138_30 <- as.numeric(x$fibo138_30 )
x$fibo124_30 <- as.numeric(x$fibo124_30 )
a$fibo_Pa_damodaran_ebitda_30 <- apply(x, 1, sum)




a$id_aux1<- a$id + 1
a$id_aux2<- a$id + 2
a$id_aux3<- a$id + 3
a$id_aux4<- a$id + 4
a$id_aux5<- a$id + 5
a$id_aux10<- a$id + 10
a$id_aux15<- a$id + 15
a$id_aux30<- a$id + 30
a$id_aux45<- a$id + 45
a$id_aux60<- a$id + 60
a$id_aux90<- a$id + 90
x <- a

a <- x

y <- data.frame(
  a$price.open,
  a$price.high,
  a$price.low,
  a$price.close,
  a$volume,
  a$price.adjusted,
  a$ref.date,
  a$ticker,
  a$setor,
  a$'Pre?o Fechamento',
  a$pa_EBITDA,
  a$pa_pl,
  a$Pa_damodaran_ebitda,
  a$fibo_preco,
  a$fibo_preco_900,
  a$fibo_preco_30,
  a$fibo_preco_90,
  a$fibo_pa_ebitda_90,
  a$fibo_pa_ebitda_30,
  a$fibo_pa_pl_90,
  a$fibo_pa_pl_30,
  a$fibo_Pa_damodaran_ebitda,
  a$fibo_Pa_damodaran_ebitda_90,
  a$fibo_Pa_damodaran_ebitda_30,
  a$fibo_pa_ebitda,
  a$fibo_pa_pl,
  a$id,
  a$id_aux1,
  a$id_aux2,
  a$id_aux3,
  a$id_aux4,
  a$id_aux5,
  a$id_aux10,
  a$id_aux15,
  a$id_aux30,
  a$id_aux45,
  a$id_aux60,
  a$id_aux90,
  a$preco
)


names(y) <- c("price.open",
              "price.high",
              "price.low",
              "price.close",
              "volume",
              "price.adjusted",
              "ref.date",
              "ticker",
              "setor",
              "'preco_fechamento",
              "pa_EBITDA",
              "pa_pl",
              "Pa_damodaran_ebitda",
              "fibo_preco",
              "fibo_preco_900",
              "fibo_preco_30",
              "fibo_preco_90",
              "fibo_pa_ebitda_90",
              "fibo_pa_ebitda_30",
              "fibo_pa_pl_90",
              "fibo_pa_pl_30",
              "fibo_Pa_damodaran_ebitda",
              "fibo_Pa_damodaran_ebitda_90",
              "fibo_Pa_damodaran_ebitda_30",
              "fibo_pa_ebitda",
              "fibo_pa_pl",
              "id",
              "id_aux1",
              "id_aux2",
              "id_aux3",
              "id_aux4",
              "id_aux5",
              "id_aux10",
              "id_aux15",
              "id_aux30",
              "id_aux45",
              "id_aux60",
              "id_aux90",
              "preco")



dbWriteTable(storiesDb, name = "base_arvore3", y, overwrite = TRUE)
dbWriteTable(storiesDb, name = "base_arvore_bkp", a, overwrite = TRUE)


b <- a
head(y)



x <- y

y <- data.frame(x$id, x$ticker, x$preco)

y <- unique(y)

##### build foward price database


names(y) <- c("id", "ticker", "preco1")
x <-
  left_join(x,
            y,
            by = c("id_aux1" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco2")
x <-
  left_join(x,
            y,
            by = c("id_aux2" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco3")
x <-
  left_join(x,
            y,
            by = c("id_aux3" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco4")
x <-
  left_join(x,
            y,
            by = c("id_aux4" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco5")
x <-
  left_join(x,
            y,
            by = c("id_aux5" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco10")
x <-
  left_join(x,
            y,
            by = c("id_aux10" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco15")
x <-
  left_join(x,
            y,
            by = c("id_aux15" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco30")
x <-
  left_join(x,
            y,
            by = c("id_aux30" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco45")
x <-
  left_join(x,
            y,
            by = c("id_aux45" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco60")
x <-
  left_join(x,
            y,
            by = c("id_aux60" = "id", "ticker" = "ticker"),
            keep = FALSE)
names(y) <- c("id", "ticker", "preco90")
x <-
  left_join(x,
            y,
            by = c("id_aux90" = "id", "ticker" = "ticker"),
            keep = FALSE)
x$ret1 <- ((x$preco1 / x$preco) ^ (1 / 1) - 1)
x$ret2 <- ((x$preco2 / x$preco) ^ (1 / 2) - 1)
x$ret3 <- ((x$preco3 / x$preco) ^ (1 / 3) - 1)
x$ret4 <- ((x$preco4 / x$preco) ^ (1 / 4) - 1)
x$ret5 <- ((x$preco5 / x$preco) ^ (1 / 5) - 1)
x$ret10 <- ((x$preco10 / x$preco) ^ (1 / 10) - 1)
x$ret15 <- ((x$preco15 / x$preco) ^ (1 / 15) - 1)
x$ret30 <- ((x$preco30 / x$preco) ^ (1 / 30) - 1)
x$ret45 <- ((x$preco45 / x$preco) ^ (1 / 45) - 1)
x$ret60 <- ((x$preco60 / x$preco) ^ (1 / 60) - 1)
x$ret90 <- ((x$preco90 / x$preco) ^ (1 / 90) - 1)
x$ret_ebitda <- x$pa_EBITDA / x$preco
x$ret_pl <- x$pa_pl / x$preco
x$ret_dam_ebitda <- x$Pa_damodaran_ebitda / x$preco

### Exclude data inconsistence by stock split

x$ret_ebitda[x$ret_ebitda > 3] = NA
x$ret_pl[x$ret_pl > 3] = NA
x$ret_dam_ebitda[x$ret_dam_ebitda > 3] = NA
x$ret_ebitda[x$ret_ebitda < 0] = NA
x$ret_pl[x$ret_pl < 0] = NA
x$ret_dam_ebitda[x$ret_dam_ebitda < 0] = NA
