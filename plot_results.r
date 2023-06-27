##################################################################
#   Vovinha Project (Fibonacci retracement and Financial Multiples for Binary Trees)
#   File 5: plot_results.r
#   Objective: plot model results
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


#### Plot results



### Montar posicoes ####


VarImpPlot(model)
varImp(model)
prp(model)
model
plot(model, uniform=TRUE, margin=0.1)
text(model, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
rpart.plot(model,type = 3)
plotcp(model)
rsq.rpart(model)
printcp(model)
model$cptable
summary(model)
text(model, pretty = 0)

                

tabela_est_base_treino <- data.frame(c(
  mean(na.omit(y$ret10)),
  mean(na.omit(y$ret15)),
  mean(na.omit(y$ret30)),
  mean(na.omit(y$ret45)),
  mean(na.omit(y$ret60)),
  mean(na.omit(y$ret90))
),

c(
  sd(na.omit(y$ret10)),
  sd(na.omit(y$ret15)),
  sd(na.omit(y$ret30)),
  sd(na.omit(y$ret45)),
  sd(na.omit(y$ret60)),
  sd(na.omit(y$ret90))
))

tabela_est_base_teste <- data.frame(c(
  mean(na.omit(teste$ret10)),
  mean(na.omit(teste$ret15)),
  mean(na.omit(teste$ret30)),
  mean(na.omit(teste$ret45)),
  mean(na.omit(teste$ret60)),
  mean(na.omit(teste$ret90))
),

c(
  sd(na.omit(teste$ret10)),
  sd(na.omit(teste$ret15)),
  sd(na.omit(teste$ret30)),
  sd(na.omit(teste$ret45)),
  sd(na.omit(teste$ret60)),
  sd(na.omit(teste$ret90))
))

library(tidyverse)
library(hrbrthemes)
library(viridis)

install.packages("hrbrthemes")
install.packages("viridis")

memory.limit(99999)

setwd("C:/Users/augusto.bastos/OneDrive - Ara�jo Fontes Ltda/�rea de Trabalho/UFMG/TCC/Tese/Tabelas/r")

jpeg(file="hist1.jpeg")
hist(y$ret10, 
     main="Retorno 10 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret10)), col= 'red',lwd=3, lty=2)
dev.off()

jpeg(file="hist2.jpeg")
hist(y$ret15, 
     main="Retorno 15 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret15)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist3.jpeg")
hist(y$ret30, 
     main="Retorno 30 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret30)), col= 'red',lwd=3, lty=2)
dev.off()



jpeg(file="hist4.jpeg")
hist(y$ret45, 
     main="Retorno 45 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret45)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist5.jpeg")
hist(y$ret60, 
     main="Retorno 60 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret60)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist6.jpeg")
hist(y$ret90, 
     main="Retorno 90 dias", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(-0.1,0.1),
     ylim = c(1000,50000),
     las=1, 
     breaks=500)
abline(v = mean(na.omit(y$ret90)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist7.jpeg")
hist(y$ret_dam_ebitda, 
     main="Retorno Alvo Multiplo EBITDA Damodaran", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(0.1,10),
     ylim = c(100,50000),
     las=1, 
     breaks=50)
abline(v = mean(na.omit(y$ret_dam_ebitda)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist8.jpeg")
hist(y$ret_ebitda, 
     main="Retorno Alvo Multiplo EBITDA Calculado", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(0.1,10),
     ylim = c(100,50000),
     las=1, 
     breaks=50)
abline(v = mean(na.omit(y$ret_ebitda)), col= 'red',lwd=3, lty=2)
dev.off()


jpeg(file="hist9.jpeg")
hist(y$ret_pl, 
     main="Retorno Alvo Multiplo Pre�o Lucro", 
     xlab="Retorno", 
     border="blue", 
     col="gray",
     xlim=c(0.1,10),
     ylim = c(100,50000),
     las=1, 
     breaks=50)
abline(v = mean(na.omit(y$ret_pl)), col= 'red',lwd=3, lty=2)
dev.off()

titulo <- c("	 Fibonacci para Pre�o Corrente o Pre�o Hist.",
            "	Erro	",
            "	 Fibonacci para Pre�o Corrente 30 Per.",
            "	 Fibonacci para Pre�o Corrente 90 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Calculado) 90 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Calculado) 30 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo P/L 90 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo P/L 30 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Damodaran) Pre�o Hist.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Damodaran)  30 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Damodaran) 90 Per.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo EBITDA (Calculado) Pre�o Hist.",
            "	 Fibonacci para Pre�o Alvo M�ltiplo P/LPre�o Hist."
)
nomes <- names(y)

for (i in 14:26) {

jpeg(file=paste("hist",nomes[i],".jpeg"))
hist(y[,i], 
     main= titulo[i-13] , 
     xlab="Retorno", 
     ylab = "Frequencia",
     border="blue", 
     col="gray",
     xlim=c(0,13),
     ylim = c(100,50000),
     las=1, 
     breaks=50)
abline(v = mean(na.omit(y$ret_pl)), col= 'red',lwd=3, lty=2)
dev.off()

}

amox <- resultado$ret90
amox[is.na(amox)] = 0
amoy <- resultado$`pred 61`
amox[is.na(amoy)] = 0
cor(amox, amoy)

cor(resultado$ret90, resultado$`pred 61`)
