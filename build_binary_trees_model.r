##################################################################
#   Vovinha Project (Fibonacci retracement and Financial Multiples for Binary Trees)
#   File 4: build_binary_trees_model.r
#   Objective: Buld Binary trees model
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


##### Build model

y <- subset(x, ref.date <= '2018-01-01')
names(x)
teste <-
  subset(x, ref.date >= '2018-01-01' & ref.date <= '2020-01-01')
resultado <- teste
treino <- y
modelos <- list
for (i in 52:61) {
  model <- rpart(
    y[, i] ~ setor +
      fibo_preco +
      fibo_preco_30 +
      fibo_preco_90 +
      fibo_pa_ebitda_90 +
      fibo_pa_ebitda_30 +
      fibo_pa_pl_90 +
      fibo_pa_pl_30 +
      fibo_Pa_damodaran_ebitda +
      fibo_Pa_damodaran_ebitda_90 +
      fibo_Pa_damodaran_ebitda_30 +
      fibo_pa_ebitda +
      fibo_pa_pl +
      ret_ebitda +
      ret_pl +
      ret_dam_ebitda,
    data = y,
    minbucket = nrow(y) * 0.01,
    method = "anova",
    usesurrogate = 0,
    model = TRUE
  )
  modelos[i - 51] <- model
  if (nrow(model$frame) > 1) {
    pred <-
      as.data.frame(predict(model, newdata = teste), col.names = "pred")
    names(pred) <- paste("pred", i)
    treino_aux <-
      as.data.frame(predict(model, newdata = y), col.names = "treino")
    names(treino_aux) <- paste("treino", i)
    ### Montar posicoes ####
    resultado <-  cbind(resultado, pred)
    treino <- cbind(treino, treino_aux)
  }
}
rm(teste_nos)

for (i in 56:61) {
  base_nos <- as.data.frame(unique(resultado[, i + 9]))
  for (j in 1:nrow(base_nos)) {
    pred_no <- subset(resultado, resultado[, i + 9] == base_nos[j, ])
    treino_no <- subset(treino, treino[, i + 9] == base_nos[j, ])
    ### testes ####
    t_real <- treino_no[, i] > 0
    t_proj <- treino_no[, i + 9] > 0
    t_acerto <-
      sum(as.numeric(na.omit(t_real == t_proj))) / nrow(as.data.frame(na.omit(t_real ==
                                                                                t_proj)))
    t_retm_no <- mean(na.omit(treino_no[, i]))
    t_sdret_no <- sd(na.omit(treino_no[, i]))
    t_cont_no <- nrow(as.data.frame(na.omit(t_real == t_proj)))
    t_ks_norm <-
      ks.test(na.omit(treino_no[, i]),
              "pnorm",
              mean = t_retm_no,
              sd = t_sdret_no)
    t_retorno <-
      ifelse(na.omit(t_real == t_proj),
             1 * abs(na.omit(treino_no[, i])),-1 * abs(na.omit(treino_no[, i])))
    t_retm_retorno <- mean(na.omit(t_retorno))
    t_sdret_retorno <- sd(na.omit(t_retorno))
    p_real <- pred_no[, i] > 0
    p_proj <- pred_no[, i + 9] > 0
    p_acerto <-
      sum(as.numeric(na.omit(p_real == p_proj))) / nrow(as.data.frame(na.omit(p_real ==
                                                                                p_proj)))
    p_retm_no <- mean(na.omit(pred_no[, i]))
    p_sdret_no <- sd(na.omit(pred_no[, i]))
    p_cont_no <- nrow(as.data.frame(na.omit(p_real == p_proj)))
    p_ks_norm <-
      ks.test(na.omit(pred_no[, i]),
              "pnorm",
              mean = p_retm_no,
              sd = p_sdret_no)
    p_retorno <-
      ifelse(na.omit(p_real == p_proj),
             1 * abs(na.omit(treino_no[, i])),-1 * abs(na.omit(treino_no[, i])))
    p_retm_retorno <- mean(na.omit(p_retorno))
    p_sdret_retorno <- sd(na.omit(p_retorno))
    ks_pred <- ks.test(na.omit(treino_no[, i]), na.omit(pred_no[, i]))
    aux_teste_nos <- data.frame(
      as.character(names(resultado[i])),
      base_nos[j, ],
      t_acerto ,
      t_retm_no ,
      t_sdret_no ,
      t_cont_no ,
      t_retm_retorno,
      t_sdret_retorno,
      as.numeric(t_ks_norm$statistic),
      as.numeric(t_ks_norm$p.value) ,
      p_acerto ,
      p_retm_no ,
      p_sdret_no ,
      p_cont_no ,
      p_retm_retorno,
      p_sdret_retorno,
      as.numeric(p_ks_norm$statistic),
      as.numeric(p_ks_norm$p.value) ,
      as.numeric(ks_pred$statistic),
      as.numeric(ks_pred$p.value)
    )
    if (!exists("teste_nos")) {
      teste_nos <- aux_teste_nos
    }
    teste_nos <- rbind(teste_nos, aux_teste_nos)
  }
}
wb <- createWorkbook()
#
addWorksheet(wb, "resultados")
writeData(wb,
          "resultados",
          teste_nos,
          rowNames = TRUE,
          sep = ".")
saveWorkbook(wb, "resutados.xlsx", overwrite = TRUE)
mean(na.omit(y$ret1))
mean(na.omit(y$ret10))
mean(na.omit(y$ret15))
mean(na.omit(teste$ret15))
mean(na.omit(teste$ret15))
mean(na.omit(teste$ret10))
mean(na.omit(teste$ret15))
mean(na.omit(teste$ret30))
mean(na.omit(teste$ret45))
mean(na.omit(teste$ret60))
mean(na.omit(teste$ret90))


ks.test(resultado$ret10[resultado$pred10 == max(resultado$pred10)],"pnorm",mean(resultado$ret10[resultado$pred10 == max(resultado$pred10)]),sd((resultado$ret10[resultado$pred10 == max(resultado$pred10)]))) 

# posicoes <- subset(resultados, pred > 0.05 | pred < -0.05 )

posicoes <- resultado[!is.na(resultado[ , i ]),]
posicoes <- posicoes[!is.na(posicoes$pred),]



posicoes$pos_projetada <- ifelse(posicoes$pred > 0.00, "call", "put")

posicoes$pos_real <- ifelse(posicoes[ , i ] > 0.00, "call", "put")

table(posicoes$pos_projetada, posicoes$pos_real)

confusionMatrix(table(posicoes$pos_projetada, posicoes$pos_real))

posicoes$acerto <-
  ifelse(
    posicoes$pos_projetada == posicoes$pos_real,
    1 * abs(posicoes[ , i ]),
    -1 * abs(posicoes[ , i ])
  )



summary(posicoes$acerto)

hist(posicoes$acerto)
