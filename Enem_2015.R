
# Limpa ambiente de trabalho 
# ===================================
rm(list = ls())
cat("\014")


# Bibliotecas com modelos de predição 
# ===================================
library("nnet")
library("tree")
library("e1071")
library("FNN")
library("randomForest")
library("party")


# Bibliotecas gráficas
# ===================================
library("tabplot")
library("ggplot2")
library("shiny")
#library("partykit")


# Bibliotecas para manipulação de arquivos 
# ===================================
#library(readr)  # CSV
library(readxl) # XLS


# Carrega dados de arquivos CSV  
# ===================================
#
# Falta alterar , para .
#ENEM_2015_CH <- read_delim("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/ENEM_2015-CH.csv", ";", escape_double = FALSE, trim_ws = TRUE, fileEncoding = "utf-8"))
#ENEM_2015_CN <- read_delim("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/ENEM_2015-CN.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#ENEM_2015_LC <- read_delim("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/ENEM_2015-LC.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#ENEM_2015_MT <- read_delim("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/ENEM_2015-MT.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#ENEM_2015_RED <- read_delim("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/ENEM_2015-RED.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#
# Modifiquei para utilizar o formato XLS como entrada (ver abaixo)


# Carrega dados de arquivos XLS
# ===================================
ENEM_2015_CH <- read_excel("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/PLANILHA_ENEM_ESCOLA_2015.xlsx", sheet = "CH")
ENEM_2015_CN <- read_excel("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/PLANILHA_ENEM_ESCOLA_2015.xlsx", sheet = "CN")
ENEM_2015_LC <- read_excel("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/PLANILHA_ENEM_ESCOLA_2015.xlsx", sheet = "LC")
ENEM_2015_MT <- read_excel("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/PLANILHA_ENEM_ESCOLA_2015.xlsx", sheet = "MT")
ENEM_2015_RED <- read_excel("~/Dropbox/BI Master/Trabalhos/Redes Neurais/Files/PLANILHA_ENEM_ESCOLA_2015.xlsx", sheet = "RED")


#converte atributos categóricos em fator 
characterIndexes = c(2,3,4,5,6,8,12,13,14)
for(i in characterIndexes)
{
  # Forma original informada
  #ENEM_2015_CN[,i] = as.factor(ENEM_2015_CN[,i])
  
  # Forma ajustada para resolver o erro
  ENEM_2015_CH[,i] = as.factor(unlist(ENEM_2015_CH[,i]))
  ENEM_2015_CN[,i] = as.factor(unlist(ENEM_2015_CN[,i]))
  ENEM_2015_LC[,i] = as.factor(unlist(ENEM_2015_LC[,i]))
  ENEM_2015_MT[,i] = as.factor(unlist(ENEM_2015_MT[,i]))
  ENEM_2015_RED[,i] = as.factor(unlist(ENEM_2015_RED[,i]))
}

#deleta linhas que contenham NA (missing values)
ENEM_2015_CH = na.omit(ENEM_2015_CH)
ENEM_2015_CN = na.omit(ENEM_2015_CN)
ENEM_2015_LC = na.omit(ENEM_2015_LC)
ENEM_2015_MT = na.omit(ENEM_2015_MT)
ENEM_2015_RED = na.omit(ENEM_2015_RED)
#
# Atenção: Reduz o número de observações de 15.567 para 9.956


# Separa dados em Treinamento e Teste 
# ===================================
# Removidas as colunas não relevantes:
# - CÓDIGO DA ENTIDADE (um por escola, não permite generalização)
# - NOME DA ENTIDADE (um por escola, não permite generalização)
# - NOME MUNICÍPIO (Devido a pequena amostragem em algumas UFs o Teste incluia elementos que não fizeram parte do Treinamento 
#   apresentando erro. Mais detalhes abaixo)
#
# OBJETIVO: REGRESSÃO LINEAR:
# - MÉDIA ESCOLA (Coluna 20)

set.seed(0)
ENEM_2015_CH_indexes = sample(1:nrow(ENEM_2015_CH), size=0.3*nrow(ENEM_2015_CH))
ENEM_2015_CH_train = ENEM_2015_CH[-ENEM_2015_CH_indexes,c(3,5:18,20)]
ENEM_2015_CH_test = ENEM_2015_CH[ENEM_2015_CH_indexes,c(3,5:18,20)]

ENEM_2015_CN_indexes = sample(1:nrow(ENEM_2015_CN), size=0.3*nrow(ENEM_2015_CN))
ENEM_2015_CN_train = ENEM_2015_CN[-ENEM_2015_CN_indexes,c(3,5:18,20)]
ENEM_2015_CN_test = ENEM_2015_CN[ENEM_2015_CN_indexes,c(3,5:18,20)]

ENEM_2015_LC_indexes = sample(1:nrow(ENEM_2015_LC), size=0.3*nrow(ENEM_2015_LC))
ENEM_2015_LC_train = ENEM_2015_LC[-ENEM_2015_LC_indexes,c(3,5:18,20)]
ENEM_2015_LC_test = ENEM_2015_LC[ENEM_2015_LC_indexes,c(3,5:18,20)]

ENEM_2015_MT_indexes = sample(1:nrow(ENEM_2015_MT), size=0.3*nrow(ENEM_2015_MT))
ENEM_2015_MT_train = ENEM_2015_MT[-ENEM_2015_MT_indexes,c(3,5:18,20)]
ENEM_2015_MT_test = ENEM_2015_MT[ENEM_2015_MT_indexes,c(3,5:18,20)]

ENEM_2015_RED_indexes = sample(1:nrow(ENEM_2015_RED), size=0.3*nrow(ENEM_2015_RED))
ENEM_2015_RED_train = ENEM_2015_RED[-ENEM_2015_RED_indexes,c(3,5:18,20)]
ENEM_2015_RED_test = ENEM_2015_RED[ENEM_2015_RED_indexes,c(3,5:18,20)]




# Visualização da Tabela com bibliotecas do R 
# =======================================================================================================================
# install.packages("shiny")
#itableplot()

# https://cran.r-project.org/web/packages/tabplot/vignettes/tabplot-vignette.html


tableplot(ENEM_2015_CH, sortCol=MÉDIA.ESCOLA, select=c(20, 3, 5:10), fontsize = 10, title = "Ciências Humanas (1/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)
tableplot(ENEM_2015_CH, sortCol=MÉDIA.ESCOLA, select=c(20, 11:13, 15:18), fontsize = 10, title = "Ciências Humanas (2/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)

tableplot(ENEM_2015_CN, sortCol=MÉDIA.ESCOLA, select=c(20, 3, 5:10), fontsize = 10, title = "Ciências da Natureza (1/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)
tableplot(ENEM_2015_CN, sortCol=MÉDIA.ESCOLA, select=c(20, 11:13, 15:18), fontsize = 10, title = "Ciências da Natureza (2/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)

tableplot(ENEM_2015_LC, sortCol=MÉDIA.ESCOLA, select=c(20, 3, 5:10), fontsize = 10, title = "Linguagens e Códigos (1/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)
tableplot(ENEM_2015_LC, sortCol=MÉDIA.ESCOLA, select=c(20, 11:13, 15:18), fontsize = 10, title = "Linguagens e Códigos (2/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)

tableplot(ENEM_2015_MT, sortCol=MÉDIA.ESCOLA, select=c(20, 3,5:10), fontsize = 10, title = "Matemática (1/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)
tableplot(ENEM_2015_MT, sortCol=MÉDIA.ESCOLA, select=c(20, 11:13, 15:18), fontsize = 10, title = "Matemática (2/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)

tableplot(ENEM_2015_RED, sortCol=MÉDIA.ESCOLA, select=c(20, 3, 5:10), fontsize = 10, title = "Redação (1/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)
tableplot(ENEM_2015_RED, sortCol=MÉDIA.ESCOLA, select=c(20, 11:13, 15:18), fontsize = 10, title = "Redação (2/2)", fontsize.title = 12, decreasing=TRUE, nBins=100, scales="lin", sample=TRUE)


# Início dos testes com Redes Neurais
# =======================================================================================================================
#
# Nota: Tamaho de redes escolhidos após testes de MAPE e tempo de processamento
#
system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=12, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
system.time(ENEM_2015_CN_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CN_train, size=10, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
system.time(ENEM_2015_LC_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_LC_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
system.time(ENEM_2015_MT_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_MT_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
system.time(ENEM_2015_RED_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_RED_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))

# > system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=12, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# usuário   sistema decorrido 
# 0.233     0.000     0.233 
# > system.time(ENEM_2015_CN_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CN_train, size=10, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# usuário   sistema decorrido 
# 0.213     0.000     0.212 
# > system.time(ENEM_2015_LC_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_LC_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# usuário   sistema decorrido 
# 29.903     0.000    29.949 
# > system.time(ENEM_2015_MT_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_MT_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# usuário   sistema decorrido 
# 21.307     0.000    21.321 
# > system.time(ENEM_2015_RED_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_RED_train, size=19, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# usuário   sistema decorrido 
# 64.590     0.000    64.582

#summary(ENEM_2015_CH_NN)
#summary(ENEM_2015_CN_NN)
#summary(ENEM_2015_LC_NN)
#summary(ENEM_2015_MT_NN)
#summary(ENEM_2015_RED_NN)


# Teste do modelo de Redes Neurais
# ===================================
ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
ENEM_2015_CN_NN_Predictions = predict(ENEM_2015_CN_NN, ENEM_2015_CN_test)
ENEM_2015_LC_NN_Predictions = predict(ENEM_2015_LC_NN, ENEM_2015_LC_test)
ENEM_2015_MT_NN_Predictions = predict(ENEM_2015_MT_NN, ENEM_2015_MT_test)
ENEM_2015_RED_NN_Predictions = predict(ENEM_2015_RED_NN, ENEM_2015_RED_test)
# Nota 1: Gerou um erro ao rodar com o fator NOME MUNICÍPIO: "factor NOME MUNICÍPIO has new levels". Os dados de teste incluiram 
# Municípios que não fizeram parte do processo de treinamento. Com isso refiz todo o processo de seleção de colunas e treinamento 
# de forma a remover esta coluna do treinamento e testes.
# 
# Original:
#ENEM_2015_CH_train = ENEM_2015_CH[-ENEM_2015_CH_indexes,c(3:18,20)]
#ENEM_2015_CH_test = ENEM_2015_CH[ENEM_2015_CH_indexes,c(3:18,20)]
#
#ENEM_2015_CN_train = ENEM_2015_CN[-ENEM_2015_CN_indexes,c(3:18,20)]
#ENEM_2015_CN_test = ENEM_2015_CN[ENEM_2015_CN_indexes,c(3:18,20)]
#
#ENEM_2015_LC_train = ENEM_2015_LC[-ENEM_2015_LC_indexes,c(3:18,20)]
#ENEM_2015_LC_test = ENEM_2015_LC[ENEM_2015_LC_indexes,c(3:18,20)]
#
#ENEM_2015_MT_train = ENEM_2015_MT[-ENEM_2015_MT_indexes,c(3:18,20)]
#ENEM_2015_MT_test = ENEM_2015_MT[ENEM_2015_MT_indexes,c(3:18,20)]
#
#ENEM_2015_RED_train = ENEM_2015_RED[-ENEM_2015_RED_indexes,c(3:18,20)]
#ENEM_2015_RED_test = ENEM_2015_RED[ENEM_2015_RED_indexes,c(3:18,20)]
#
#
# Nota 2: Ao remover o Município, o algorítmo rodou de forma muito mais ágil (saindo de 600 segundos para menos de 1) para um conjunto 
# de 10 neurônios na camada escondida. A avaliar se a agilidade não tornou a precisão prejudicial.


# Medição de erro do modelo de Redes Neurais
# ===================================
# Root Mean Squared Error
ENEM_2015_CH_NN_RMSE = sqrt(mean((ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)^2, na.rm = TRUE)) 
ENEM_2015_CN_NN_RMSE = sqrt(mean((ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_NN_Predictions)^2, na.rm = TRUE)) 
ENEM_2015_LC_NN_RMSE = sqrt(mean((ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_NN_Predictions)^2, na.rm = TRUE)) 
ENEM_2015_MT_NN_RMSE = sqrt(mean((ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_NN_Predictions)^2, na.rm = TRUE)) 
ENEM_2015_RED_NN_RMSE = sqrt(mean((ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_NN_Predictions)^2, na.rm = TRUE)) 

ENEM_2015_CH_NN_RMSE
ENEM_2015_CN_NN_RMSE
ENEM_2015_LC_NN_RMSE
ENEM_2015_MT_NN_RMSE
ENEM_2015_RED_NN_RMSE

# > ENEM_2015_CH_NN_RMSE
# [1] 41.07864
# > ENEM_2015_CN_NN_RMSE
# [1] 48.52818
# > ENEM_2015_LC_NN_RMSE
# [1] 28.6965
# > ENEM_2015_MT_NN_RMSE
# [1] 50.21069
# > ENEM_2015_RED_NN_RMSE
# [1] 51.57166

# MAPE:
ENEM_2015_CH_NN_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_CN_NN_MAPE = (sum(abs(ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_NN_Predictions)/ENEM_2015_CN_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_LC_NN_MAPE = (sum(abs(ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_NN_Predictions)/ENEM_2015_LC_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_MT_NN_MAPE = (sum(abs(ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_NN_Predictions)/ENEM_2015_MT_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_RED_NN_MAPE = (sum(abs(ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_NN_Predictions)/ENEM_2015_RED_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_RED_test$`MÉDIA ESCOLA`))*100

ENEM_2015_CH_NN_MAPE
ENEM_2015_CN_NN_MAPE
ENEM_2015_LC_NN_MAPE
ENEM_2015_MT_NN_MAPE
ENEM_2015_RED_NN_MAPE

# > ENEM_2015_CH_NN_MAPE
# [1] 5.806539
# > ENEM_2015_CN_NN_MAPE
# [1] 7.575686
# > ENEM_2015_LC_NN_MAPE
# [1] 4.289722
# > ENEM_2015_MT_NN_MAPE
# [1] 6.923036
# > ENEM_2015_RED_NN_MAPE
# [1] 6.729972


# A sequência abaixo inclui diversas experimentações feitas de forma a achar os parâmetros ideias para as redes neurais 
# aplicadas à cada área de conhecimento
# ===================================================
#

# Teste: Qual o tamanho da rede neural com menor RMSE
# ===================================
# print(paste("ENEM_2015_CH:"))
# for (NNSize in 2:50){
#   ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000)
#   ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
#   ENEM_2015_CH_RMSE = sqrt(mean((ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)^2, na.rm = TRUE))
#   print(paste("Neural Network Size: ", NNSize, " RMSE: ", ENEM_2015_CH_RMSE))
# }
# Resultado
# [1] "Neural Network Size:  2  RMSE:  40.7981359275534"
# [1] "Neural Network Size:  3  RMSE:  40.7981358783867"
# [1] "Neural Network Size:  4  RMSE:  40.7981359221665"
# [1] "Neural Network Size:  5  RMSE:  40.7981359103989"
# [1] "Neural Network Size:  6  RMSE:  40.7981359026533"
# [1] "Neural Network Size:  7  RMSE:  40.7981361025488"
# [1] "Neural Network Size:  8  RMSE:  40.7981359525928"
# [1] "Neural Network Size:  9  RMSE:  40.7981359138395"
# [1] "Neural Network Size:  10  RMSE:  40.7981359515197"
# [1] "Neural Network Size:  11  RMSE:  40.7861126463123"
# [1] "Neural Network Size:  12  RMSE:  40.7981359516778"
# [1] "Neural Network Size:  13  RMSE:  40.7981359564697"
# [1] "Neural Network Size:  14  RMSE:  40.7981359497742"
# [1] "Neural Network Size:  15  RMSE:  40.7981353715864"
# [1] "Neural Network Size:  16  RMSE:  28.8614076289062"
# [1] "Neural Network Size:  17  RMSE:  40.7981348925882"
# [1] "Neural Network Size:  18  RMSE:  21.6554140388126"
# [1] "Neural Network Size:  19  RMSE:  28.1324102339973"
# [1] "Neural Network Size:  20  RMSE:  27.8961836299292"
# [1] "Neural Network Size:  21  RMSE:  20.2522036370058"
# [1] "Neural Network Size:  22  RMSE:  40.798135921061"
# [1] "Neural Network Size:  23  RMSE:  21.5359898658163"
# [1] "Neural Network Size:  24  RMSE:  40.7981161555741"
# [1] "Neural Network Size:  25  RMSE:  28.2422674820731"
# [1] "Neural Network Size:  26  RMSE:  26.4321920491627"
# [1] "Neural Network Size:  27  RMSE:  24.7818875190246"
# [1] "Neural Network Size:  28  RMSE:  40.7981359845056"
# [1] "Neural Network Size:  29  RMSE:  28.0607448627497"
# [1] "Neural Network Size:  30  RMSE:  40.7981359651183"
# [1] "Neural Network Size:  31  RMSE:  40.7981359626898"
# [1] "Neural Network Size:  32  RMSE:  25.625045297987"
# [1] "Neural Network Size:  33  RMSE:  40.7981359694501"
# [1] "Neural Network Size:  34  RMSE:  19.7710354652319" <<<<<<
# [1] "Neural Network Size:  35  RMSE:  40.7981360063205"
# [1] "Neural Network Size:  36  RMSE:  26.9474127668062"
# [1] "Neural Network Size:  37  RMSE:  25.5543275635046"
# [1] "Neural Network Size:  38  RMSE:  40.7981145231667"
# [1] "Neural Network Size:  39  RMSE:  22.5482515808096"
# [1] "Neural Network Size:  40  RMSE:  25.4234655566583"
# [1] "Neural Network Size:  41  RMSE:  23.3418099542867"
# [1] "Neural Network Size:  42  RMSE:  20.3388591773582"
# [1] "Neural Network Size:  43  RMSE:  25.5110557421086"
# [1] "Neural Network Size:  44  RMSE:  23.6940682410115"
# [1] "Neural Network Size:  45  RMSE:  22.9384277312041"
# [1] "Neural Network Size:  46  RMSE:  22.9244405805845"
# [1] "Neural Network Size:  47  RMSE:  27.7062723723952"
# [1] "Neural Network Size:  48  RMSE:  27.1154058463105"
# [1] "Neural Network Size:  49  RMSE:  28.9545178136059"
# [1] "Neural Network Size:  50  RMSE:  25.3116759872644"


# print(paste("ENEM_2015_CN:"))
# for (NNSize in 2:50){
#   ENEM_2015_CN_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CN_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000)
#   ENEM_2015_CN_NN_Predictions = predict(ENEM_2015_CN_NN, ENEM_2015_CN_test)
#   ENEM_2015_CN_RMSE = sqrt(mean((ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_NN_Predictions)^2, na.rm = TRUE))
#   print(paste("Neural Network Size: ", NNSize, " RMSE: ", ENEM_2015_CN_RMSE))
# }
# [1] "Neural Network Size:  2  RMSE:  47.238634039708"
# [1] "Neural Network Size:  3  RMSE:  31.9754456364466"
# [1] "Neural Network Size:  4  RMSE:  47.2386342267334"
# [1] "Neural Network Size:  5  RMSE:  33.9117793096028"
# [1] "Neural Network Size:  6  RMSE:  47.2387574730143"
# [1] "Neural Network Size:  7  RMSE:  47.2386342570895"
# [1] "Neural Network Size:  8  RMSE:  47.2386341801703"
# [1] "Neural Network Size:  9  RMSE:  47.2386341715094"
# [1] "Neural Network Size:  10  RMSE:  47.238634177895"
# [1] "Neural Network Size:  11  RMSE:  47.2386333199117"
# [1] "Neural Network Size:  12  RMSE:  47.2386341820643"
# [1] "Neural Network Size:  13  RMSE:  47.2386341585584"
# [1] "Neural Network Size:  14  RMSE:  34.2757484340899"
# [1] "Neural Network Size:  15  RMSE:  33.1062772267036"
# [1] "Neural Network Size:  16  RMSE:  47.238634070787"
# [1] "Neural Network Size:  17  RMSE:  47.2386341579571"
# [1] "Neural Network Size:  18  RMSE:  47.238634187523"
# [1] "Neural Network Size:  19  RMSE:  47.2385456861104"
# [1] "Neural Network Size:  20  RMSE:  47.2386341370533"
# [1] "Neural Network Size:  21  RMSE:  47.2250069072249"
# [1] "Neural Network Size:  22  RMSE:  47.2386341819271"
# [1] "Neural Network Size:  23  RMSE:  23.3888362711886" <<<<
# [1] "Neural Network Size:  24  RMSE:  31.9005203298588"
# [1] "Neural Network Size:  25  RMSE:  25.0678585892331"
# [1] "Neural Network Size:  26  RMSE:  35.1992350482638"
# [1] "Neural Network Size:  27  RMSE:  47.2386341807557"
# [1] "Neural Network Size:  28  RMSE:  47.2386341517141"
# [1] "Neural Network Size:  29  RMSE:  33.0517981996791"
# [1] "Neural Network Size:  30  RMSE:  47.2386339427856"
# [1] "Neural Network Size:  31  RMSE:  23.4023218968234" 
# [1] "Neural Network Size:  32  RMSE:  31.8855913106578"
# [1] "Neural Network Size:  33  RMSE:  25.2223757274686"
# [1] "Neural Network Size:  34  RMSE:  47.2386341617976"
# [1] "Neural Network Size:  35  RMSE:  26.7618542454039"
# [1] "Neural Network Size:  36  RMSE:  31.3251995404254"
# [1] "Neural Network Size:  37  RMSE:  28.3558173786925"
# [1] "Neural Network Size:  38  RMSE:  32.769854318346"
# [1] "Neural Network Size:  39  RMSE:  33.3273539277826"
# [1] "Neural Network Size:  40  RMSE:  23.8701503505859"
# [1] "Neural Network Size:  41  RMSE:  27.3535250921142"
# [1] "Neural Network Size:  42  RMSE:  47.2384415749886"
# [1] "Neural Network Size:  43  RMSE:  24.2779567628048"
# [1] "Neural Network Size:  44  RMSE:  27.7810192452415"
# [1] "Neural Network Size:  45  RMSE:  31.3000149575931"
# [1] "Neural Network Size:  46  RMSE:  27.2092854551249"
# [1] "Neural Network Size:  47  RMSE:  25.5465666303892"
# [1] "Neural Network Size:  48  RMSE:  24.682195052156"
# [1] "Neural Network Size:  49  RMSE:  24.2132250856258"
# [1] "Neural Network Size:  50  RMSE:  24.3643434606759"


# print(paste("ENEM_2015_LC:"))
# for (NNSize in 2:50){
#   ENEM_2015_LC_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_LC_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000)
#   ENEM_2015_LC_NN_Predictions = predict(ENEM_2015_LC_NN, ENEM_2015_LC_test)
#   ENEM_2015_LC_RMSE = sqrt(mean((ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_NN_Predictions)^2, na.rm = TRUE))
#   print(paste("Neural Network Size: ", NNSize, " RMSE: ", ENEM_2015_LC_RMSE))
# }
# [1] "Neural Network Size:  2  RMSE:  41.5613072968125"
# [1] "Neural Network Size:  3  RMSE:  41.5613062483573"
# [1] "Neural Network Size:  4  RMSE:  41.5613062619032"
# [1] "Neural Network Size:  5  RMSE:  41.5613062720525"
# [1] "Neural Network Size:  6  RMSE:  41.5579557853686"
# [1] "Neural Network Size:  7  RMSE:  41.5613062039256"
# [1] "Neural Network Size:  8  RMSE:  41.5613062250429"
# [1] "Neural Network Size:  9  RMSE:  41.5613062373793"
# [1] "Neural Network Size:  10  RMSE:  41.5613062362349"
# [1] "Neural Network Size:  11  RMSE:  41.5613044806979"
# [1] "Neural Network Size:  12  RMSE:  41.5613062235556"
# [1] "Neural Network Size:  13  RMSE:  41.5613075246853"
# [1] "Neural Network Size:  14  RMSE:  41.5613062816468"
# [1] "Neural Network Size:  15  RMSE:  41.5613068854913"
# [1] "Neural Network Size:  16  RMSE:  41.5613064712388"
# [1] "Neural Network Size:  17  RMSE:  27.9956068518228"
# [1] "Neural Network Size:  18  RMSE:  25.3487634521699"
# [1] "Neural Network Size:  19  RMSE:  41.5613054865655"
# [1] "Neural Network Size:  20  RMSE:  41.5613062343662"
# [1] "Neural Network Size:  21  RMSE:  25.2472941772449"
# [1] "Neural Network Size:  22  RMSE:  19.9468496759238"
# [1] "Neural Network Size:  23  RMSE:  29.4586704259595"
# [1] "Neural Network Size:  24  RMSE:  41.5613051739223"
# [1] "Neural Network Size:  25  RMSE:  27.6737719834157"
# [1] "Neural Network Size:  26  RMSE:  21.6549040614547"
# [1] "Neural Network Size:  27  RMSE:  22.9822069636269"
# [1] "Neural Network Size:  28  RMSE:  41.5613062515796"
# [1] "Neural Network Size:  29  RMSE:  25.219930768798"
# [1] "Neural Network Size:  30  RMSE:  41.5549019567901"
# [1] "Neural Network Size:  31  RMSE:  20.2427302070468"
# [1] "Neural Network Size:  32  RMSE:  21.2621840984434"
# [1] "Neural Network Size:  33  RMSE:  29.4747785608459"
# [1] "Neural Network Size:  34  RMSE:  21.9207236842865"
# [1] "Neural Network Size:  35  RMSE:  27.4589050964786"
# [1] "Neural Network Size:  36  RMSE:  26.0231712677709"
# [1] "Neural Network Size:  37  RMSE:  26.4844429084251"
# [1] "Neural Network Size:  38  RMSE:  41.5613057768825"
# [1] "Neural Network Size:  39  RMSE:  20.4884697110344"
# [1] "Neural Network Size:  40  RMSE:  24.7899487715128"
# [1] "Neural Network Size:  41  RMSE:  23.3433003870624"
# [1] "Neural Network Size:  42  RMSE:  20.9806437830956"
# [1] "Neural Network Size:  43  RMSE:  21.6116519327183"
# [1] "Neural Network Size:  44  RMSE:  25.6002264156177"
# [1] "Neural Network Size:  45  RMSE:  19.7059322980696" <<<<
# [1] "Neural Network Size:  46  RMSE:  24.4471892403955"
# [1] "Neural Network Size:  47  RMSE:  28.6806272784295"
# [1] "Neural Network Size:  48  RMSE:  20.1210355873893"
# [1] "Neural Network Size:  49  RMSE:  41.5613058981905"
# [1] "Neural Network Size:  50  RMSE:  27.3864006061778"


# print(paste("ENEM_2015_MT:"))
# for (NNSize in 2:50){
#   ENEM_2015_MT_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_MT_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000)
#   ENEM_2015_MT_NN_Predictions = predict(ENEM_2015_MT_NN, ENEM_2015_MT_test)
#   ENEM_2015_MT_RMSE = sqrt(mean((ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_NN_Predictions)^2, na.rm = TRUE))
#   print(paste("Neural Network Size: ", NNSize, " RMSE: ", ENEM_2015_MT_RMSE))
# }
# [1] "Neural Network Size:  2  RMSE:  68.2786070869037"
# [1] "Neural Network Size:  3  RMSE:  67.3787987312821"
# [1] "Neural Network Size:  4  RMSE:  68.2786062298012"
# [1] "Neural Network Size:  5  RMSE:  68.2786062792606"
# [1] "Neural Network Size:  6  RMSE:  68.2786058661788"
# [1] "Neural Network Size:  7  RMSE:  68.2786063231288"
# [1] "Neural Network Size:  8  RMSE:  46.7900571526204"
# [1] "Neural Network Size:  9  RMSE:  68.2786062505904"
# [1] "Neural Network Size:  10  RMSE:  68.2786063692875"
# [1] "Neural Network Size:  11  RMSE:  41.8798709846356"
# [1] "Neural Network Size:  12  RMSE:  46.1125266526835"
# [1] "Neural Network Size:  13  RMSE:  68.2786064009441"
# [1] "Neural Network Size:  14  RMSE:  68.2786062974187"
# [1] "Neural Network Size:  15  RMSE:  56.1420354979245"
# [1] "Neural Network Size:  16  RMSE:  49.3434702536262"
# [1] "Neural Network Size:  17  RMSE:  68.2786062819481"
# [1] "Neural Network Size:  18  RMSE:  68.2786062662336"
# [1] "Neural Network Size:  19  RMSE:  46.5698955816106"
# [1] "Neural Network Size:  20  RMSE:  51.8009174195585"
# [1] "Neural Network Size:  21  RMSE:  51.0073840732736"
# [1] "Neural Network Size:  22  RMSE:  47.7082645348991"
# [1] "Neural Network Size:  23  RMSE:  48.539151317519"
# [1] "Neural Network Size:  24  RMSE:  68.2786063232927"
# [1] "Neural Network Size:  25  RMSE:  36.6934393749945"
# [1] "Neural Network Size:  26  RMSE:  47.3890304796194"
# [1] "Neural Network Size:  27  RMSE:  45.4797646200455"
# [1] "Neural Network Size:  28  RMSE:  34.8397372226462"
# [1] "Neural Network Size:  29  RMSE:  47.3265832942286"
# [1] "Neural Network Size:  30  RMSE:  47.0364648669355"
# [1] "Neural Network Size:  31  RMSE:  48.6109963142239"
# [1] "Neural Network Size:  32  RMSE:  36.7802332137644"
# [1] "Neural Network Size:  33  RMSE:  48.7573589900139"
# [1] "Neural Network Size:  34  RMSE:  35.012049167107"
# [1] "Neural Network Size:  35  RMSE:  45.7621281473509"
# [1] "Neural Network Size:  36  RMSE:  41.6703122958367"
# [1] "Neural Network Size:  37  RMSE:  36.9244811657167"
# [1] "Neural Network Size:  38  RMSE:  44.0699188747805"
# [1] "Neural Network Size:  39  RMSE:  68.278606340282"
# [1] "Neural Network Size:  40  RMSE:  40.9108546217501"
# [1] "Neural Network Size:  41  RMSE:  44.1009655815193"
# [1] "Neural Network Size:  42  RMSE:  39.9413437075334"
# [1] "Neural Network Size:  43  RMSE:  34.6077886099847" <<<<
# [1] "Neural Network Size:  44  RMSE:  47.5363721750586"
# [1] "Neural Network Size:  45  RMSE:  45.0996125712061"
# [1] "Neural Network Size:  46  RMSE:  40.6920080714525"
# [1] "Neural Network Size:  47  RMSE:  40.1241371786056"
# [1] "Neural Network Size:  48  RMSE:  45.1224625659008"
# [1] "Neural Network Size:  49  RMSE:  36.2824148136619"
# [1] "Neural Network Size:  50  RMSE:  41.3761621967181"


# print(paste("ENEM_2015_RED:"))
# for (NNSize in 2:50){
#   ENEM_2015_RED_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_RED_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000)
#   ENEM_2015_RED_NN_Predictions = predict(ENEM_2015_RED_NN, ENEM_2015_RED_test)
#   ENEM_2015_RED_RMSE = sqrt(mean((ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_NN_Predictions)^2, na.rm = TRUE))
#   print(paste("Neural Network Size: ", NNSize, " RMSE: ", ENEM_2015_RED_RMSE))
# }
# [1] "Neural Network Size:  2  RMSE:  42.0483092295277"
# [1] "Neural Network Size:  3  RMSE:  77.1050829269998"
# [1] "Neural Network Size:  4  RMSE:  77.1050831055506"
# [1] "Neural Network Size:  5  RMSE:  77.1050831515819"
# [1] "Neural Network Size:  6  RMSE:  77.1050831492508"
# [1] "Neural Network Size:  7  RMSE:  77.1050832997713"
# [1] "Neural Network Size:  8  RMSE:  77.1050832007871"
# [1] "Neural Network Size:  9  RMSE:  77.1050831722985"
# [1] "Neural Network Size:  10  RMSE:  53.9302864891834"
# [1] "Neural Network Size:  11  RMSE:  77.1050831737737"
# [1] "Neural Network Size:  12  RMSE:  77.1050835882155"
# [1] "Neural Network Size:  13  RMSE:  77.1050831560334"
# [1] "Neural Network Size:  14  RMSE:  47.2623166945942"
# [1] "Neural Network Size:  15  RMSE:  77.1050831321453"
# [1] "Neural Network Size:  16  RMSE:  76.5000425396126"
# [1] "Neural Network Size:  17  RMSE:  77.1050831581954"
# [1] "Neural Network Size:  18  RMSE:  77.1050833077544"
# [1] "Neural Network Size:  19  RMSE:  77.1050829420234"
# [1] "Neural Network Size:  20  RMSE:  54.5930292921496"
# [1] "Neural Network Size:  21  RMSE:  77.1050830235422"
# [1] "Neural Network Size:  22  RMSE:  77.1147421250127"
# [1] "Neural Network Size:  23  RMSE:  77.1050832838295"
# [1] "Neural Network Size:  24  RMSE:  53.3492056976367"
# [1] "Neural Network Size:  25  RMSE:  50.8981468779249"
# [1] "Neural Network Size:  26  RMSE:  41.9219260056406"
# [1] "Neural Network Size:  27  RMSE:  48.5703071837788"
# [1] "Neural Network Size:  28  RMSE:  48.2435620349879"
# [1] "Neural Network Size:  29  RMSE:  77.1050831547458"
# [1] "Neural Network Size:  30  RMSE:  49.5926698254843"
# [1] "Neural Network Size:  31  RMSE:  77.1050844657152"
# [1] "Neural Network Size:  32  RMSE:  42.3373193954117"
# [1] "Neural Network Size:  33  RMSE:  50.9887685824963"
# [1] "Neural Network Size:  34  RMSE:  49.3621027740167"
# [1] "Neural Network Size:  35  RMSE:  44.5263215993169"
# [1] "Neural Network Size:  36  RMSE:  54.3028518269606"
# [1] "Neural Network Size:  37  RMSE:  54.3981943931597"
# [1] "Neural Network Size:  38  RMSE:  46.5428969721935"
# [1] "Neural Network Size:  39  RMSE:  49.5695425723832"
# [1] "Neural Network Size:  40  RMSE:  50.6729311258948"
# [1] "Neural Network Size:  41  RMSE:  53.2878824280569"
# [1] "Neural Network Size:  42  RMSE:  49.465492427888"
# [1] "Neural Network Size:  43  RMSE:  49.1919719958722"
# [1] "Neural Network Size:  44  RMSE:  53.0400555484776"
# [1] "Neural Network Size:  45  RMSE:  49.6537609784507"
# [1] "Neural Network Size:  46  RMSE:  46.9576863298481"
# [1] "Neural Network Size:  47  RMSE:  50.9377798864721"
# [1] "Neural Network Size:  48  RMSE:  41.4907517758125" <<<<
# [1] "Neural Network Size:  49  RMSE:  53.1109323442266"
# [1] "Neural Network Size:  50  RMSE:  44.1733893068921"


# Teste: Alterar o decay de forma a avaliar o tempo de aprendizado / erro (usando MAPE)
# ===================================
# print(paste("ENEM_2015_CH:"))
# for (NNDecay in c(0.5, 0.1, 0.01, 0.001, 0.0001)){
#   system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=NNDecay, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#   ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
#   ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
#   print(paste("Neural Network Decay:", NNDecay, " MAPE:", ENEM_2015_CH_MAPE, "%"))
# }
# 
# system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
# ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
# print(paste("Neural Network Decay:", 0.0001, " MAPE:", ENEM_2015_CH_MAPE, "%"))
# 
# system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=0.001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
# ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
# print(paste("Neural Network Decay:", 0.001, " MAPE:", ENEM_2015_CH_MAPE, "%"))
# 
# system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=0.01, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
# ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
# print(paste("Neural Network Decay:", 0.01, " MAPE:", ENEM_2015_CH_MAPE, "%"))
# 
# system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=0.1, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
# ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
# print(paste("Neural Network Decay:", 0.1, " MAPE:", ENEM_2015_CH_MAPE, "%"))
# 
# system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=10, decay=0.5, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
# ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
# ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
# print(paste("Neural Network Decay:", 0.5, " MAPE:", ENEM_2015_CH_MAPE, "%"))


# Teste: Qual o tamanho da rede neural com menor MAPE
# ===================================
# print(paste("ENEM_2015_CH:"))
# for (NNSize in 2:50){
#   Time = system.time(ENEM_2015_CH_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CH_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#   ENEM_2015_CH_NN_Predictions = predict(ENEM_2015_CH_NN, ENEM_2015_CH_test)
#   ENEM_2015_CH_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_NN_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
#   print(paste("Neural Network Size:", NNSize, " MAPE:", ENEM_2015_CH_MAPE, "%", " Processing time:", Time[3]))
# }
# [1] "Neural Network Size: 2  MAPE: 5.80940998536347 %  Processing time: 0.150999999998021"
# [1] "Neural Network Size: 3  MAPE: 5.80940933142576 %  Processing time: 0.225999999998749"
# [1] "Neural Network Size: 4  MAPE: 5.8094091267829 %  Processing time: 0.17699999999968"
# [1] "Neural Network Size: 5  MAPE: 3.83388097066142 %  Processing time: 26.0879999999997"
# [1] "Neural Network Size: 6  MAPE: 5.80940779918266 %  Processing time: 0.258999999998196"
# [1] "Neural Network Size: 7  MAPE: 5.80940934983816 %  Processing time: 0.25800000000163"
# [1] "Neural Network Size: 8  MAPE: 5.80940942128609 %  Processing time: 0.300999999999476"
# [1] "Neural Network Size: 9  MAPE: 5.80940920145524 %  Processing time: 0.406999999999243"
# [1] "Neural Network Size: 10  MAPE: 5.80940958182454 %  Processing time: 0.459000000002561"
# [1] "Neural Network Size: 11  MAPE: 2.78328495013876 %  Processing time: 77.5960000000014"
# [1] "Neural Network Size: 12  MAPE: 2.64657743843872 %  Processing time: 92.8389999999999"
# [1] "Neural Network Size: 13  MAPE: 5.80940894896993 %  Processing time: 0.452000000001135"
# [1] "Neural Network Size: 14  MAPE: 5.80940937071846 %  Processing time: 0.53099999999904"
# [1] "Neural Network Size: 15  MAPE: 3.69696102668818 %  Processing time: 57.2940000000017"
# [1] "Neural Network Size: 16  MAPE: 5.80940927038854 %  Processing time: 0.957999999998719"
# [1] "Neural Network Size: 17  MAPE: 5.80940932028037 %  Processing time: 0.800999999999476"
# [1] "Neural Network Size: 18  MAPE: 3.76497575271071 %  Processing time: 43.7520000000004"
# [1] "Neural Network Size: 19  MAPE: 3.88480723388203 %  Processing time: 26.7949999999983"
# [1] "Neural Network Size: 20  MAPE: 3.90354528476462 %  Processing time: 73.1490000000013"
# [1] "Neural Network Size: 21  MAPE: 3.26927556910249 %  Processing time: 108.741000000002"
# [1] "Neural Network Size: 22  MAPE: 3.81061579800351 %  Processing time: 127.25"
# [1] "Neural Network Size: 23  MAPE: 5.80940959763223 %  Processing time: 1.68100000000049"
# [1] "Neural Network Size: 24  MAPE: 5.80941068268551 %  Processing time: 1.53800000000047"
# [1] "Neural Network Size: 25  MAPE: 3.70003004665646 %  Processing time: 61.7830000000031"
# [1] "Neural Network Size: 26  MAPE: 5.80940948290016 %  Processing time: 1.8169999999991"
# [1] "Neural Network Size: 27  MAPE: 5.81000260976267 %  Processing time: 2.60199999999895"
# [1] "Neural Network Size: 28  MAPE: 5.80941023174244 %  Processing time: 1.88300000000163"
# [1] "Neural Network Size: 29  MAPE: 5.80945372803461 %  Processing time: 2.40899999999965"
# [1] "Neural Network Size: 30  MAPE: 5.80940955577323 %  Processing time: 3.58399999999892"
# [1] "Neural Network Size: 31  MAPE: 3.35109577365825 %  Processing time: 257.245999999999"
# [1] "Neural Network Size: 32  MAPE: 5.80940923872863 %  Processing time: 1.72200000000157"
# [1] "Neural Network Size: 33  MAPE: 2.83635498771244 %  Processing time: 240.004000000001"
# [1] "Neural Network Size: 34  MAPE: 3.39374484841202 %  Processing time: 199.418999999998"
# [1] "Neural Network Size: 35  MAPE: 5.80940921307879 %  Processing time: 3.00699999999779"
# [1] "Neural Network Size: 36  MAPE: 3.82060535457688 %  Processing time: 104.159"
# [1] "Neural Network Size: 37  MAPE: 2.70136998732646 %  Processing time: 505.868000000002"
# [1] "Neural Network Size: 38  MAPE: 3.62893325126062 %  Processing time: 347.199000000001"
# [1] "Neural Network Size: 39  MAPE: 5.80940955877989 %  Processing time: 2.57900000000154"
# [1] "Neural Network Size: 40  MAPE: 2.93089739132347 %  Processing time: 1270.127"
# [1] "Neural Network Size: 41  MAPE: 2.8384416011752 %  Processing time: 183.481"
# [1] "Neural Network Size: 42  MAPE: 2.75836207259979 %  Processing time: 349.337"
# [1] "Neural Network Size: 43  MAPE: 4.11421504646649 %  Processing time: 58.0490000000027"
# [1] "Neural Network Size: 44  MAPE: 3.06805239245911 %  Processing time: 442.641"
# [1] "Neural Network Size: 45  MAPE: 3.25502564180848 %  Processing time: 657.062999999998"
# [1] "Neural Network Size: 46  MAPE: 3.65710588259361 %  Processing time: 286.934000000001"
# [1] "Neural Network Size: 47  MAPE: 3.36965635732708 %  Processing time: 371.826000000001"
# [1] "Neural Network Size: 48  MAPE: 3.76532512705134 %  Processing time: 141.926000000003"
# [1] "Neural Network Size: 49  MAPE: 2.90427626125146 %  Processing time: 135.447"
# [1] "Neural Network Size: 50  MAPE: 2.73290517827211 %  Processing time: 240.595000000001"


# print(paste("ENEM_2015_CN:"))
# for (NNSize in 2:50){
#   Time = system.time(ENEM_2015_CN_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_CN_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#   ENEM_2015_CN_NN_Predictions = predict(ENEM_2015_CN_NN, ENEM_2015_CN_test)
#   ENEM_2015_CN_MAPE = (sum(abs(ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_NN_Predictions)/ENEM_2015_CN_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CN_test$`MÉDIA ESCOLA`))*100
#   print(paste("Neural Network Size:", NNSize, " MAPE:", ENEM_2015_CN_MAPE, "%", " Processing time:", Time[3]))
# }
# [3] "Neural Network Size: 2  MAPE: 7.64865646290641 % Processing time: 0.175999999999476"
# [3] "Neural Network Size: 3  MAPE: 7.6497386343151 % Processing time: 0.731999999999971"
# [3] "Neural Network Size: 4  MAPE: 7.64865806418308 % Processing time: 0.17699999999968" 
# [3] "Neural Network Size: 5  MAPE: 7.64865700390943 % Processing time: 0.326000000000931"
# [3] "Neural Network Size: 6  MAPE: 7.64865741976263 % Processing time: 0.287000000000262"
# [3] "Neural Network Size: 7  MAPE: 7.6486577526754 % Processing time: 0.34900000000016" 
# [3] "Neural Network Size: 8  MAPE: 7.64865814396617 % Processing time: 0.310999999999694"
# [3] "Neural Network Size: 9  MAPE: 7.64865765662096 % Processing time: 0.347999999999956"
# [3] "Neural Network Size: 10  MAPE: 3.21014756214967 % Processing time: 64.5649999999987"  
# [3] "Neural Network Size: 11  MAPE: 7.64865027065454 % Processing time: 0.425999999999476"
# [3] "Neural Network Size: 12  MAPE: 7.64865751940005 % Processing time: 0.444999999999709"
# [3] "Neural Network Size: 13  MAPE: 7.64865728695308 % Processing time: 0.533000000001266"
# [3] "Neural Network Size: 14  MAPE: 7.64865775144409 % Processing time: 0.654000000000451"
# [3] "Neural Network Size: 15  MAPE: 7.64864698327216 % Processing time: 0.945999999999913"
# [3] "Neural Network Size: 16  MAPE: 7.64865570454101 % Processing time: 2.73000000000138"
# [3] "Neural Network Size: 17  MAPE: 7.64865742177681 % Processing time: 0.588999999999942"
# [3] "Neural Network Size: 18  MAPE: 7.64865770728848 % Processing time: 1.14599999999882"
# [3] "Neural Network Size: 19  MAPE: 4.16750078896493 % Processing time: 84.1589999999997"
# [3] "Neural Network Size: 20  MAPE: 7.64867321294873 % Processing time: 3.30699999999888"   
# [3] "Neural Network Size: 21  MAPE: 4.50297015059042 % Processing time: 165.548000000001"  
# [3] "Neural Network Size: 22  MAPE: 7.64865767708087 % Processing time: 1.19700000000012"   
# [3] "Neural Network Size: 23  MAPE: 4.29098862403931 % Processing time: 141.941999999999"
# [3] "Neural Network Size: 24  MAPE: 4.51567673410726 % Processing time: 280.779"           
# [3] "Neural Network Size: 25  MAPE: 4.78569583501764 % Processing time: 128.053"           
# [3] "Neural Network Size: 26  MAPE: 4.18814325669692 % Processing time: 257.636999999999"  
# [3] "Neural Network Size: 27  MAPE: 7.64865730094365 % Processing time: 1.22700000000077"
# [3] "Neural Network Size: 28  MAPE: 5.31989637466917 % Processing time: 73.0509999999995"
# [3] "Neural Network Size: 29  MAPE: 4.8342705009252 % Processing time: 112.296"            
# [3] "Neural Network Size: 30  MAPE: 4.25039861024856 % Processing time: 256.683000000001"
# [3] "Neural Network Size: 31  MAPE: 4.68692824393252 % Processing time: 166.974"           
# [3] "Neural Network Size: 32  MAPE: 3.43699633708868 % Processing time: 161.797999999999"  
# [3] "Neural Network Size: 33  MAPE: 4.13271846963799 % Processing time: 237.419"           
# [3] "Neural Network Size: 34  MAPE: 4.16982949952227 % Processing time: 246.519"           
# [3] "Neural Network Size: 35  MAPE: 3.7770311602727 % Processing time: 853.655000000001"  
# [3] "Neural Network Size: 36  MAPE: 7.07469568328007 % Processing time: 14.2259999999987"
# [3] "Neural Network Size: 37  MAPE: 4.47248449340966 % Processing time: 300.699000000001"  
# [3] "Neural Network Size: 38  MAPE: 3.26929766144739 % Processing time: 166.655999999999"  
# [3] "Neural Network Size: 39  MAPE: 4.31704347759129 % Processing time: 204.345000000001"  
# [3] "Neural Network Size: 40  MAPE: 3.37574975669659 % Processing time: 381.699000000001"  
# [3] "Neural Network Size: 41  MAPE: 4.10705199143006 % Processing time: 733.847000000002"
# [3] "Neural Network Size: 42  MAPE: 3.55222445613284 % Processing time: 82.9560000000019"  
# [3] "Neural Network Size: 43  MAPE: 3.81270799894224 % Processing time: 604.701999999997"
# [3] "Neural Network Size: 44  MAPE: 4.14857482137107 % Processing time: 294.323"         
# [3] "Neural Network Size: 45  MAPE: 3.2566182516253 % Processing time: 372.681"           
# [3] "Neural Network Size: 46  MAPE: 4.22088158551911 % Processing time: 306.499"   
# [3] "Neural Network Size: 47  MAPE: 4.05416455244298 % Processing time: 292.916999999998"
# [3] "Neural Network Size: 48  MAPE: 4.29010699662454 % Processing time: 171.532999999999"  
# [3] "Neural Network Size: 49  MAPE: 4.38317982170617 % Processing time: 271.975000000002"
# [3] "Neural Network Size: 50  MAPE: 4.24362367785284 % Processing time: 598.279999999999" 

# print(paste("ENEM_2015_LC:"))
# for (NNSize in 2:50){
#   Time = system.time(ENEM_2015_LC_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_LC_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#   ENEM_2015_LC_NN_Predictions = predict(ENEM_2015_LC_NN, ENEM_2015_LC_test)
#   ENEM_2015_LC_MAPE = (sum(abs(ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_NN_Predictions)/ENEM_2015_LC_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_LC_test$`MÉDIA ESCOLA`))*100
#   print(paste("Neural Network Size:", NNSize, " MAPE:", ENEM_2015_LC_MAPE, "%", " Processing time:", Time[3]))
# }
# [1] "Neural Network Size: 2  MAPE: 6.60078785400862 %  Processing time: 0.107000000003609"
# [1] "Neural Network Size: 3  MAPE: 6.60078989948358 %  Processing time: 0.132000000005064"
# [1] "Neural Network Size: 4  MAPE: 6.60078932658712 %  Processing time: 0.181999999993423"
# [1] "Neural Network Size: 5  MAPE: 4.62872721954224 %  Processing time: 22.0290000000023"
# [1] "Neural Network Size: 6  MAPE: 2.87229701096149 %  Processing time: 62.6280000000042"
# [1] "Neural Network Size: 7  MAPE: 6.6007893355501 %  Processing time: 0.307999999997264"
# [1] "Neural Network Size: 8  MAPE: 4.4409295258748 %  Processing time: 38.6469999999972"
# [1] "Neural Network Size: 9  MAPE: 6.60079223210592 %  Processing time: 1.36000000000058"
# [1] "Neural Network Size: 10  MAPE: 6.60078931155554 %  Processing time: 0.463000000003376"
# [1] "Neural Network Size: 11  MAPE: 6.60078801835849 %  Processing time: 0.484000000004016"
# [1] "Neural Network Size: 12  MAPE: 4.28116991450153 %  Processing time: 75.8839999999982"
# [1] "Neural Network Size: 13  MAPE: 6.60078927261805 %  Processing time: 0.51600000000326"
# [1] "Neural Network Size: 14  MAPE: 6.60079307119795 %  Processing time: 2.38200000000506"
# [1] "Neural Network Size: 15  MAPE: 6.60078917744204 %  Processing time: 0.80800000000454"
# [1] "Neural Network Size: 16  MAPE: 4.4744787033889 %  Processing time: 31.1939999999959"
# [1] "Neural Network Size: 17  MAPE: 3.02529265410136 %  Processing time: 170.347000000002"
# [1] "Neural Network Size: 18  MAPE: 3.05380566189354 %  Processing time: 400.539000000004"
# [1] "Neural Network Size: 19  MAPE: 2.8052155812612 %  Processing time: 69.2059999999983"
# [1] "Neural Network Size: 20  MAPE: 6.60069338111345 %  Processing time: 1.10800000000017"
# [1] "Neural Network Size: 21  MAPE: 6.60082862221435 %  Processing time: 3.0769999999975"
# [1] "Neural Network Size: 22  MAPE: 6.60078649254231 %  Processing time: 1.85300000000279"
# [1] "Neural Network Size: 23  MAPE: 2.87874378189036 %  Processing time: 241.754000000001"
# [1] "Neural Network Size: 24  MAPE: 4.38589271365693 %  Processing time: 65.6810000000041"
# [1] "Neural Network Size: 25  MAPE: 6.60078935291554 %  Processing time: 1.14899999999761"
# [1] "Neural Network Size: 26  MAPE: 3.1845970837084 %  Processing time: 333.618000000002"
# [1] "Neural Network Size: 27  MAPE: 4.49238515831922 %  Processing time: 128.485000000001"
# [1] "Neural Network Size: 28  MAPE: 3.00305169961879 %  Processing time: 112.042000000001"
# [1] "Neural Network Size: 29  MAPE: 6.60108386407203 %  Processing time: 2.78699999999662"
# [1] "Neural Network Size: 30  MAPE: 2.89962230146188 %  Processing time: 171.565999999999"
# [1] "Neural Network Size: 31  MAPE: 3.86363726597241 %  Processing time: 209.895000000004"
# [1] "Neural Network Size: 32  MAPE: 4.36519897738678 %  Processing time: 109.961000000003"
# [1] "Neural Network Size: 33  MAPE: 4.24867854890547 %  Processing time: 112.527999999998"
# [1] "Neural Network Size: 34  MAPE: 4.22796855627837 %  Processing time: 114.678999999996"
# [1] "Neural Network Size: 35  MAPE: 3.32500050710523 %  Processing time: 93.239999999998"
# [1] "Neural Network Size: 36  MAPE: 4.19465425760411 %  Processing time: 186.296000000002"
# [1] "Neural Network Size: 37  MAPE: 6.60078387356459 %  Processing time: 4.85100000000239"
# [1] "Neural Network Size: 38  MAPE: 3.47805135654747 %  Processing time: 531.504000000001"
# [1] "Neural Network Size: 39  MAPE: 3.3694186377105 %  Processing time: 204.460000000006"
# [1] "Neural Network Size: 40  MAPE: 3.0475527670319 %  Processing time: 543.330999999998"
# [1] "Neural Network Size: 41  MAPE: 3.47470584377416 %  Processing time: 232.597999999998"
# [1] "Neural Network Size: 42  MAPE: 3.07787654649164 %  Processing time: 276.420999999995"
# [1] "Neural Network Size: 43  MAPE: 4.23641602607891 %  Processing time: 220.369999999995"
# [1] "Neural Network Size: 44  MAPE: 2.81523783476436 %  Processing time: 326.669000000002"
# [1] "Neural Network Size: 45  MAPE: 4.24150160202319 %  Processing time: 171.953999999998"
# [1] "Neural Network Size: 46  MAPE: 3.93425637374642 %  Processing time: 294.067999999999"
# [1] "Neural Network Size: 47  MAPE: 6.60078893334792 %  Processing time: 3.3289999999979"
# [1] "Neural Network Size: 48  MAPE: 3.0739531819874 %  Processing time: 591.32"
# [1] "Neural Network Size: 49  MAPE: 3.86745394373532 %  Processing time: 364.877"
# [1] "Neural Network Size: 50  MAPE: 2.96002748947394 %  Processing time: 500.642999999996"


# print(paste("ENEM_2015_MT:"))
# for (NNSize in 2:50){
#   Time = system.time(ENEM_2015_MT_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_MT_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#   ENEM_2015_MT_NN_Predictions = predict(ENEM_2015_MT_NN, ENEM_2015_MT_test)
#   ENEM_2015_MT_MAPE = (sum(abs(ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_NN_Predictions)/ENEM_2015_MT_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
#   print(paste("Neural Network Size:", NNSize, " MAPE:", ENEM_2015_MT_MAPE, "%", " Processing time:", Time[3]))
# }
# [1] "Neural Network Size: 2  MAPE: 5.0978725541273 %  Processing time: 7.35599999999999"
# [1] "Neural Network Size: 3  MAPE: 10.6073487680751 %  Processing time: 0.13300000000001"
# [1] "Neural Network Size: 4  MAPE: 10.607348623483 %  Processing time: 0.161000000000001"
# [1] "Neural Network Size: 5  MAPE: 10.6073488158587 %  Processing time: 0.216999999999999"
# [1] "Neural Network Size: 6  MAPE: 10.6073486290359 %  Processing time: 0.262999999999991"
# [1] "Neural Network Size: 7  MAPE: 10.6073503516856 %  Processing time: 0.323000000000008"
# [1] "Neural Network Size: 8  MAPE: 10.6073484981102 %  Processing time: 0.266000000000005"
# [1] "Neural Network Size: 9  MAPE: 4.8902473558031 %  Processing time: 32.003"
# [1] "Neural Network Size: 10  MAPE: 10.6073489058779 %  Processing time: 0.408999999999992"
# [1] "Neural Network Size: 11  MAPE: 10.6073435577047 %  Processing time: 1.06400000000001"
# [1] "Neural Network Size: 12  MAPE: 10.607348928354 %  Processing time: 0.451000000000008"
# [1] "Neural Network Size: 13  MAPE: 7.44072586264879 %  Processing time: 38.525"
# [1] "Neural Network Size: 14  MAPE: 6.85026160581807 %  Processing time: 56.42"
# [1] "Neural Network Size: 15  MAPE: 10.6073444143737 %  Processing time: 0.777999999999992"
# [1] "Neural Network Size: 16  MAPE: 6.96623162835861 %  Processing time: 83.243"
# [1] "Neural Network Size: 17  MAPE: 6.57070085912593 %  Processing time: 60.883"
# [1] "Neural Network Size: 18  MAPE: 6.22487054376637 %  Processing time: 166.8"
# [1] "Neural Network Size: 19  MAPE: 4.86985100652838 %  Processing time: 65.833"
# [1] "Neural Network Size: 20  MAPE: 7.13933014140048 %  Processing time: 45.3299999999999"
# [1] "Neural Network Size: 21  MAPE: 6.2773456292389 %  Processing time: 109.652"
# [1] "Neural Network Size: 22  MAPE: 6.28787133361896 %  Processing time: 116.99"
# [1] "Neural Network Size: 23  MAPE: 6.7820559037859 %  Processing time: 134.452"
# [1] "Neural Network Size: 24  MAPE: 7.69427450912013 %  Processing time: 52.9160000000001"
# [1] "Neural Network Size: 25  MAPE: 7.50314065311492 %  Processing time: 83.8220000000001"
# [1] "Neural Network Size: 26  MAPE: 10.6073490262064 %  Processing time: 2.52700000000004"
# [1] "Neural Network Size: 27  MAPE: 4.88770880001477 %  Processing time: 115.135"
# [1] "Neural Network Size: 28  MAPE: 6.7198556525369 %  Processing time: 128.561"
# [1] "Neural Network Size: 29  MAPE: 5.8261162167852 %  Processing time: 356.362"
# [1] "Neural Network Size: 30  MAPE: 10.6073489516933 %  Processing time: 2.05199999999991"
# [1] "Neural Network Size: 31  MAPE: 6.40838419099699 %  Processing time: 327.527"
# [1] "Neural Network Size: 32  MAPE: 6.11279254812926 %  Processing time: 238.178"
# [1] "Neural Network Size: 33  MAPE: 5.23677447622873 %  Processing time: 202.969"
# [1] "Neural Network Size: 34  MAPE: 4.88770307589034 %  Processing time: 86.3090000000002"
# [1] "Neural Network Size: 35  MAPE: 6.76253051205478 %  Processing time: 118.787"
# [1] "Neural Network Size: 36  MAPE: 6.22963269998058 %  Processing time: 230.542"
# [1] "Neural Network Size: 37  MAPE: 6.08790104001812 %  Processing time: 374.088"
# [1] "Neural Network Size: 38  MAPE: 10.6073528179578 %  Processing time: 2.40999999999985"
# [1] "Neural Network Size: 39  MAPE: 7.01552880802237 %  Processing time: 120.447"
# [1] "Neural Network Size: 40  MAPE: 6.70773653892354 %  Processing time: 213.165"
# [1] "Neural Network Size: 41  MAPE: 4.87062519037681 %  Processing time: 182.63"
# [1] "Neural Network Size: 42  MAPE: 6.87411354090576 %  Processing time: 158.233"
# [1] "Neural Network Size: 43  MAPE: 5.40916693595561 %  Processing time: 959.364"
# [1] "Neural Network Size: 44  MAPE: 6.29500563268021 %  Processing time: 840.597"
# [1] "Neural Network Size: 45  MAPE: 5.6455716442857 %  Processing time: 320.632000000001"
# [1] "Neural Network Size: 46  MAPE: 5.37735117298836 %  Processing time: 405.651"
# [1] "Neural Network Size: 47  MAPE: 4.97099698507208 %  Processing time: 477.42"
# [1] "Neural Network Size: 48  MAPE: 6.18056791029782 %  Processing time: 412.085"
# [1] "Neural Network Size: 49  MAPE: 6.6313109468641 %  Processing time: 333.604"
# [1] "Neural Network Size: 50  MAPE: 4.99511288363141 %  Processing time: 310.819"


#print(paste("ENEM_2015_RED:"))
#for (NNSize in 2:50){
#  Time = system.time(ENEM_2015_RED_NN <- nnet(`MÉDIA ESCOLA`~., data=ENEM_2015_RED_train, size=NNSize, decay=0.0001, maxit=10000, trace=F, linout=T, MaxNWts=5000000))
#  ENEM_2015_RED_NN_Predictions = predict(ENEM_2015_RED_NN, ENEM_2015_RED_test)
#  ENEM_2015_RED_MAPE = (sum(abs(ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_NN_Predictions)/ENEM_2015_RED_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_RED_test$`MÉDIA ESCOLA`))*100
#  print(paste("Neural Network Size:", NNSize, " MAPE:", ENEM_2015_RED_MAPE, "%", " Processing time:", Time[3]))
#}
# [1] "Neural Network Size: 2  MAPE: 10.7743026112511 %  Processing time: 0.113999999999578"
# [1] "Neural Network Size: 3  MAPE: 10.7743037104164 %  Processing time: 0.273999999999432"
# [1] "Neural Network Size: 4  MAPE: 10.7743041066883 %  Processing time: 0.166999999999462"
# [1] "Neural Network Size: 5  MAPE: 7.34475598231309 %  Processing time: 30.1740000000009"
# [1] "Neural Network Size: 6  MAPE: 10.7743035932476 %  Processing time: 0.231999999999971"
# [1] "Neural Network Size: 7  MAPE: 10.7743038179695 %  Processing time: 0.322999999998501"
# [1] "Neural Network Size: 8  MAPE: 10.7743038126652 %  Processing time: 0.275000000001455"
# [1] "Neural Network Size: 9  MAPE: 10.774303759869 %  Processing time: 0.373000000001412"
# [1] "Neural Network Size: 10  MAPE: 7.31571070948446 %  Processing time: 54.9879999999994"
# [1] "Neural Network Size: 11  MAPE: 10.7742991159891 %  Processing time: 0.380999999999403"
# [1] "Neural Network Size: 12  MAPE: 10.7743037516257 %  Processing time: 0.442999999999302"
# [1] "Neural Network Size: 13  MAPE: 10.7743033961314 %  Processing time: 0.477999999999156"
# [1] "Neural Network Size: 14  MAPE: 10.7743036590884 %  Processing time: 0.683000000000902"
# [1] "Neural Network Size: 15  MAPE: 10.7743001775837 %  Processing time: 0.530000000000655"
# [1] "Neural Network Size: 16  MAPE: 10.7743033846343 %  Processing time: 0.880999999999403"
# [1] "Neural Network Size: 17  MAPE: 10.7743035178545 %  Processing time: 0.515999999999622"
# [1] "Neural Network Size: 18  MAPE: 10.7743039883806 %  Processing time: 0.884000000000015"
# [1] "Neural Network Size: 19  MAPE: 5.62878862475387 %  Processing time: 75.7749999999996"
# [1] "Neural Network Size: 20  MAPE: 7.45063093399406 %  Processing time: 31.5950000000012"
# [1] "Neural Network Size: 21  MAPE: 7.05704095131029 %  Processing time: 253.603999999999"
# [1] "Neural Network Size: 22  MAPE: 10.7743037609346 %  Processing time: 1.29600000000028"
# [1] "Neural Network Size: 23  MAPE: 6.58964735508153 %  Processing time: 179.713"
# [1] "Neural Network Size: 24  MAPE: 10.7362597904444 %  Processing time: 4.63799999999901"
# [1] "Neural Network Size: 25  MAPE: 5.58486288857236 %  Processing time: 130.905000000001"
# [1] "Neural Network Size: 26  MAPE: 7.65224560696916 %  Processing time: 11.9660000000003"
# [1] "Neural Network Size: 27  MAPE: 10.7743036182538 %  Processing time: 1.14199999999983"
# [1] "Neural Network Size: 28  MAPE: 7.26767621335454 %  Processing time: 96.6940000000013"
# [1] "Neural Network Size: 29  MAPE: 7.14928027620239 %  Processing time: 165.630000000001"
# [1] "Neural Network Size: 30  MAPE: 6.60449089345722 %  Processing time: 298.174000000001"
# [1] "Neural Network Size: 31  MAPE: 6.83123297172522 %  Processing time: 262.918"
# [1] "Neural Network Size: 32  MAPE: 5.4511448227253 %  Processing time: 249.744000000001"
# [1] "Neural Network Size: 33  MAPE: 6.77343530401362 %  Processing time: 235.367999999999"
# [1] "Neural Network Size: 34  MAPE: 6.72565269550553 %  Processing time: 449.445"
# [1] "Neural Network Size: 35  MAPE: 6.34624205083545 %  Processing time: 452.232"
# [1] "Neural Network Size: 36  MAPE: 7.25976797375191 %  Processing time: 208.270999999999"
# [1] "Neural Network Size: 37  MAPE: 7.34988680647835 %  Processing time: 182.487999999999"
# [1] "Neural Network Size: 38  MAPE: 7.16173807796035 %  Processing time: 258.278999999999"
# [1] "Neural Network Size: 39  MAPE: 5.63742891197825 %  Processing time: 251.764999999999"
# [1] "Neural Network Size: 40  MAPE: 7.17135769174074 %  Processing time: 274.386"
# [1] "Neural Network Size: 41  MAPE: 6.81091407981095 %  Processing time: 328.498"
# [1] "Neural Network Size: 42  MAPE: 6.62174210481114 %  Processing time: 432.969000000001"
# [1] "Neural Network Size: 43  MAPE: 6.4091431090043 %  Processing time: 626.907999999999"
# [1] "Neural Network Size: 44  MAPE: 6.70640021768652 %  Processing time: 196.538999999999"
# [1] "Neural Network Size: 45  MAPE: 6.58044564699867 %  Processing time: 348.666000000001"
# [1] "Neural Network Size: 46  MAPE: 6.62056085897305 %  Processing time: 370.712000000001"
# [1] "Neural Network Size: 47  MAPE: 6.34172540894281 %  Processing time: 602.978000000001"
# [1] "Neural Network Size: 48  MAPE: 7.26700571561536 %  Processing time: 233.719999999999"
# [1] "Neural Network Size: 49  MAPE: 6.67716479629731 %  Processing time: 367.888999999999"
# [1] "Neural Network Size: 50  MAPE: 6.76185608427237 %  Processing time: 247.343000000001"


# Fim dos testes com Redes Neurais
# =======================================================================================================================


# Início dos testes com Regressão Linear Simples
# =======================================================================================================================

system.time(ENEM_2015_CH_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CH_train))
system.time(ENEM_2015_CN_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CN_train))
system.time(ENEM_2015_LC_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_LC_train))
system.time(ENEM_2015_MT_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_MT_train))
system.time(ENEM_2015_RED_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_RED_train))

# > system.time(ENEM_2015_CH_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CH_train))
# usuário   sistema decorrido 
# 0.043     0.000     0.044 
# > system.time(ENEM_2015_CN_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CN_train))
# usuário   sistema decorrido 
# 0.043     0.000     0.043 
# > system.time(ENEM_2015_LC_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_LC_train))
# usuário   sistema decorrido 
# 0.047     0.000     0.044 
# > system.time(ENEM_2015_MT_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_MT_train))
# usuário   sistema decorrido 
# 0.043     0.000     0.044 
# > system.time(ENEM_2015_RED_LM <- lm(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_RED_train))
# usuário   sistema decorrido 
# 0.043     0.000     0.043 

ENEM_2015_CH_LM_Predictions = as.numeric(predict(ENEM_2015_CH_LM, ENEM_2015_CH_test))
ENEM_2015_CN_LM_Predictions = as.numeric(predict(ENEM_2015_CN_LM, ENEM_2015_CN_test))
ENEM_2015_LC_LM_Predictions = as.numeric(predict(ENEM_2015_LC_LM, ENEM_2015_LC_test))
ENEM_2015_MT_LM_Predictions = as.numeric(predict(ENEM_2015_MT_LM, ENEM_2015_MT_test))
ENEM_2015_RED_LM_Predictions = as.numeric(predict(ENEM_2015_RED_LM, ENEM_2015_RED_test))

ENEM_2015_CH_LM_RMSE = sqrt(mean((ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_LM_Predictions)^2))
ENEM_2015_CN_LM_RMSE = sqrt(mean((ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_LM_Predictions)^2))
ENEM_2015_LC_LM_RMSE = sqrt(mean((ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_LM_Predictions)^2))
ENEM_2015_MT_LM_RMSE = sqrt(mean((ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_LM_Predictions)^2))
ENEM_2015_RED_LM_RMSE = sqrt(mean((ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_LM_Predictions)^2))

ENEM_2015_CH_LM_RMSE
ENEM_2015_CN_LM_RMSE
ENEM_2015_LC_LM_RMSE
ENEM_2015_MT_LM_RMSE
ENEM_2015_RED_LM_RMSE

# > ENEM_2015_CH_LM_RMSE
# [1] 18.50859
# > ENEM_2015_CN_LM_RMSE
# [1] 21.85835
# > ENEM_2015_LC_LM_RMSE
# [1] 17.18262
# > ENEM_2015_MT_LM_RMSE
# [1] 34.92945
# > ENEM_2015_RED_LM_RMSE
# [1] 38.77026


ENEM_2015_CH_LM_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_LM_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
ENEM_2015_CN_LM_MAPE = (sum(abs(ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_LM_Predictions)/ENEM_2015_CN_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CN_test$`MÉDIA ESCOLA`))*100
ENEM_2015_LC_LM_MAPE = (sum(abs(ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_LM_Predictions)/ENEM_2015_LC_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_LC_test$`MÉDIA ESCOLA`))*100
ENEM_2015_MT_LM_MAPE = (sum(abs(ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_LM_Predictions)/ENEM_2015_MT_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_RED_LM_MAPE = (sum(abs(ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_LM_Predictions)/ENEM_2015_RED_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_RED_test$`MÉDIA ESCOLA`))*100

ENEM_2015_CH_LM_MAPE
ENEM_2015_CN_LM_MAPE
ENEM_2015_LC_LM_MAPE
ENEM_2015_MT_LM_MAPE
ENEM_2015_RED_LM_MAPE

# > ENEM_2015_CH_LM_MAPE
# [1] 2.464265
# > ENEM_2015_CN_LM_MAPE
# [1] 3.084115
# > ENEM_2015_LC_LM_MAPE
# [1] 2.556633
# > ENEM_2015_MT_LM_MAPE
# [1] 4.834013
# > ENEM_2015_RED_LM_MAPE
# [1] 5.120099

plot(ENEM_2015_CH_LM)
plot(ENEM_2015_CN_LM)
plot(ENEM_2015_LC_LM)
plot(ENEM_2015_MT_LM)
plot(ENEM_2015_RED_LM)

# Fim dos testes com Regressão Linear Simples
# =======================================================================================================================


# Início dos testes com Árvore de Regressão
# =======================================================================================================================

controle = ctree_control(mincriterion = 0.95)

system.time(ENEM_2015_CH_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CH_train, controls = controle))
system.time(ENEM_2015_CN_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CN_train, controls = controle))
system.time(ENEM_2015_LC_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_LC_train, controls = controle))
system.time(ENEM_2015_MT_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_MT_train, controls = controle))
system.time(ENEM_2015_RED_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_RED_train, controls = controle))

# > system.time(ENEM_2015_CH_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CH_train, controls = controle))
# usuário   sistema decorrido 
# 0.383     0.000     0.389 
# > system.time(ENEM_2015_CN_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CN_train, controls = controle))
# usuário   sistema decorrido 
# 0.420     0.000     0.421 
# > system.time(ENEM_2015_LC_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_LC_train, controls = controle))
# usuário   sistema decorrido 
# 0.327     0.000     0.328 
# > system.time(ENEM_2015_MT_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_MT_train, controls = controle))
# usuário   sistema decorrido 
# 0.277     0.000     0.277 
# > system.time(ENEM_2015_RED_AR <- ctree(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_RED_train, controls = controle))
# usuário   sistema decorrido 
# 0.27      0.00      0.27 

ENEM_2015_CH_AR_Predictions = as.numeric(predict(ENEM_2015_CH_AR, ENEM_2015_CH_test))
ENEM_2015_CN_AR_Predictions = as.numeric(predict(ENEM_2015_CN_AR, ENEM_2015_CN_test))
ENEM_2015_LC_AR_Predictions = as.numeric(predict(ENEM_2015_LC_AR, ENEM_2015_LC_test))
ENEM_2015_MT_AR_Predictions = as.numeric(predict(ENEM_2015_MT_AR, ENEM_2015_MT_test))
ENEM_2015_RED_AR_Predictions = as.numeric(predict(ENEM_2015_RED_AR, ENEM_2015_RED_test))

ENEM_2015_CH_AR_RMSE = sqrt(mean((ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_AR_Predictions)^2))
ENEM_2015_CN_AR_RMSE = sqrt(mean((ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_AR_Predictions)^2))
ENEM_2015_LC_AR_RMSE = sqrt(mean((ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_AR_Predictions)^2))
ENEM_2015_MT_AR_RMSE = sqrt(mean((ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_AR_Predictions)^2))
ENEM_2015_RED_AR_RMSE = sqrt(mean((ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_AR_Predictions)^2))

ENEM_2015_CH_AR_RMSE
ENEM_2015_CN_AR_RMSE
ENEM_2015_LC_AR_RMSE
ENEM_2015_MT_AR_RMSE
ENEM_2015_RED_AR_RMSE

# > ENEM_2015_CH_AR_RMSE
# [1] 18.06778
# > ENEM_2015_CN_AR_RMSE
# [1] 21.9101
# > ENEM_2015_LC_AR_RMSE
# [1] 17.6387
# > ENEM_2015_MT_AR_RMSE
# [1] 35.11719
# > ENEM_2015_RED_AR_RMSE
# [1] 38.09756

ENEM_2015_CH_AR_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_AR_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
ENEM_2015_CN_AR_MAPE = (sum(abs(ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_AR_Predictions)/ENEM_2015_CN_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CN_test$`MÉDIA ESCOLA`))*100
ENEM_2015_LC_AR_MAPE = (sum(abs(ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_AR_Predictions)/ENEM_2015_LC_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_LC_test$`MÉDIA ESCOLA`))*100
ENEM_2015_MT_AR_MAPE = (sum(abs(ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_AR_Predictions)/ENEM_2015_MT_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_RED_AR_MAPE = (sum(abs(ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_AR_Predictions)/ENEM_2015_RED_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_RED_test$`MÉDIA ESCOLA`))*100

ENEM_2015_CH_AR_MAPE
ENEM_2015_CN_AR_MAPE
ENEM_2015_LC_AR_MAPE
ENEM_2015_MT_AR_MAPE
ENEM_2015_RED_AR_MAPE

# > ENEM_2015_CH_AR_MAPE
# [1] 2.392405
# > ENEM_2015_CN_AR_MAPE
# [1] 2.966295
# > ENEM_2015_LC_AR_MAPE
# [1] 2.587762
# > ENEM_2015_MT_AR_MAPE
# [1] 4.508804
# > ENEM_2015_RED_AR_MAPE
# [1] 4.950901

#plot(ENEM_2015_CH_AR)
#plot(ENEM_2015_CH_AR, terminal_panel = node_terminal)
#plot(ENEM_2015_CH_AR, terminal_panel = node_density)

#plot(ENEM_2015_CN_AR)
#plot(ENEM_2015_CN_AR, terminal_panel = node_terminal)
#plot(ENEM_2015_CN_AR, terminal_panel = node_density)

#plot(ENEM_2015_LC_AR)
#plot(ENEM_2015_LC_AR, terminal_panel =node_terminal)
#plot(ENEM_2015_LC_AR, terminal_panel = node_density)

#plot(ENEM_2015_MT_AR)
#plot(ENEM_2015_MT_AR, terminal_panel = node_terminal)
#plot(ENEM_2015_MT_AR, terminal_panel = node_density)

#plot(ENEM_2015_RED_AR)
#plot(ENEM_2015_RED_AR, terminal_panel = node_terminal)
#plot(ENEM_2015_RED_AR, terminal_panel = node_density)

# http://stackoverflow.com/questions/13751962/how-to-plot-a-large-ctree-to-avoid-overlapping-nodes
plot(ENEM_2015_CH_AR, type="simple", inner_panel=node_inner(ENEM_2015_CH_AR, abbreviate = TRUE, pval = FALSE, id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ENEM_2015_CH_AR, abbreviate = TRUE, digits = 1, fill = c("white"), id = FALSE))
plot(ENEM_2015_CN_AR, type="simple", inner_panel=node_inner(ENEM_2015_CN_AR, abbreviate = TRUE, pval = FALSE, id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ENEM_2015_CN_AR, abbreviate = TRUE, digits = 1, fill = c("white"), id = FALSE))
plot(ENEM_2015_LC_AR, type="simple", inner_panel=node_inner(ENEM_2015_LC_AR, abbreviate = TRUE, pval = FALSE, id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ENEM_2015_LC_AR, abbreviate = TRUE, digits = 1, fill = c("white"), id = FALSE))
plot(ENEM_2015_MT_AR, type="simple", inner_panel=node_inner(ENEM_2015_MT_AR, abbreviate = TRUE, pval = FALSE, id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ENEM_2015_MT_AR, abbreviate = TRUE, digits = 1, fill = c("white"), id = FALSE))
plot(ENEM_2015_RED_AR, type="simple", inner_panel=node_inner(ENEM_2015_RED_AR, abbreviate = TRUE, pval = FALSE, id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ENEM_2015_RED_AR, abbreviate = TRUE, digits = 1, fill = c("white"), id = FALSE))

# Fim dos testes com Árvore de Regressão
# =======================================================================================================================


# Início dos testes com K Nearest Neighbors
# =======================================================================================================================
#
# Ainda não completo (com erros)

kvalue = 33 # kvalue define o nmero de vizinhos

library(mice)

system.time(ENEM_2015_CH_KNN <- knn.reg(train = ENEM_2015_CH_train, y = ENEM_2015_CH_train[, 20], test = ENEM_2015_CH_test, k = kvalue))
system.time(ENEM_2015_CN_KNN <- knn.reg(train = ENEM_2015_CN_train, y = ENEM_2015_CN_train[, 20], test = ENEM_2015_CN_test, k = kvalue))
system.time(ENEM_2015_LC_KNN <- knn.reg(train = ENEM_2015_LC_train, y = ENEM_2015_LC_train[, 20], test = ENEM_2015_LC_test, k = kvalue))
system.time(ENEM_2015_MT_KNN <- knn.reg(train = ENEM_2015_MT_train, y = ENEM_2015_MT_train[, 20], test = ENEM_2015_MT_test, k = kvalue))
system.time(ENEM_2015_RED_KNN <- knn.reg(train = ENEM_2015_RED_train, y = ENEM_2015_RED_train[, 20], test = ENEM_2015_RED_test, k = kvalue))


# Fim dos testes com K Nearest Neighbors
# =======================================================================================================================


# Início dos testes com Random Forest
# =======================================================================================================================
#
# Ainda não completo (com erros)

ENEM_2015_CH_RF <- randomForest(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CH_train, ntree = 500, check.names=TRUE)
ENEM_2015_CN_RF <- randomForest(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_CN_train, ntree = 500)
ENEM_2015_LC_RF <- randomForest(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_LC_train, ntree = 500)
ENEM_2015_MT_RF <- randomForest(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_MT_train, ntree = 500)
ENEM_2015_RED_RF <- randomForest(`MÉDIA ESCOLA` ~ ., data = ENEM_2015_RED_train, ntree = 500)

ENEM_2015_CH_RF_Predictions = as.numeric(predict(ENEM_2015_CH_RF, ENEM_2015_CH_test))
ENEM_2015_CN_RF_Predictions = as.numeric(predict(ENEM_2015_CN_RF, ENEM_2015_CN_test))
ENEM_2015_LC_RF_Predictions = as.numeric(predict(ENEM_2015_LC_RF, ENEM_2015_LC_test))
ENEM_2015_MT_RF_Predictions = as.numeric(predict(ENEM_2015_MT_RF, ENEM_2015_MT_test))
ENEM_2015_RED_RF_Predictions = as.numeric(predict(ENEM_2015_RED_RF, ENEM_2015_RED_test))

ENEM_2015_CH_RF_RMSE = sqrt(mean((ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_RF_Predictions)^2))
ENEM_2015_CN_RF_RMSE = sqrt(mean((ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_RF_Predictions)^2))
ENEM_2015_LC_RF_RMSE = sqrt(mean((ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_RF_Predictions)^2))
ENEM_2015_MT_RF_RMSE = sqrt(mean((ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_RF_Predictions)^2))
ENEM_2015_RED_RF_RMSE = sqrt(mean((ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_RF_Predictions)^2))

ENEM_2015_CH_RF_RMSE
ENEM_2015_CN_RF_RMSE
ENEM_2015_LC_RF_RMSE
ENEM_2015_MT_RF_RMSE
ENEM_2015_RED_RF_RMSE

ENEM_2015_CH_RF_MAPE = (sum(abs(ENEM_2015_CH_test$`MÉDIA ESCOLA` - ENEM_2015_CH_RF_Predictions)/ENEM_2015_CH_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CH_test$`MÉDIA ESCOLA`))*100
ENEM_2015_CN_RF_MAPE = (sum(abs(ENEM_2015_CN_test$`MÉDIA ESCOLA` - ENEM_2015_CN_RF_Predictions)/ENEM_2015_CN_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_CN_test$`MÉDIA ESCOLA`))*100
ENEM_2015_LC_RF_MAPE = (sum(abs(ENEM_2015_LC_test$`MÉDIA ESCOLA` - ENEM_2015_LC_RF_Predictions)/ENEM_2015_LC_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_LC_test$`MÉDIA ESCOLA`))*100
ENEM_2015_MT_RF_MAPE = (sum(abs(ENEM_2015_MT_test$`MÉDIA ESCOLA` - ENEM_2015_MT_RF_Predictions)/ENEM_2015_MT_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_MT_test$`MÉDIA ESCOLA`))*100
ENEM_2015_RED_RF_MAPE = (sum(abs(ENEM_2015_RED_test$`MÉDIA ESCOLA` - ENEM_2015_RED_RF_Predictions)/ENEM_2015_RED_test$`MÉDIA ESCOLA`, na.rm = TRUE)/length(ENEM_2015_RED_test$`MÉDIA ESCOLA`))*100

ENEM_2015_CH_RF_MAPE
ENEM_2015_CN_RF_MAPE
ENEM_2015_LC_RF_MAPE
ENEM_2015_MT_RF_MAPE
ENEM_2015_RED_RF_MAPE

# Fim dos testes com Random Forest
# =======================================================================================================================