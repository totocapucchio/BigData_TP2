library(usethis)
library(tidyverse)
library(ggplot2)
library(caret)
library(nnet)
library(NeuralNetTools)

bank_test <- read.delim("C:/Git Projects/BigData_TP2/bank_test.txt")
bank_train <- read.delim("C:/Git Projects/BigData_TP2/bank_train.txt")
View(bank_test)
View(bank_train)

#Separamos en datos para entrenar el modelo y en datos para validarlo
filas <- createDataPartition(bank_train$y,p = 0.80 )[[1]]
train <- slice(bank_train, filas)
validate <- slice(bank_train, -filas)