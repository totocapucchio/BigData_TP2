library(usethis)
library(tidyverse)
library(lattice)
library(ggplot2)
library(caret)
library(nnet)
library(NeuralNetTools)

bank_test <- read.delim("bank_test.txt")
bank_train <- read.delim("bank_train.txt")
View(bank_test)
View(bank_train)

set.seed(2023)

#Separamos en datos para entrenar el modelo y en datos para validarlo
filas <- createDataPartition(bank_train$y,p = 0.80 )[[1]]
train <- slice(bank_train, filas)
validate <- slice(bank_train, -filas)

"Arboles de decision"
set.seed(2013)

ajuste <- train(
  y ~ ., 
  data = training, 
  method = "rf", 
  ntree = 10,
  tuneGrid = expand.grid(mtry = 1:10)
)

ajuste

varImp(ajuste)

predict(ajuste, newdata = bank_test)

bank_test$predicciones <- predict(ajuste, newdata = bank_test)

tabla_frecuencias <- table(Predicciones = bank_test$predicciones)

print(tabla_frecuencias)

