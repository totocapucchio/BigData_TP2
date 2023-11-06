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


"Deep Learning"

set.seed(2023)

#Separamos en datos para entrenar el modelo y en datos para validarlo
filas <- createDataPartition(bank_train$y,p = 0.80 )[[1]]
train <- slice(bank_train, filas)
validate <- slice(bank_train, -filas)

grilla <- expand.grid(size = c(3, 5, 8), decay = c(3, 1.5, 5))

dl <- train(y ~ ., 
            data = train,
            method = "nnet",
            tuneGrid = grilla,
            trControl = trainControl(method = "cv", number = 5), 
            maxit = 200,
            trace = FALSE)

dl

confusionMatrix(
  as.factor(train$y), 
  predicciones_train <- predict(dl, newdata = train, type = "raw")
)

#Predecimos los valores para los datos de validación y vemos su desempeño

confusionMatrix(
  as.factor(validate$y), 
  predicciones_validate <- predict(dl, newdata = validate, type = "raw")
)

# Generamos las predicciones para test

predicciones_test <- predict(dl, newdata = bank_test)

bank_test$y <- predicciones_test

view(bank_test)

# Conservar solo la columna "y"

grupo4 <- bank_test["y"]

#Exportar los datos con solo la columna y

write.table(grupo4, "grupo4.txt", sep = "\t", row.names = FALSE)

"Arboles de decision"

set.seed(2023)

ajuste <- train(
  y ~ ., 
  data = train, 
  method = "rf", 
  ntree = 10,
  tuneGrid = expand.grid(mtry = 1:10)
)

ajuste

confusionMatrix(
  as.factor(validate$y), 
  predict(ajuste, newdata = validate)
)

predicciones_rf_validate <- predict(ajuste, newdata = validate)

validate$predicciones <- predicciones_rf_validate

"KNN"

# Vectores numéricos para guardar las medidas de error (una para cada k candidato)
train_error <- NULL
test_error <- NULL

# Valores para K: elegimos unos pocos para ejemplificar
candidatos <- c(5, 10, 25, 40, 60, 70) 

# Con una estructura iterativa repetimos el proceso para cada valor de k a probar
for (k in candidatos) {
  
  algoritmo <- train(
    y ~ .,
    data = train, 
    method = "knn",
    tuneGrid = data.frame(k), 
    preProcess = c("center", "scale"))
  
  # Predicción en train y train error
  predicciones_train <- predict(algoritmo, newdata = train)
  (train_error <- mean(train$y != predicciones_train))
  
  predicciones_test <- predict(algoritmo, newdata = validate)
  (test_error <- mean(validate$y != predicciones_test))}

(resultados <- data.frame(k = candidatos, train_error, test_error))

predicciones_knn_validate <- predict(algoritmo, newdata = validate)

validate$predicciones <- predicciones_knn_validate

confusionMatrix(as.factor(validate$y), 
                predict(algoritmo, newdata = validate))
