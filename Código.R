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


"Deep Learning"

set.seed(2023)

#Separamos en datos para entrenar el modelo y en datos para validarlo
filas <- createDataPartition(bank_train$y,p = 0.80 )[[1]]
train <- slice(bank_train, filas)
validate <- slice(bank_train, -filas)

grilla <- expand.grid(size = c(3, 5, 8), decay = c(0.1, 1.5, 5))

dl <- train(y ~ ., 
            data = train,
            method = "nnet",
            tuneGrid = grilla,
            trControl = trainControl(method = "cv", number = 5), 
            maxit = 90,
            trace = FALSE)

dl

confusionMatrix(
  as.factor(validate$y), 
  predict(dl, newdata = validate)
)

predicciones_test_dl <- predict(dl, newdata = bank_test)

bank_test$predicciones <- prediccionesdl

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

# Ajusta un modelo KNN utilizando train() de caret
vecinos <- train (y ~ ., data = bank_train, method = "knn",
                  preProcess = c("center", "scale"))
vecinos

# Entrenar
mod_knn <- train(
  y ~ .,
  data = banco_train,
  method = "knn",
  tuneGrid = data.frame(k = c(5, 10, 15)), # ACÁ PONER LOS VALORES QUE USTEDES QUIEREN PROBAR
  preProcess = c("center", "scale"),       # ESTANDARIZAR LAS VARIABLES
  trControl = trainControl(method = "cv" , number = 10)  # PEDIR QUE SE USE CV CON 10 FOLDS PARA VALIDAR
)

# resultados
plot(mod_knn)
mod_knn$bestTune
mod_knn$finalModel
mod_knn$results

# train error
pred_train_knn <- predict(mod_knn, banco_train)
confusionMatrix(pred_train_knn, factor(banco_train$y), positive = "yes")

# error de test (ya que veo que se dejaron por separado una parte de los datos train y no la usaron)
pred_test_knn <- predict(mod_knn, banco_test)
confusionMatrix(pred_test_knn, factor(banco_test$y), positive = "yes")



