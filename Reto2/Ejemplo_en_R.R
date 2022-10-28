library(tidyverse)
library(caret)
library(MLmetrics)
library(fastDummies)

df = read_csv('crime_data.csv')

Y = factor(df$TARGET)
X = df %>% select(-TARGET)

evaluate <- function(X, Y){
  # NO ALTERAR ESTA FUNCION
  set.seed(2022)
  n_folds = 3
  folds <- sample(cut(seq(1,nrow(X)),breaks=n_folds,labels=FALSE))
  scores <- double(length = n_folds)
  for(i in 1:n_folds){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    
    X_train <- X[-testIndexes, ]
    Y_train <- Y[-testIndexes]
    X_test <- X[testIndexes, ]
    Y_test <- Y[testIndexes]
    
    model <- model_fit(X_train, Y_train)
    predictions = model_predict(X_test, model)
    cm = as.matrix(table(Actual = Y_test, Predicted = predictions))
    
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    
    precision = diag / colsums 
    recall = diag / rowsums 
    f1 = 2 * precision * recall / (precision + recall) 
    f1 = replace_na(f1, 0)
    
    scores[i] <- mean(f1)
  }
  return(mean(scores))
}

model_fit <- function(X,Y){
  # Programa tu modelo aqui
  modelo_de_ejemplo <- caret::train(x = X, y = Y, method = 'rpart', maxdepth=5)
  return(modelo_de_ejemplo)
}

model_predict <- function(X, model){
  # retorna un vector de predicciones aqui
  return(predict(model, X))
}

evaluate(X,Y)
