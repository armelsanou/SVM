# 1. Charger le jeu de donn?es dans R. Afficher les donn?es d'apprentissage.

rm(list = ls())
data <- read.table(file = "synth_train.txt", header = TRUE)

data <- read.table(file = "spiral.txt")

dim(data)
## [1] 100 3
X <- as.matrix(data[, -3])
Y <- as.factor(data$V3)
plot(data[,-3], pch = data$V3, col = data$V3)
legend("topright", legend = c("classe 1", "classe 2", "Classe3"), pch = 1:3, col = 1:3)

## 2. Charger (ou installer puis charger le package e1071) et consulter l'aide de la fonction svm.
# install.packages('e1071')
library(e1071)
help(svm)
##3. Appliquer la m?thode des SVM avec un noyau lin?aire (on utilisera la m?thode par formule, du type
##  y  ., data=..., pour pouvoir utiliser les fonctions graphiques dans la suite).

data$V3 <- as.factor(data$V3)
svm.lin <- svm(V3 ~ ., data = data, kernel = "linear")
svm.lin

##4. Calculer l'erreur d'apprentissage du pr?dicteur obtenu. Charger les donn?es test puis calculer son
## erreur test.
svm.lin.pred <- predict(object = svm.lin, newdata = data)
sum(svm.lin.pred != Y)/length(Y)

data_test <- read.table(file = "synth_test.txt", header = TRUE)
dim(data_test)

head(data_test)

X_test <- as.matrix(data_test[, -1])
Y_test <- as.factor(data_test$y)
svm.lin.test <- predict(object = svm.lin, newdata = data_test)
sum(svm.lin.test != Y_test)/length(Y_test)

## 5. En utilisant la fonction plot appliqu?e au r?sultat du SVM pr?c?dent, tracer la fronti?re de d?cision
## du SVM lin?aire.
plot(svm.lin, data = data, grid = 200)

## 6. Faire la m?me chose pour des SVM avec noyaux polynomiaux de degr?s 2, 3, 4, 5.
svm.deg2 <- svm(V3 ~ ., data = data, kernel = "polynomial", degree = 2)
svm.deg2

svm.deg2.pred <- predict(svm.deg2, data)
sum(svm.deg2.pred != Y)/length(Y)
## [1] 0.14
svm.deg2.test <- predict(svm.deg2, data_test)
sum(svm.deg2.test != Y_test)/length(Y_test)
## [1] 0.165
plot(svm.deg2, data, grid = 200)

svm.deg3 <- svm(V3 ~ ., data = data, kernel = "polynomial", degree = 3)
svm.deg3

svm.deg3.pred <- predict(svm.deg3, data)
sum(svm.deg3.pred != Y)/length(Y)
## [1] 0.05
svm.deg3.test <- predict(svm.deg3, data_test)
sum(svm.deg3.test != Y_test)/length(Y_test)
## [1] 0.06
plot(svm.deg3, data, grid = 200)

svm.deg4 <- svm(V3 ~ ., data = data, kernel = "polynomial", degree = 4)
svm.deg4

svm.deg4.pred <- predict(svm.deg4, data)
sum(svm.deg4.pred != Y)/length(Y)
## [1] 0.13
svm.deg4.test <- predict(svm.deg4, data_test)
sum(svm.deg4.test != Y_test)/length(Y_test)
## [1] 0.19
plot(svm.deg4, data, grid = 200)

svm.deg5 <- svm(V3 ~ ., data = data, kernel = "polynomial", degree = 5)
svm.deg5

svm.deg5.pred <- predict(svm.deg5, data)
sum(svm.deg5.pred != Y)/length(Y)
## [1] 0.04
svm.deg5.test <- predict(svm.deg5, data_test)
sum(svm.deg5.test != Y_test)/length(Y_test)
## [1] 0.065
plot(svm.deg5, data, grid = 200)

svm.gaus <- svm(V3 ~ ., data = data)
svm.gaus

svm.gaus.pred <- predict(svm.gaus, data)
sum(svm.gaus.pred != Y)/length(Y)
## [1] 0.04
svm.gaus.test <- predict(svm.gaus, data_test)
sum(svm.gaus.test != Y_test)/length(Y_test)

plot(svm.gaus, data = data, grid = 200)



