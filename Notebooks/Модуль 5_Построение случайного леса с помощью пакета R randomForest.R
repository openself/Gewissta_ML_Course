# Модуль 5.Построение случайного леса с помощью пакета R randomForest

# Лекция 5.1 Построение ансамбля деревьев классификации

# 5.1.1 Подготовка данных

# загружаем данные
data <- read.csv2("C:/Trees/Response.csv")

# выполняем необходимые преобразования
data[, -c(12:13)] <- lapply(data[, -c(12:13)], factor)
set.seed(42)
data$random_number <- runif(nrow(data),0,1)
development <- data[which(data$random_number > 0.3), ]
holdout <- data[ which(data$random_number <= 0.3), ]
development$random_number <- NULL
holdout$random_number <- NULL


# 5.1.2 Построение модели и получение OOB оценки качества

# загружаем пакет randomForest
library(randomForest)

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес деревьев классификации
model<-randomForest(response ~., development, importance=TRUE)

# выводим информацию о качестве модели
print(model)

# матрицу ошибок по методу OOB 
# можно еще вывести так
table(development$response, predict(model))

# строим график зависимости ошибок классификации по методу OOB
# от количества случайно отбираемых предикторов
plot(model)

# настраиваем оптимальное значение mtry
set.seed(152)
tuneRF(development[,1:13], development[,14], ntreeTry=500, trace=FALSE)


# 5.1.3 Получение информации о деревьях случайного леса

# выведем информацию о последних 15 узлах
# дерева №1 случайного леса, не отображая
# метки переменных расщепления и метки
# спрогнозированных классов
info_tree1 <- getTree(model, k=1, labelVar=F)
tail(info_tree1, 15)

# выведем информацию о последних 15 узлах
# дерева №1 случайного леса, отобразив
# метки переменных расщепления и метки
# спрогнозированных классов
info_tree1 <- getTree(model, k=1, labelVar=T)
tail(info_tree1, 15)

# 5.1.4 Важности предикторов

# выводим важности предикторов
importance(model)

# выводим график важности предикторов
varImpPlot(model)

# вычисляем частоты использования переменных 
# в качестве предикторов разбиения
freq <- varUsed(model, by.tree=FALSE, count=TRUE)
# извлекаем названия предикторов
names <- colnames(development[,-14])
# сопоставляем названия предикторов
# c частотами
names(freq) <- names
# выводим результаты сопоставления
freq


# 5.1.5 Графики частной зависимости

# строим график частной зависимости для переменной age,
# интересующий класс – класс 1 (класс Есть отклик)
# значение по оси ординат - разность между логарифмом 
# доли голосов, поданных деревьями за интересующий класс 
# зависимой переменной, и усредненной суммой логарифмов 
# голосов, поданных деревьями за каждый класс
partialPlot(model, development, age, 1)

# строим график частной зависимости для переменной cus_leng,
# интересующий класс – класс 1 (класс Есть отклик)
partialPlot(model, development, cus_leng, 1)

# строим график частной зависимости для переменной atm_user,
# интересующий класс – класс 1 (класс Есть отклик)
partialPlot(model, development, atm_user, 1)

# строим график частной зависимости для переменной atm_user,
# интересующий класс – класс 0 (класс Нет отклика)
partialPlot(model, development, atm_user, 0)

# 5.1.6 Вычисление вероятностей классов

# вычисляем вероятности классов для обучающей выборки
# обычным методом
prob_dev <- predict(model, development, type="prob")
# вычисляем вероятности классов для обучающей выборки
# по методу OOB
prob_dev_oob <- predict(model, type="prob")

# выводим вероятности для последних 5 наблюдений
# обучающей выборки, вычисленные по обычному методу
tail(prob_dev, 5)

# выводим вероятности для последних 5 наблюдений
# обучающей выборки, вычисленные по методу OOB
tail(prob_dev_oob, 5)

# 5.1.7 Оценка дискриминирующей способности модели с помощью ROC-кривой

# загружаем пакет pROC для построения ROC-кривых
library(pROC)
# строим ROC-кривую для обучающей выборки (на основе 
# вероятностей, вычисленных обычным способом)
roc_dev<-plot(roc(development$response, prob_dev[,2], ci=TRUE), percent=TRUE, 
              print.auc=TRUE, col="#1c61b6")
# вычисляем вероятности классов для контрольной выборки
prob_hold <- predict(model, holdout, type="prob")
# добавляем ROC-кривую для контрольной выборки
roc_hold<-plot(roc(holdout$response, prob_hold[,2], ci=TRUE), percent=TRUE, 
               print.auc=TRUE, col="#008600", print.auc.y= .4, add=TRUE)
# создаем легенды к ROC-кривым
legend("bottomright", legend=c("Обучающая выборка (обычный метод)", 
                               "Контрольная выборка"), 
       col=c("#1c61b6", "#008600"), lwd=2)


# строим ROC-кривую для обучающей выборки (на основе 
# вероятностей, вычисленных по способу OOB)
roc_dev<-plot(roc(development$response, prob_dev_oob[,2], ci=TRUE), percent=TRUE, 
              print.auc=TRUE, col="#1c61b6")
# добавляем ROC-кривую для контрольной выборки
roc_hold<-plot(roc(holdout$response, prob_hold[,2], ci=TRUE), percent=TRUE, 
               print.auc=TRUE, col="#008600", print.auc.y= .4, add=TRUE)
# cоздаем легенды к ROC-кривым
legend("bottomright", legend=c("Обучающая выборка (метод OOB)", 
                               "Контрольная выборка"), 
       col=c("#1c61b6", "#008600"), lwd=2)


# загружаем пакет rpart
library(rpart)
# подгоняем модель CART
set.seed(42)
model_cart <-rpart(response ~., development)
# записываем вероятности, спрогнозированные деревом CART
# для контрольной выборки, в объект prob_hold_cart 
prob_hold_cart <- predict(model_cart, holdout, type="prob")
# визуализируем обе ROC-кривые
rf<-plot(roc(holdout$response, prob_hold[,2], ci=TRUE), 
         percent=TRUE, print.auc=TRUE, col="#1c61b6")
cart<-plot(roc(holdout$response, prob_hold_cart[,2], ci=TRUE), percent=TRUE, 
           print.auc=TRUE, col="#008600", print.auc.y= .4, add=TRUE)
# создаем легенды к ROC-кривым
legend("bottomright", legend=c("Случайный лес", "Дерево CRT"), 
       col=c("#1c61b6", "#008600"), lwd=2)


# 5.1.8 Получение спрогнозированных классов зависимой переменной

# задаем стартовое значение генератора
# случайных чисел
set.seed(152)

# вычисляем классы зависимой переменной 
# для обучающей выборки обычным способом
resp_dev <- predict(model, development, type="response")

# выводим классы зависимой переменной
# для последних 5 наблюдений обучающей 
# выборки, вычисленные по обычному методу
tail(resp_dev, 5)

# выводим матрицу ошибок для обучающей выборки
# на основе классов, вычисленных обычным методом
table(development$response, resp_dev)

# задаем стартовое значение генератора
# случайных чисел
set.seed(152)
# вычисляем классы зависимой переменной 
# для контрольной выборки
resp_hold <- predict(model, holdout, type="response")
# выводим матрицу ошибок для контрольной выборки
table(holdout$response, resp_hold)


# 5.1.9 График зазора прогнозов

plot(margin(model))

# Лекция 5.2 Построение ансамбля деревьев регрессии

# 5.2.1 Подготовка данных

# загружаем данные
data <- read.csv2("C:/Trees/Creddebt.csv")

# выполняем необходимые преобразования
data$ed <- ordered(data$ed, levels = c("Неполное среднее", "Среднее", "Среднее специальное",   
                                       "Незаконченное высшее", "Высшее, ученая степень"))
set.seed(100)
ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
development <- data[ind==1,]
holdout <- data[ind==2,]

# 5.2.2 Построение модели и получение OOB оценки качества

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес деревьев регрессии
model<-randomForest(creddebt ~., development, importance=TRUE)

# выводим информацию о качестве модели
print(model)

# строим график зависимости среднеквадратичной ошибки по методу OOB
# от количества деревьев в ансамбле
plot(model)


# 5.2.3 Важности предикторов

importance(model)
varImpPlot(model)

# 5.2.4 Графики частной зависимости

# строим график частной зависимости для переменной income
partialPlot(model, development, income)

# строим график частной зависимости для переменной debtinc
partialPlot(model, development, debtinc)

# 5.2.5 Работа с прогнозами и вычисление среднеквадратической ошибки

# прогнозируем значения зависимой переменной 
# для обучающей выборки обычным способом
predvalue_dev <- predict(model, development)

# выводим значения зависимой переменной
# для последних 5 наблюдений обучающей 
# выборки, вычисленные по обычному методу
tail(predvalue_dev, 5)

# вычисляем среднеквадратичную ошибку для обучающей выборки по обычному методу, 
# для этого сумму квадратов разностей между фактическими и спрогнозированными 
# значениями зависимой переменной делим на количество наблюдений, при этом
# каждое спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по всем бутстреп-выборкам 
MSE_dev <- sum((development$creddebt - predvalue_dev)^2)/nrow(development)

# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной в обучающей выборке от ее среднего значения
TSS <- sum((development$creddebt-(mean(development$creddebt)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной в обучающей выборке от спрогнозированных, 
# при этом каждое спрогнозированное значение – результат усреднения 
# средних значений, вычисленных деревьями по всем бутстреп-выборкам 
RSS <- sum((development$creddebt-predvalue_dev)^2)
# вычисляем R-квадрат для обучающей выборки по обычному методу
R2_dev <- (1-(RSS/TSS))*100

# печатаем результаты
output <- c(MSE_dev, R2_dev)
names(output) <- c("MSE", "R2")
output

# прогнозируем значения зависимой переменной 
# для обучающей выборки по методу OOB
oob_predvalue_dev <- predict(model)

# вычисляем среднеквадратичную ошибку для обучающей выборки по обычному методу, 
# для этого сумму квадратов разностей между фактическими и спрогнозированными 
# значениями зависимой переменной делим на количество наблюдений, при этом
# каждое спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по OOB выборкам 
oob_MSE_dev <- sum((development$creddebt-oob_predvalue_dev)^2)/nrow(development)

# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной в обучающей выборке от ее среднего значения
TSS <- sum((development$creddebt-(mean(development$creddebt)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной в обучающей выборке от спрогнозированных, 
# при этом каждое спрогнозированное значение – результат усреднения 
# средних значений, вычисленных деревьями по OOB выборкам 
RSS <- sum((development$creddebt-oob_predvalue_dev)^2)
# вычисляем R-квадрат для обучающей выборки по методу OOB
oob_R2_dev <- (1-(RSS/TSS))*100

# печатаем результаты
output <- c(oob_MSE_dev, oob_R2_dev)
names(output) <- c("MSE", "R2")
output

# прогнозируем значения зависимой переменной 
# для контрольной выборки
predvalue_hold <- predict(model, holdout)

# выводим значения зависимой переменной
# для последних 5 наблюдений контрольной 
# выборки
tail(predvalue_hold, 5)

# вычисляем среднеквадратичную ошибку для контрольной выборки 
MSE_hold <- sum((holdout$creddebt-predvalue_hold)^2)/nrow(holdout)
# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной в контрольной выборке от ее среднего значения
TSS <- sum((holdout$creddebt-(mean(holdout$creddebt)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной в контрольной выборке от спрогнозированных
RSS <- sum((holdout$creddebt-predvalue_hold)^2)
# вычисляем R-квадрат для контрольной выборки
R2_hold <- (1-(RSS/TSS))*100
# печатаем результаты
output <- c(MSE_hold, R2_hold)
names(output) <- c("MSE", "R2")
output

# 5.2.6 Улучшение качества прогнозов

# настраиваем оптимальное значение mtry
set.seed(152)
tuneRF(development[,1:6], development[,7], ntreeTry=500, trace=FALSE)

# строим модель c новым значением mtry
set.seed(152)
model2<-randomForest(creddebt ~., development, mtry=6)

print(model2)

# прогнозируем значения зависимой переменной 
# для контрольной выборки
predval_hold <- predict(model2, holdout)
# вычисляем среднеквадратичную ошибку для контрольной выборки 
MSE_hold <- sum((holdout$creddebt-predval_hold)^2)/nrow(holdout)
# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной в контрольной выборке от ее среднего значения
TSS <- sum((holdout$creddebt-(mean(holdout$creddebt)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной в контрольной выборке от спрогнозированных
RSS <- sum((holdout$creddebt-predval_hold)^2)
# вычисляем R-квадрат для контрольной выборки
R2_hold <- (1-(RSS/TSS))*100
# печатаем результаты
output <- c(MSE_hold, R2_hold)
names(output) <- c("MSE", "R2")
output

# 5.2.7 Получение более развернутого вывода о качестве модели

# создаем датафреймы
Xtrain <-development[,1:6]
ytrain <-development[,7]
Xtest <-holdout[,1:6]
ytest <-holdout[,7]

# строим модель, теперь мы получим
# более развернутые результаты
set.seed(152)
model2 <- randomForest(Xtrain, ytrain, Xtest, ytest, mtry=6)

# выводим информацию о качестве модели
print(model2)

# 5.3 Поиск оптимальных параметров случайного леса с помощью пакета caret

# 5.3.1 Схема оптимизации параметров, реализованная в пакете caret

# 5.3.2 Настройка условий оптимизации

# 5.3.3 Поиск оптимальных параметров для задачи классификации

# устанавливаем developer-версию пакета caret
# devtools::install_github("topepo/caret/pkg/caret")

# загружаем данные
data <- read.csv2("C:/Trees/Response.csv")

# выполняем необходимые преобразования
data[, -c(12:13)] <- lapply(data[, -c(12:13)], factor)

set.seed(42)
data$random_number <- runif(nrow(data),0,1)
training <- data[which(data$random_number > 0.3), ]
test <- data[ which(data$random_number <= 0.3), ]
data$random_number <- NULL
training$random_number <- NULL
test$random_number <- NULL

# загружаем пакет caret
library(caret)

# не забудьте загрузить пакет randomForest,
# если он ранее не был загружен
# library(randomForest)

# распараллеливаем вычисления
library(parallel)
library(doParallel)
# будем использовать 3 ядра процессора
cluster <- makeCluster(3)
registerDoParallel(cluster)

# задаем набор условий оптимизации: применяем 5-блочную 
# перекрестную проверку и перебор параметров
# по заданной сетке
control <- trainControl(method="cv", number=5, 
                        search="grid", allowParallel = TRUE)

# задаем сетку параметров для решетчатого поиска
tunegrid <- expand.grid(.mtry=c(1:7))

# строим модели случайного леса и выбираем 
# оптимальную с т.з. правильности 
set.seed(152)
rf_gridsearch <- train(response ~ ., data=training, method="rf", 
                       ntree=600, tuneGrid=tunegrid, 
                       trControl=control)

# выводим результаты решетчатого поиска 
print(rf_gridsearch)

# визуализируем результаты решетчатого поиска 
plot(rf_gridsearch)

# вычисляем прогнозы для тестовой выборки
predval <- predict(rf_gridsearch, test)
# выводим матрицу ошибок
table(test$response, predval)

# присваиваем символьные метки значениям
# зависимой переменной
training$response<-factor(training$response, levels=c(0, 1),
                          labels=c("NoResponse","Response"),exclude=NULL)
test$response<-factor(test$response, levels=c(0, 1),
                      labels=c("NoResponse","Response"),exclude=NULL)

# задаем обновленный набор условий для оптимизации
control <- trainControl(method="cv", number=5, search="grid",
                         allowParallel=TRUE,
                         classProbs=TRUE, 
                         summaryFunction=twoClassSummary)

# строим модели случайного леса и выбираем 
# оптимальную с т.з. AUC
set.seed(152)
rf_gridsearch2 <- train(response ~ ., data=training, method="rf", 
                        metric="ROC", ntree=600,
                        tuneGrid=tunegrid, trControl=control)

# выводим результаты решетчатого поиска 
print(rf_gridsearch2)

# визуализируем результаты решетчатого поиска 
plot(rf_gridsearch2)

# вычисляем AUC оптимальной модели
# на тестовой выборке
prob <- predict(rf_gridsearch2, test, type="prob")
roc(test$response, prob[,2], ci=TRUE)

# строим ROC-кривую оптимальной модели
# на тестовой выборке
plot(roc(test$response, prob[,2], ci=TRUE))

# отключаем кластер параллельных вычислений
stopCluster(cluster)
# переводим среду R в обычный режим
registerDoSEQ()


# пишем собственную реализацию решетчатого 
# поиска для случайного леса 
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "nodesize"), 
                                  class = rep("numeric", 2), label = c("mtry", "nodesize"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry=param$mtry, nodesize=param$nodesize, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method="cv", number=5, search="grid",
                        classProbs=TRUE, summaryFunction=twoClassSummary)
tunegrid <- expand.grid(.mtry=c(3:7), .nodesize=c(40, 50, 60))
set.seed(152)
custom <- train(response ~ ., ntree=600, data=training, method=customRF, metric="ROC",            
                tuneGrid=tunegrid, trControl=control)

# выводим результаты решетчатого поиска
print(custom)

# визуализируем результаты решетчатого поиска 
plot(custom)

# вычисляем AUC оптимальной модели
# на тестовой выборке
score <- predict(custom, test, type="prob")
roc(test$response, score[,2], ci=TRUE)

# 5.3.4 Поиск оптимальных параметров для задачи регрессии

# загружаем данные
data <- read.csv2("C:/Trees/Creddebt.csv")

# выполняем необходимые преобразования
data$ed <- ordered(data$ed, levels = c("Неполное среднее", "Среднее", "Среднее специальное",   
                                       "Незаконченное высшее", "Высшее, ученая степень"))
set.seed(100)
ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
tr <- data[ind==1,]
tst <- data[ind==2,]


# пишем собственную реализацию решетчатого 
# поиска для случайного леса 
customRF2 <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF2$parameters <- data.frame(parameter = c("mtry", "nodesize"), 
                                   class = rep("numeric", 2), label = c("mtry", "nodesize"))
customRF2$grid <- function(x, y, len = NULL, search = "grid") {}
customRF2$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry=param$mtry, nodesize=param$nodesize, ...)
}
customRF2$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF2$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF2$sort <- function(x) x[order(x[,1]),]
customRF2$levels <- function(x) x$classes

control <- trainControl(method="cv", number=5, search="grid", allowParallel=TRUE)
tunegrid <- expand.grid(.mtry=c(1:6), .nodesize=c(1, 2, 3, 4))
set.seed(152)
custom2 <- train(creddebt ~ ., ntree=600, data=tr, method=customRF2,            
                 tuneGrid=tunegrid, trControl=control)

# выводим результаты решетчатого поиска 
print(custom2)

# визуализируем результаты решетчатого поиска 
plot(custom2)

# прогнозируем значения зависимой переменной 
# для тестовой выборки с помощью
# оптимальной модели
predictions <- predict(custom2, tst)
# вычисляем корень из среднеквадратичной ошибки для тестовой выборки 
RMSE <- sqrt(sum((tst$creddebt-predictions)^2)/nrow(tst))
# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной в тестовой выборке от ее среднего значения
TSS <- sum((tst$creddebt-(mean(tst$creddebt)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной в тестовой выборке от спрогнозированных
RSS <- sum((tst$creddebt-predictions)^2)
# вычисляем R-квадрат для тестовой выборки
R2 <- (1-(RSS/TSS))*100
# вычисляем среднюю абсолютную ошибку для тестовой выборки 
MAE <- sum(abs(tst$creddebt-predictions))/nrow(tst)
# печатаем результаты
output <- c(RMSE, R2, MAE)
names(output) <- c("RMSE", "R2", "MAE")
output


# Лекция 5.4. Улучшение интерпретабельности случайного леса с помощью пакета randomForestExplainer

# устанавливаем пакет randomForestExplainer
# devtools::install_github("MI2DataLab/randomForestExplainer")

# загружаем пакет randomForestExplainer
library(randomForestExplainer)

# загружаем данные
data <- read.csv2("C:/Trees/Response.csv")

# выполняем необходимые преобразования
data[, -c(12:13)] <- lapply(data[, -c(12:13)], factor)
set.seed(42)
data$random_number <- runif(nrow(data),0,1)
development <- data[which(data$random_number > 0.3), ]
holdout <- data[ which(data$random_number <= 0.3), ]
development$random_number <- NULL
holdout$random_number <- NULL

# задаем стартовое значение генератора
# случайных чисел
set.seed(152)
# строим случайный лес деревьев классификации
forest<-randomForest(response ~., development, localImp=TRUE)

# мы передаем нашу модель случайного леса функции
# min_depth_distribution, чтобы получить информацию
# о значениях минимальной глубины для каждого 
# предиктора по каждому дереву
min_depth_frame <- min_depth_distribution(forest)
# выводим информацию о значении минимальной глубины
# для всех предикторов по первому дереву
subset(min_depth_frame, min_depth_frame$tree == 1)

# выводим график распределения минимальной глубины
plot_min_depth_distribution(min_depth_frame)

# выводим график распределения минимальной глубины,
# ограничившись 5 наиболее важными предикторами
plot_min_depth_distribution(min_depth_frame, 
                            mean_sample = "relevant_trees", 
                            k = 5)

# вычислим альтернативные метрики важности
importance_frame <- measure_importance(forest)
importance_frame

# постороим многомерный график для
# оценки важности предикторов
plot_multi_way_importance(importance_frame)

# строим вручную настроенный многомерный
# график для оценки важности предикторов
plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease",
                          y_measure = "gini_decrease",
                          size_measure = "times_a_root", 
                          no_of_labels = 5)

# строим парные графики для оценки корреляций 
# между метриками важности
plot_importance_ggpairs(importance_frame)

# строим парные графики для оценки корреляций 
# между метриками важности с добавлением 
# сглаживания LOESS
plot_importance_rankings(importance_frame)

# извлекаем имена 5 наиболее
# важных предикторов
vars <- important_variables(importance_frame, 
                            k = 5, 
                            measures = c("mean_min_depth",
                                         "no_of_trees"))

# строим таблицу взаимодействий
interactions_frame <- min_depth_interactions(forest, vars)

# выведем первые 6 строк таблицы
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

# визуализируем полученные данные
plot_min_depth_interactions(interactions_frame)



