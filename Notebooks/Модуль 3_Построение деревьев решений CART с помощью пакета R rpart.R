# Модуль 3 Построение деревьев решений CART с помощью пакета R rpart

# Лекция 3.1 Знакомство с методом CART
# 3.1.1 Описание алгоритма
# 3.1.2 Неоднородность
# 3.1.3 Метод отсечения ветвей на основе меры стоимости-сложности с перекрестной проверкой
# 3.1.4 Обработка пропущенных значений
# 3.1.5 Иллюстрация работы метода CART на конкретных примерах
# 3.1.5.1 Дерево классификации
# 3.1.5.2 Особенности реализации дерева классификации CART в пакете R rpart

# загружаем данные
data <- read.csv2("C:/Trees/CART_classification.csv")

# устанавливаем пакет rpart
# install.packages("rpart")

# загружаем пакет rpart
library(rpart)

# строим модель дерева классификации CART
set.seed(42)
model<-rpart(default~., method='class', data, 
             control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.01))

# выводим краткую информацию
# о модели классификации CART
model

# 3.1.5.3 Дерево регрессии
# 3.1.5.4 Особенности реализации дерева регрессии CART в пакете R rpart

# загружаем данные
data <- read.csv2("C:/Trees/CART_regression.csv")
str(data)

# строим модель дерева регрессии CART
set.seed(42)
model<-rpart(days_of_delinquency~., method='anova', data, 
             control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.01))

# выводим краткую информацию
# о модели регрессии CART
model

# Лекция 3.2 Построение и интерпретация дерева классификации CART
# 3.2.1 Подготовка данных

# загружаем данные
data <- read.csv2("C:/Trees/Response.csv")

# смотрим типы переменных
str(data)

# преобразовываем переменные в факторы, за исключением 
# переменных с индексами 12 и 13
data[, -c(12:13)] <- lapply(data[, -c(12:13)], factor)

# смотрим типы переменных
str(data)

# задаем стартовое значение генератора случайных чисел, чтобы
# каждый раз получать одно и то же разбиение
# на обучающую и контрольную выборки 
set.seed(100)

# разбиваем набор на обучающую и контрольную выборки
ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train <- data[ind==1,]
valid <- data[ind==2,]

# проверяем наличие пропусков в обучающей выборке
sapply(train, function(x) sum(is.na(x)))

# проверяем наличие пропусков в контрольной выборке
sapply(valid, function(x) sum(is.na(x)))


# 3.2.2 Построение и интерпретация модели классификации

set.seed(42)
# строим модель классификации, уменьшив пороговое 
# значение штрафа за сложность cp
model<-rpart(response~., method='class', control=rpart.control(cp=0.001), 
             data=train)


# выводим информацию о качестве модели
printcp(model)

# выводим подробную информацию о модели
summary(model)

# выводим краткую информацию о модели
print(model)

# устанавливаем пакет rattle
# install.packages("rattle")

# загружаем пакет rattle
library(rattle)

# выводим непосредственно
# правила разбиения
asRules(model)

# вычисляем важности переменных
model$variable.importance

# 3.2.3 Работа с диаграммой дерева

# устанавливаем пакет rpart.plot
# install.packages("rpart.plot")

# загружаем пакеты rpart.plot
library(rpart.plot)

# строим диаграмму дерева, branch задает 
# форму ветвей, space и yspace 
# настраивают горизонтальные и вертикальные 
# размеры боксов, отображающих узлы,
# split.cex настраивает размер 
# шрифта для определений узлов,
# nn.cex задает размер номеров узлов,
# nn.font задает тип шрифта 
# для номеров узлов, 
# split.prefix позволяет вставить 
# текст перед определением узла,
# split.suffix позволяет вставить 
# текст после определения узла
fancyRpartPlot(model,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4,
               split.prefix="is ", 
               split.suffix="?")

# строим диаграмму дерева снизу вверх,
# использовав значение yflip=TRUE
fancyRpartPlot(model,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4,
               split.prefix="is ", 
               split.suffix="?", yflip=TRUE)

# строим диаграмму дерева, поменяв определения
# узлов на противоположные с помощью
# значения xflip=TRUE
fancyRpartPlot(model,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4,
               split.prefix="is ", 
               split.suffix="?", xflip=TRUE)

# строим диаграмму дерева, меняя подписи
# yes и no на русские да и нет
fancyRpartPlot(model,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4, split.prefix="is ",
               split.suffix="?", yes.text="Да", no.text="Нет")

# строим диаграмму дерева, теперь в левую
# часть разбиения будем записывать узел,
# соответствующий отрицательному ответу
# на вопрос
fancyRpartPlot(model,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4, split.prefix="is ",
               split.suffix="?", yes.text="Да", no.text="Нет", left=FALSE)


# устанавливаем пакет partykit
# install.packages("partykit")

# загружаем пакет partykit
library(partykit)

# переводим модель дерева - объект класса 
# rpart в объект класса party
model_party <- as.party(model)

# строим дерево party
plot(model_party)

# выводим диаграмму для узла 18
model18 <- model_party[18]
plot(model18)

# выясним, какие наблюдения 
# относятся к узлу 11
node11 <- data_party(model_party, 11)

# выведем первые 5 наблюдений узла 11
head(node11, 5)  


# 3.2.4 Прунинг дерева классификации CART

# выводим информацию о качестве модели
printcp(model)

# строим график зависимости кросс-валидационной 
# ошибки от числа расщеплений и сложности модели
plotcp(model)

# строим обрезанное дерево
model2 <- prune(model, cp=0.0015)

# выводим диаграмму обрезанного дерева
fancyRpartPlot(model2,
               branch=0.5,
               space=0.001, yspace=0.001,
               split.cex=1.5,
               nn.cex=1, nn.font=4,
               split.prefix="is ", 
               split.suffix="?")

# автоматически выбираем оптимальное
# значение cp
model3<- prune(model,cp=model$cptable
               [which.min(model$cptable[,"xerror"]),"CP"])

# 3.2.5 Оценка качества модели

# оцениваем дискриминирующую способность
# обрезанного дерева на контрольной выборке
library(pROC)
prob_valid <- predict(model2, valid, type="prob")
roc_valid <- roc(valid$response, prob_valid[,2], ci=TRUE)

# выводим доверительный интервал AUC
roc_valid

# выводим ROC-кривую
plot.roc(roc_valid)

# устанавливаем пакет ROCR
# install.packages("ROCR")
# загружаем пакет ROCR
library(ROCR)

# создаем объект prediction
pred <- prediction(prob_valid[,2], valid$response)

# строим ROC-кривую
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 4)

# вычисляем AUC
AUC = performance(pred,"auc")@y.values[[1]]
AUC

# получаем спрогнозированные значения зависимой переменной
# для контрольной выборки
predvalue <- predict(model2, valid, type="class")

# строим матрицу ошибок
table(valid$response, predvalue)

# Лекция 3.3 Построение и интерпретация дерева регрессии CART
# 3.3.1 Подготовка данных

# загружаем данные
data <- read.csv2("C:/Trees/Creddebt.csv")

# смотрим типы переменных
str(data)

# порядковую переменную ed преобразовываем
# в упорядоченный фактор
data$ed <- ordered(data$ed, levels = c("Неполное среднее", 
                                       "Среднее", 
                                       "Среднее специальное",   
                                       "Незаконченное высшее", 
                                       "Высшее, ученая степень"))

# смотрим типы переменных
str(data)

# задаем стартовое значение генератора случайных чисел, чтобы
# каждый раз получать одно и то же разбиение
# на обучающую и контрольную выборки 
set.seed(100)

# разбиваем набор на обучающую и контрольную выборки
ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
tr <- data[ind==1,]
val <- data[ind==2,]

# проверяем наличие пропусков в обучающей выборке
sapply(tr, function(x) sum(is.na(x)))

# проверяем наличие пропусков в контрольной выборке
sapply(val, function(x) sum(is.na(x)))

# 3.3.2 Построение и интерпретация модели регрессии

set.seed(42)
# строим модель регрессии, уменьшив пороговое 
# значение штрафа за сложность cp
m<-rpart(creddebt~., method='anova', control=rpart.control(cp=0.001), 
         data=tr)

# выводим информацию о качестве модели
printcp(m)

# вычисляем общую сумму квадратов отклонений
TSS <- sum((tr$creddebt-(mean(tr$creddebt)))^2)
TSS

# 3.2.3 Прунинг и оценка качества дерева регрессии CART

# строим график зависимости кросс-валидационной 
# ошибки от числа расщеплений и сложности модели
plotcp(m)

# находим оптимальное значение cp
cp=m$cptable[which.min(m$cptable[,"xerror"]),"CP"]
cp

# выполняем прунинг
m2 <- prune(m, cp=0.001862835)

# строим диаграмму обрезанного
# дерева регрессии
fancyRpartPlot(m2,
               branch=0.5,
               space=0.1, yspace=0.01,
               split.cex=1,
               nn.cex=0.5, nn.font=1,
               split.prefix="is ", 
               split.suffix="?")

# вычисляем спрогнозированные значения зависимой 
# переменной на контрольной выборке
predvalue <- predict(m2, val)

# вычисляем девианс или сумму квадратов остатков
# на контрольной выборке, из фактического значения
# зависимой переменной вычитаем спрогнозированное
# значение, возводим остаток в квадрат и квадраты
# остатков суммируем
D <- sum((val$creddebt - predvalue)^2)
D

# вычисляем среднеквадратичную ошибку, сумму квадратов
# делим на количество наблюдений
MSE <- sum((val$creddebt - predvalue)^2)/nrow(val)
MSE

# вычисляем коэффициент детерминации
TSS <- sum((val$creddebt-(mean(val$creddebt)))^2)
RSS <- sum((val$creddebt-predvalue)^2)
R2 <- 1-(RSS/TSS)
R2

# строим модель линейной регрессии
linearMod <- lm(creddebt ~ ., data=tr)

# получаем спрогнозированные значения
# зависимой переменной с помощью
# модели линейной регрессии
predvalue_regr <-predict(linearMod, val) 

# вычисляем девианс для модели линейной регрессии
D_regr <- sum((val$creddebt - predvalue_regr)^2)
D_regr

# вычисляем среднеквадратичную ошибку
# для модели линейной регрессии
MSE_regr <- sum((val$creddebt - predvalue_regr)^2)/nrow(val)
MSE_regr

# вычисляем коэффициент детерминации
# для модели линейной регрессии
TSS <- sum((val$creddebt-(mean(val$creddebt)))^2)
RSS <- sum((val$creddebt-predvalue_regr)^2)
R2_regr <- 1-(RSS/TSS)
R2_regr

# загружаем пакет randomForest для 
# построения случайного леса
library(randomForest)

# строим модель случайного леса
set.seed(42)
forestMod <- randomForest(creddebt ~ ., data=tr)

# получаем спрогнозированные значения
# зависимой переменной с помощью
# модели случайного леса
predvalue_forest <- predict(forestMod, val)

# вычисляем коэффициент детерминации
# для модели случайного леса
TSS <- sum((val$creddebt-(mean(val$creddebt)))^2)
RSS <- sum((val$creddebt-predvalue_forest)^2)
R2 <- 1-(RSS/TSS)
R2






