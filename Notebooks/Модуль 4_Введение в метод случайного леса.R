# устанавливаем пакет randomForest
# install.packages("randomForest")

# загружаем пакет randomForest
library(randomForest)

# загружаем данные
data <- read.csv2("C:/Trees/RF_classification.csv")

# смотрим данные
data

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес из 10 деревьев классификации
model<-randomForest(default ~., data, ntree=10, importance=TRUE, norm.votes=FALSE)

# выводим информацию об ошибке классификации
# по методу OOB
print(model)

# выводим классы, спрогнозированные по методу OOB
oob_predictions <- predict(model, type="response")
oob_results <-data.frame(data, result=oob_predictions)
oob_results

# смотрим частоты голосов деревьев для каждого класса
# (деревья построены по out-of-bag выборкам)
oob_vote_freq <- predict(model, type="vote", norm.votes=FALSE)
oob_vote_freq

# смотрим процентные доли голосов деревьев для каждого класса
# (деревья построены по out-of-bag выборкам)
oob_vote_fract <- predict(model, type="vote", norm.votes=TRUE)
oob_vote_fract

# выводим вероятности классов, 
# спрогнозированные по методу OOB
oob_probabilities <- predict(model, type="prob")
oob_probabilities

# выводим классы, спрогнозированные по обычному методу
predictions <- predict(model, data, type="response")
results <-data.frame(data, result=predictions)
results

# смотрим частоты голосов деревьев для каждого класса
# (деревья построены по всем бутстреп-выборкам)
vote_freq <- predict(model, data, type="vote", norm.votes=FALSE)
vote_freq

# смотрим процентные доли голосов деревьев для каждого класса
# (деревья построены по всем бутстреп-выборкам)
vote_fract <- predict(model, data, type="vote", norm.votes=TRUE)
vote_fract

# смотрим вероятности классов, 
# спрогнозированные по методу OOB
probabilities <- predict(model, data, type="prob")
probabilities

# смотрим частоты голосов деревьев по 
# каждому классу и голос каждого дерева
ind_predictions <- predict(model, data, type="vote", 
                           norm.votes=FALSE, predict.all=TRUE)
ind_predictions

# загружаем данные
data <- read.csv2("C:/Trees/RF_regression.csv")

# смотрим данные
data

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес из 10 деревьев регрессии
model<-randomForest(days_of_delinquency ~., data, ntree=10, importance=TRUE)

# выводим информацию о среднеквадратичной ошибке
# и R-квадрате по методу OOB
print(model)

# выводим значения, спрогнозированные по методу OOB
oob_predictions <- predict(model, type="response")
oob_results <-data.frame(data, result=oob_predictions)
oob_results

# вычисляем среднеквадратичную ошибку по методу OOB, для этого сумму 
# квадратов разностей между фактическими и спрогнозированными значениями
# зависимой переменной делим на количество наблюдений, при этом
# каждое спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по OOB выборкам 
oob_MSE <- sum((data$days_of_delinquency - oob_predictions)^2)/nrow(data)
oob_MSE


# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной от ее среднего значения
TSS <- sum((data$days_of_delinquency-(mean(data$days_of_delinquency)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной от спрогнозированных, при этом каждое 
# спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по OOB выборкам 
RSS <- sum((data$days_of_delinquency-oob_predictions)^2)
# вычисляем R-квадрат по методу OOB
oob_R2 <- (1-(RSS/TSS))*100
oob_R2


# выводим значения, спрогнозированные по обычному методу
predictions <- predict(model, data, type="response")
results <-data.frame(data, result=predictions)
results

# вычисляем среднеквадратичную ошибку по обычному методу, для этого сумму 
# квадратов разностей между фактическими и спрогнозированными значениями
# зависимой переменной делим на количество наблюдений, при этом
# каждое спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по всем бутстреп-выборкам 
MSE <- sum((data$days_of_delinquency - predictions)^2)/nrow(data)

# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной от ее среднего значения
TSS <- sum((data$days_of_delinquency-(mean(data$days_of_delinquency)))^2)
# вычисляем сумму квадратов отклонений фактических значений 
# зависимой переменной от спрогнозированных, при этом каждое 
# спрогнозированное значение – результат усреднения средних
# значений, вычисленных деревьями по всем бутстреп-выборкам 
RSS <- sum((data$days_of_delinquency-predictions)^2)
# вычисляем R-квадрат по обычному методу
R2 <- (1-(RSS/TSS))*100

# печатаем результаты
output <- c(MSE, R2)
names(output) <- c("MSE", "R2")
output  


