# Модуль 2 Построение деревьев решений CHAID с помощью пакета R CHAID

# Лекция 2.1 Знакомство с методом CHAID
# 2.1.1 Описание алгоритма
# 2.1.2 Немного о тесте хи-квадрат
# 2.1.3 Способы объединения категорий предикторов
# 2.1.4 Поправка Бонферрони
# 2.1.5 Иллюстрация работы CHAID на конкретном примере

# Лекция 2.2 Предварительная подготовка данных перед построением  модели дерева CHAID
# 2.2.1 Загрузка данных

# задаем постоянный CRAN репозиторий
cat(".Rprofile: Setting US repositoryn")
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
rm(r)

# устанавливаем пакеты
install.packages("dplyr")
install.packages("Hmisc")
install.packages("car")
install.packages("imputeMissings")
install.packages("lsr")
install.packages("CHAID", repos="http://R-Forge.R-project.org")
install.packages("pROC")
install.packages("xlsx")
install.packages("stringr")


# считываем CSV-файл в датафрейм data
data <- read.csv2("C:/Trees/Churn.csv", sep=";")

# смотрим первые 10 наблюдений 
# датафрейма data
head(data, 10)

# 2.2.2 Фиксация пустых строковых значений как пропусков

# выводим количество пропусков
# по переменной income 
sum(is.na(data$income))

# выводим количество пропусков
# по переменной pay 
sum(is.na(data$pay))

# помечаем пустые строковые
# значения как пропуски 
is.na(data) <- data==''

# выводим первые 10 наблюдений
# датафрейма data 
head(data, 10)

# выводим количество пропусков
# по переменной pay 
sum(is.na(data$pay))

# фиксируем пустые строковые
# значения как пропуски 
data <- read.csv2("C:/Trees/Churn.csv", sep=";", na.strings="")

# 2.2.3 Изменение типов переменных

# смотрим типы переменных
str(data)

# преобразовываем переменную
# churn в вектор типа factor 
data$churn <- as.factor(data$churn)

# преобразовываем переменные longdist, local,
# agecat в вектор типа ordered factor 
data$longdist <- ordered(data$longdist, 
                         levels = c("<2", "2-8", "9-14", "15-20", "21+"))
data$local <- ordered(data$local, 
                      levels = c("<8", "8-20", "21-35", "36-55", "56+"))
data$agecat <- ordered(data$agecat, 
                       levels = c("<31", "31-45", "46-58", "59-70", "71+"))

# загружаем пакет dplyr 
library(dplyr)
# c помощью функции recode пакета dplyr
# переименовываем категории переменной marital_status
data$churn <- recode(data$churn, "0"="Остается", "1"="Уходит")

# отсоединяем пакет dplyr
detach("package:dplyr", unload=TRUE)

# смотрим типы переменных
str(data)

# 2.2.4 Обработка дублирующихся наблюдений

# смотрим дублирующиеся наблюдения
data[duplicated(data),]

# проверим, дублируется ли наблюдение 
# по заданному набору условий  
data[data$longdist == "<2" & data$local == "<8" & data$int_disc =="Нет" & 
       data$billtype =="Бюджетный" & data$pay =="CC" &  data$gender =="Женский&*" & 
       data$marital =="_Женат" & data$income == 32118.4 & data$agecat =="71+" & 
       data$churn =="Уходит", ]

# оставим только уникальные наблюдения  
data <- unique(data)
# смотрим, сколько наблюдений мы
# теперь будем использовать  
nrow(data)

# 2.2.5 Вывод подробной информации о переменных

# загружаем пакет Hmisc  
library(Hmisc)
# с помощью функции describe пакета
# Hmisc выведем подробную информацию
# о переменных
describe(data)

# создаем числовой ряд  
series <- c(18, 20, 23, 20, 23, 27, 24, 23, 29)
# вычисляем медиану  
median(series)

# вычисляем медиану
quantile(series, 0.50)

# вычисляем 0.25-квантиль
quantile(series, 0.25)

# 2.2.6 Нормализация строковых значений

# создаем вектор названий переменных  
names <- c("gender", "marital")
# с помощью функции lapply применяем
# функцию summary к переменным, названия
# которых записаны в векторе names  
lapply(data[,names], function(x) summary(x))

# с помощью функции lapply применяем
# функцию levels к переменным, названия
# которых записаны в векторе names  
lapply(data[,names], function(x) levels(x))

# преобразовываем переменную gender из вектора типа factor
# в вектор типа character (нельзя напрямую изменить строковое
# значение фактора, потому что в факторе под капотом для строковых 
# значений используются целочисленные коды, поэтому переводим в 
# вектор типа character, у которого значения – строки)
data$gender <- as.character(data$gender)

# удаляем с помощью функции gsub символы & и *,
# первый аргумент – удаляемые символы, второй
# аргумент – символы, на которые нужно заменить,
# третий аргумент – вектор типа character или строка 
data$gender <- gsub('\\&\\*', "", data$gender)

# преобразовываем переменную gender обратно из вектора
# типа character в вектор типа factor
data$gender <- as.factor(data$gender)

# загружаем пакет stringr
library(stringr)

# преобразовываем переменную marital из вектора типа factor
# в вектор типа character
data$marital <- as.character(data$marital)

# удаляем с помощью функции str_replace_all пакета stringr
# все символы, не являющиеся буквами алфавита и числами
# первый аргумент – вектор типа character или строка,
# второй аргумент – символы, которые нужно удалить,
# третий аргумент – символы, на которые нужно заменить
data$marital <- str_replace_all(data$marital, "[^[:alnum:]]", "")

# преобразовываем переменную marital обратно из вектора
# типа character в вектор типа factor
data$marital <- as.factor(data$marital) 

# снова выводим информацию об уровнях переменной
lapply(data[,names], function(x) levels(x))


# 2.2.7 Обработка редких категорий

# выводим информацию о распределении
# значений переменной pay
summary(data$pay)

# загружаем пакет car
library(car)
# с помощью функции recode пакета car
# объединяем редкую категорию CD с
# самой часто встречающейся
# категорией CC
data$pay <- recode(data$pay, "'CD'='CC'")
# снова выводим информацию о распределении
# значений переменной pay
summary(data$pay)

# отсоединяем пакет Hmisc
detach("package:Hmisc", unload=TRUE)

# 2.2.8 Однократное случайное разбиение набора данных на обучающую 
# и контрольную выборки для проверки модели

# разбиваем набор данных на случайную 
# и контрольную выборки
set.seed(42)

data$random_number <- runif(nrow(data),0,1)
development <- data[ which(data$random_number > 0.3), ]
holdout <- data[ which(data$random_number <= 0.3), ]

data$random_number <- NULL
development$random_number <- NULL
holdout$random_number <- NULL

# смотрим обучающий датафрейм
str(development)

# смотрим контрольный датафрейм
str(holdout)

# выводим информацию о распределении
# классов зависимой переменной churn
# в обучающей выборке
summary(development$churn)

# выводим информацию о распределении
# классов зависимой переменной churn
# в контрольной выборке
summary(holdout$churn)

# 2.2.9 Импутация пропусков

# выведем информацию о пропусках
# в обучающей выборке
sapply(development, function(x) sum(is.na(x)))

# выведем информацию о пропусках
# в контрольной выборке
sapply(holdout, function(x) sum(is.na(x)))

# выведем информацию о пропусках в обучающей выборке
# c помощью lapply, теперь результат будет
# возвращен в виде списка
lapply(development, function(x) sum(is.na(x)))

# выведем информацию о пропусках в контрольной выборке
# c помощью lapply, теперь результат будет
# возвращен в виде списка
lapply(holdout, function(x) sum(is.na(x)))

# вычисляем среднее значение
# переменной income
mean(development$income, na.rm = TRUE)

# выполняем импутацию пропусков с помощью среднего, обратите
# внимание, среднее было вычислено отдельно для
# каждой выборки
development$income[is.na(development$income)] <- mean(development$income, na.rm=TRUE)
holdout$income[is.na(holdout$income)] <- mean(holdout$income, na.rm=TRUE)

# выведем первые 10 наблюдений 
# датафрейма development 
head(development, 10)

# создаем индикатор пропусков, у функции ifelse первый аргумент –
# проверяемое условие (является ли наблюдение переменной billtype
# пропущенным), второй аргумент – значение, которое возвращается,
# если условие выполняется, третий аргумент – значение, которое
# возвращается, если условие не выполняется
development$billtype_ind <- ifelse(is.na(development$billtype), 1, 0)

# выводим первые 10 наблюдений новой
# переменной billtype_ind
head(development[,"billtype_ind"], 10)

# удаляем переменную billtype_ind
development$billtype_ind <- NULL

# с помощью функции recode пакета car запишем пропуски
# в отдельную категорию MISSING
development$billtype2 <- recode(development$billtype, "NA='MISSING'")
# смотрим первые 10 наблюдений переменной billtype2
head(development[,"billtype2"], 10)

# удаляем переменную billtype2
development$billtype2 <- NULL

# с помощью функции lapply преобразовываем переменные 
# longdist, local и agecat в неупорядоченные
# факторы
names <- c("longdist", "local", "agecat")
development[,names] <- lapply(development[,names], factor, ordered=FALSE)
holdout[,names] <- lapply(holdout[,names], factor, ordered=FALSE)

# загружаем пакет imputeMissings
library(imputeMissings)
# с помощью функции compute пакета
# imputeMissings вычисляем моды и
# медианы для импутации
values <- compute(development)
values

# выводим сводку о количестве пропусков в
# каждой переменной с помощью функции
# colSums пакета imputeMissings
colSums(is.na(development))

# выполняем автоматическую импутацию с помощью
# функции impute пакета imputeMissings
development <- impute(development, method = "median/mode")
holdout <- impute(holdout, method = "median/mode")

# с помощью функции ordered преобразовываем переменные 
# longdist, local и agecat обратно в упорядоченные
# факторы
development$longdist <- ordered(development$longdist, 
                                levels = c("<2", "2-8", "9-14", "15-20", "21+"))
development$local <- ordered(development$local, 
                             levels = c("<8", "8-20", "21-35", "36-55", "56+"))
development$agecat <- ordered(development$agecat, 
                              levels = c("<31", "31-45", "46-58", "59-70", "71+"))
holdout$longdist <- ordered(holdout$longdist, 
                            levels = c("<2", "2-8", "9-14", "15-20", "21+"))
holdout$local <- ordered(holdout$local, 
                         levels = c("<8", "8-20", "21-35", "36-55", "56+"))
holdout$agecat <- ordered(holdout$agecat, 
                          levels = c("<31", "31-45", "46-58", "59-70", "71+"))

# выводим информацию о пропусках в обучающей выборке
sapply(development, function(x) sum(is.na(x)))

# выводим информацию о пропусках в контрольной выборке
sapply(holdout, function(x) sum(is.na(x)))


# 2.2.10 Биннинг количественных переменных для получения порядковых предикторов

# выводим диапазон значений 
# переменной income
range(development$income)

# разбиваем переменную income на 10 интервалов
# одинаковой ширины
development$binned <-cut(x=development$income, breaks=seq(0, 100000, by=10000), 
                         include.lowest=TRUE, right=TRUE, 
                         ordered_result=TRUE, dig.lab = 6)

# смотрим распределение значений
# переменной binned
summary(development$binned)

# загружаем пакет lsr
library(lsr)
# с помощью функции quantileCut пакета lsr разбиваем
# переменную income на 10 интервалов c одинаковым
# количеством наблюдений
development$binned2 <- quantileCut(x=development$income, n=10, 
                                   include.lowest=TRUE, right=TRUE, 
                                   ordered_result=TRUE, dig.lab=6)

# смотрим распределение значений
# переменной binned2
summary(development$binned2)

# смотрим типы переменных
str(development)

# удаляем только что созданные переменные
development$binned <- NULL
development$binned2 <- NULL

# выполняем биннинг с учетом полученных правил разбиения
development$income <-cut(x=development$income, 
                         breaks=c(-Inf,11000,21000,31000,41000,
                                  50000,59000,69000,80000,
                                  90000,+Inf), 
                         include.lowest=TRUE, 
                         ordered_result=TRUE)

holdout$income <-cut(x=holdout$income, 
                     breaks=c(-Inf,11000,21000,31000,41000,
                              50000,59000,69000,80000,
                              90000,+Inf), 
                     include.lowest=TRUE, 
                     ordered_result=TRUE)


# Лекция 2.3 Построение модели дерева CHAID и ее проверка

# 2.3.1 Построение модели и работа с диаграммой дерева

# загружаем пакет CHAID
library(CHAID)

# задаем набор условий для построения дерева CHAID
params <- chaid_control(minprob = 0.01, minsplit = 1000, minbucket = 500)
# строим модель дерева CHAID
chd  <- chaid(churn ~ ., control = params, development)

# выводим диаграмму дерева
plot(chd)

# выводим диаграмму дерева
# в схематичном виде
print(chd)

# 2.3.2 Получение прогнозов модели

# выводим спрогнозированные вероятности отрицательного
# класса для первых 10 наблюдений
# контрольной выборки
prob_hold <- predict(chd, holdout, type="prob")
prob_hold[1:10]

# округляем спрогнозированные вероятности
# до второго десятичного знака
round(prob_hold[1:10], 2)

# извлекаем правила разбиения,
# полученные нашей моделью
library(partykit)
treetable <- function(party_tree) {
  
  df_list <- list()
  var_names <-  attr(party_tree$terms, "term.labels")
  var_levels <- lapply(party_tree$data, levels)
  
  walk_the_tree <- function(node, rule_branch = NULL) {
    # проходим структуру разбиений дерева (рекурсивная функция)
    # извлекаем правила для каждой ветви
    if(missing(rule_branch)) {
      rule_branch <- setNames(data.frame(t(replicate(length(var_names), NA))), var_names)
      rule_branch <- cbind(rule_branch, nodeId = NA)
      rule_branch <- cbind(rule_branch, predict = NA)
    }
    if(is.terminal(node)) {
      rule_branch[["nodeId"]] <- node$id
      rule_branch[["predict"]] <- predict_party(party_tree, node$id, type="prob") 
      
      
      df_list[[as.character(node$id)]] <<- rule_branch
    } else {
      for(i in 1:length(node)) {
        rule_branch1 <- rule_branch
        val1 <- decision_rule(node,i)
        rule_branch1[[names(val1)[1]]] <- val1
        walk_the_tree(node[i], rule_branch1)
      }
    }
  }
  
  decision_rule <- function(node, i) {
    # возвращаем правила разбиения в датафрейм вместе с названиями переменных и значениями
    var_name <- var_names[node$split$varid[[1]]]
    values_vec <- var_levels[[var_name]][ node$split$index == i]
    values_txt <- paste(values_vec, collapse = ", ")
    return( setNames(values_txt, var_name))
  }
  walk_the_tree(party_tree$node)
  res_table <- Reduce(rbind, df_list)
  return(res_table)
}

table <- treetable(chd)
table

# задаем стартовое значение генератора
# случайных чисел для воспроизводимости
set.seed(42)

# выводим спрогнозированные классы
# для первых 10 наблюдений
# контрольной выборки
predvalue_hold <- predict(chd, holdout, type="response")
predvalue_hold[1:10]

# строим таблицу классификации
table(holdout$churn, predvalue_hold)

# 2.3.3 Работа с матрицей ошибок

# снижаем пороговое значение спрогнозированной
# вероятности положительного класса до 0.3
table(holdout$churn == "Уходит", predict(chd, 
                                         holdout, type="prob")[,"Уходит"] >= 0.30)

# повышаем пороговое значение спрогнозированной
# вероятности положительного класса до 0.7
table(holdout$churn == "Уходит", predict(chd, 
                                         holdout, type="prob")[,"Уходит"] >= 0.70)

# 2.3.4 Знакомство с ROC-кривой и AUC

# создаем вектор фактических значений (классов)
# зависимой переменной
cls = c('P', 'P', 'P', 'N', 'P', 'P', 'N', 'P', 'P', 'N', 'N',
        'N', 'N', 'N', 'P', 'N', 'N', 'N', 'N', 'N')

# создаем вектор спрогнозированных вероятностей 
# положительного класса
score = c(0.92, 0.9, 0.88, 0.85, 0.82, 0.79, 0.75, 0.73, 0.72, 0.7, 0.6, 
          0.59, 0.58, 0.53, 0.52, 0.4, 0.33, 0.32, 0.24, 0.18)

# записываем положительные
# и отрицательные примеры 
pos = score[cls == 'P']
neg = score[cls == 'N']

# задаем стартовое значение генератора
# случайных чисел для воспроизводимости
set.seed(14)

# извлекаем случайным образом положительные и  
# отрицательные примеры и вычисляем долю случаев, 
# когда положительные примеры получили более 
# высокий ранг, чем отрицательные примеры
p = replicate(200000, sample(pos, size=1) > sample(neg, size=1))
mean(p)

# разделим вероятности на 2
score_divided_by_2 = c(0.92, 0.9, 0.88, 0.85, 0.82, 0.79, 0.75, 0.73, 0.72, 0.7, 0.6, 
                       0.59, 0.58, 0.53, 0.52, 0.4, 0.33, 0.32, 0.24, 0.18)/2

score_divided_by_2

# снова вычисляем AUC как долю случаев, когда
# случайно отобранный положительный объект
# будет проранжирован выше, чем случайно
# отобранный отрицательный 
pos = score_divided_by_2[cls == 'P']
neg = score_divided_by_2[cls == 'N']

set.seed(14)
p = replicate(200000, sample(pos, size=1) > sample(neg, size=1))
mean(p)

# возведем вероятности в квадрат
score_squared = c(0.92, 0.9, 0.88, 0.85, 0.82, 0.79, 0.75, 0.73, 0.72, 0.7, 0.6, 
                  0.59, 0.58, 0.53, 0.52, 0.4, 0.33, 0.32, 0.24, 0.18)^2

score_squared

# снова вычисляем AUC как долю случаев, когда
# случайно отобранный положительный объект
# будет проранжирован выше, чем случайно
# отобранный отрицательный 
pos = score_squared[cls == 'P']
neg = score_squared[cls == 'N']

set.seed(14)
p = replicate(200000, sample(pos, size=1) > sample(neg, size=1))
mean(p)

# загружаем пакет pROC
library(pROC)
# автоматически вычисляем AUC 
roc <- roc(cls, score)

# выводим информацию о значении AUC
roc

# автоматически строим ROC-кривую
plot.roc(x=roc, legacy.axes=TRUE)

# выводим AUC нашей модели дерева
# для контрольной выборки
roc_hold <- roc(holdout$churn, prob_hold[,2])
roc_hold

# строим ROC-кривую нашей модели дерева
# для контрольной выборки
plot.roc(roc_hold)

# 2.3.6 Вычисление интервальных оценок AUC для модели

# генеририруем 50 случайных чисел
# в диапазоне от 45 до 100
set.seed(42)
income <- runif(50, min=45, max=100)
income

# вычисляем среднее значение дохода
mean <- mean(income)
mean

# записываем информацию 
# о размере выборки
N <- 50

# вычисляем предел погрешности 
err <- 1.645*(sd(income)/sqrt(N))

# вычисляем нижнюю границу 90%-ного
# доверительного интервала 
mean-err

# вычисляем верхнюю границу 90%-ного
# доверительного интервала 
mean+err

# генерируем данные
set.seed(42)
income <- runif(50, min=45, max=100)
# задаем функцию, вычисляющую среднее
m <- function(x) mean(x)
# создаем вектор, в который будем 
# записывать средние значения, 
# вычисляемые по бутстреп-выборкам
boot <-numeric(5000)
# создаем 5000 бутстреп-выборок и вычисляем
# 5000 средних значений дохода
for (i in 1:5000) boot[i] <- m(sample(income, replace=T))

# вычисляем нижнюю границу 95%-ного
# доверительного интервала
quantile(boot,0.025)

# вычисляем верхнюю границу 95%-ного
# доверительного интервала
quantile(boot,0.975)

# генерируем данные
set.seed(42)
income <- runif(50, min=45, max=100)
# задаем количество бутстреп-выборок (5000)
B = 5000
# задаем размер бутстреп-выборки, 
# тот же, что и размер исходной
# (50 наблюдений)
n = 50
# создаем матрицу, у которой строки - бустреп-выборки (5000),
# а столбцы - наблюдения, отобранные в бутстреп-выборку с
# возвращением (50)
boot.samples = matrix(sample(income, size = B * n, replace = TRUE), B, n)
# применяем с помощью функции apply нашу функцию mean к каждой
# строке матрицы boot.samples (1 означает строки, 2 - столбцы)
boot.statistics = apply(boot.samples, 1, function(x) mean(x))

# вычисляем нижнюю границу 95%-ного
# доверительного интервала
quantile(boot,0.025)

# вычисляем верхнюю границу 95%-ного
# доверительного интервала
quantile(boot,0.975)

# вычисляем 95%-ный доверительный интервал AUC 
# по асимптотическому методу Делонга (по умолчанию)
# для модели дерева на контрольной выборке
roc_hold <- roc(holdout$churn, prob_hold[,2], ci=TRUE)
roc_hold

# бутстреп использует рандомизацию, поэтому 
# задаем стартовое значение генератора
# случайных чисел для воспроизводимости
set.seed(14)
# вычисляем 95%-ный доверительный интервал AUC 
# по бутстреп-методу (по умолчанию 2000 бутстреп-
# выборок) для модели дерева на контрольной выборке
ci.auc(roc_hold, method="bootstrap")


# задаем стартовое значение генератора
# случайных чисел для воспроизводимости
set.seed(14)
# вычисляем 95%-ный доверительный интервал AUC 
# по бутстреп-методу, увеличиваем количество
# бутстреп-выборок до 5000
ci.auc(roc_hold, method="bootstrap", boot.n=5000)

# вычисляем вероятности классов для обучающей
# выборки и записываем в объект prob_dev
prob_dev <- predict(chd, development, type="prob")

# строим график ROC-кривой для обучающей выборки
roc_dev <-plot.roc(development$churn, prob_dev[,2], 
                   main="Графики ROC-кривых для модели CHAID",
                   ci=TRUE, percent=TRUE,
                   print.auc=TRUE, col="red")

# добавляем в уже существующий график ROC-кривую
# для контрольной выборки
roc_hold <-plot.roc(holdout$churn, prob_hold[,2], 
                    ci=TRUE, percent=TRUE,
                    print.auc=TRUE, print.auc.x=50, print.auc.y=45, 
                    col="purple", add=TRUE)

# добавляем легенды
legend("bottomright", legend=c("Обучающая выборка", "Контрольная выборка"), 
       col=c("red", "purple"), lwd=2)

# 2.3.7 Выбор наилучшей модели из нескольких на основе доверительных интервалов AUC

# задаем новые значения параметров
# и строим новую модель
params2 <- chaid_control(minprob = 0.01, minsplit = 500, minbucket = 250)
chd2  <- chaid(churn ~ ., control = params2, development)

# сравниваем доверительный интервал AUC модели
# со значениями minsplit=1000 и minbucket=500
# и доверительный интервал AUC модели со
# значениями minsplit=500 и minbucket=250
prob_hold2 <- predict(chd2, holdout, type="prob")

roc_hold <-plot.roc(holdout$churn, prob_hold[,2], 
                   main="Сравнение ROC-кривых двух моделей CHAID на контрольной выборке",
                   ci=TRUE, percent=TRUE,
                   print.auc=TRUE, col="#1c61b6")

roc_hold2 <-plot.roc(holdout$churn, prob_hold2[,2], 
                    ci=TRUE, percent=TRUE,
                    print.auc=TRUE, print.auc.x=50, print.auc.y=45, 
                    col="#008600", add=TRUE)

legend("bottomright", legend=c("Модель CHAID: minsplit = 1000, minbucket = 500", 
                               "Модель CHAID: minsplit = 500, minbucket = 250"), 
       col=c("#1c61b6", "#008600"), lwd=2)

# задаем новые значения параметров
# и строим новую модель
params3 <- chaid_control(minprob = 0.01, minsplit = 200, minbucket = 100)
chd3  <- chaid(churn ~ ., control = params3, development)

# сравниваем доверительный интервал AUC модели
# со значениями minsplit=1000 и minbucket=500
# и доверительный интервал AUC модели со
# значениями minsplit=200 и minbucket=100
prob_hold3 <- predict(chd3, holdout, type="prob")

roc_hold <-plot.roc(holdout$churn, prob_hold[,2], 
                    main="Сравнение ROC-кривых двух моделей CHAID на контрольной выборке",
                    ci=TRUE, percent=TRUE,
                    print.auc=TRUE, col="#1c61b6")

roc_hold3 <-plot.roc(holdout$churn, prob_hold3[,2], 
                     ci=TRUE, percent=TRUE,
                     print.auc=TRUE, print.auc.x=50, print.auc.y=45, 
                     col="#008600", add=TRUE)

legend("bottomright", legend=c("Модель CHAID: minsplit = 1000, minbucket = 500", 
                               "Модель CHAID: minsplit = 200, minbucket = 100"), 
       col=c("#1c61b6", "#008600"), lwd=2)


# получаем прогнозы
predictions <- predict(chd3, holdout, type="response")

# строим матрицу ошибок
table(holdout$churn, predictions)

# вычисляем точность
Precision=427/(427+139)

# вычисляем полноту
Recall=427/(427+171)

# вычисляем F1-меру
F1_score = (2*Precision*Recall)/(Precision + Recall)
F1_score

probs <- predict(chd3, holdout, type="prob")

library(precrec)
sscurves <- evalmod(scores = probs[,2], labels = holdout$churn)
sscurves

plot(sscurves)

library(ggplot2)




# 2.3.8 Многократное случайное разбиение на обучающую 
# и контрольную выборки как способ проверки модели

# подготавливаем данные перед выполнением 100-кратного разбиения
# на обучающую и контрольную выборки, при этом импутацию пропусков
# и биннинг делаем отдельно на каждой итерации внутри цикла
data <- read.csv2("C:/Trees/Churn.csv", sep=";", na.strings="")

data$churn <- as.factor(data$churn)

data <- unique(data)

data$gender <- as.character(data$gender)
data$gender <- gsub("\\&\\*", "", data$gender)
data$gender <- as.factor(data$gender)

data$marital <- as.character(data$marital)
data$marital <- str_replace_all(data$marital, "[^[:alnum:]]", "")
data$marital <- as.factor(data$marital)

data$pay <- recode(data$pay, "'CD'='CC'")

chaid1.auc = NULL
chaid2.auc = NULL

# выполняем 100-кратное разбиение на 
# обучающую и контрольную выборки, на
# каждой из 100 итераций на обучающей
# выборке строим модель, на контрольной
# проверяем ее
set.seed(42)
for (i in 1:100) {
  data$random_number <- runif(nrow(data),0,1)
  development <- data[ which(data$random_number > 0.3), ]
  holdout     <- data[ which(data$random_number <= 0.3), ]
  data$random_number <- NULL
  development$random_number <- NULL
  holdout$random_number <- NULL
  
  # импутируем пропуски в переменной income в обучающей и контрольной
  # выборках средним, вычисленным на обучающей выборке
  development$income[is.na(development$income)] <- mean(development$income, na.rm=TRUE)
  holdout$income[is.na(holdout$income)] <- mean(development$income, na.rm=TRUE)
  
  # импутируем пропуски в остальных переменных в обучающей и контрольной
  # выборках медианами и модами, вычисленными на обучающей выборке
  values <- compute(development)
  development <- impute(development, object=values)
  holdout <- impute(holdout, object=values)
  
  development$longdist <- ordered(development$longdist, 
                                  levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  development$local <- ordered(development$local, 
                               levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  development$agecat <- ordered(development$agecat, 
                                levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  holdout$longdist <- ordered(holdout$longdist, 
                              levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  holdout$local <- ordered(holdout$local, 
                           levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  holdout$agecat <- ordered(holdout$agecat, 
                            levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  
  development$income <-cut(x=development$income, 
                           breaks=c(-Inf,11000,21000,31000,41000,
                                    50000,59000,69000,80000,
                                    90000,+Inf), 
                           include.lowest=TRUE, 
                           ordered_result=TRUE)
  
  holdout$income <-cut(x=holdout$income, 
                       breaks=c(-Inf,11000,21000,31000,41000,
                                50000,59000,69000,80000,
                                90000,+Inf), 
                       include.lowest=TRUE, 
                       ordered_result=TRUE)
  
  chaid1.model  <- chaid(churn ~ . , 
                        control = chaid_control
                        (minprob = 0.01,
                         minsplit = 1000, minbucket = 500),
                        data=development)
  chaid1.score <- predict(chaid1.model, holdout, type = "prob")
  chaid1.roc <- roc(holdout$churn, chaid1.score[,2])
  chaid1.auc[i] <- chaid1.roc$auc
  
  chaid2.model  <- chaid(churn ~ . , 
                         control = chaid_control
                         (minprob = 0.01,
                          minsplit = 200, minbucket = 100),
                         data=development)
  chaid2.score <- predict(chaid2.model, holdout, type = "prob")
  chaid2.roc <- roc(holdout$churn, chaid2.score[,2])
  chaid2.auc[i] <- chaid2.roc$auc
}

# выводим среднее значение AUC для двух моделей
# на контрольной выборке по результатам 100
# случайных разбиений «обучение-контроль»
results <- c(mean(chaid1.auc), mean(chaid2.auc))
names(results) <- c("Mean AUC minsplit=1000 и minbucket=500", 
                    "Mean AUC minsplit=200 и minbucket=100")
results

# вычисляем 95%-ный доверительный интервал 
# AUC для каждой модели
chaid1.ci = quantile(chaid1.auc, c(.025,.975))
chaid2.ci = quantile(chaid2.auc, c(.025,.975))

results_ci <- list(chaid1.ci, chaid2.ci)
names(results_ci) <- c("95% CI AUC minsplit=1000 и minbucket=500", 
                     "95% CI AUC minsplit=200 и minbucket=100")
results_ci


# вычисляем 95%-ный доверительный интервал 
# разницы средних значений AUC
n=100
diff=chaid1.auc-chaid2.auc

lower_limit=mean(diff)-1.96*(sd(diff)/sqrt(n))
upper_limit=mean(diff)+1.96*(sd(diff)/sqrt(n))

results <- list(lower_limit, upper_limit)
names(results) <- c("lower_limit", 
                    "upper_limit")
results


# 2.3.9 Проверка модели с помощью бутстрепа

# подготавливаем данные перед выполнением бутстрепа
# при этом импутацию пропусков и биннинг делаем 
# отдельно на каждой итерации внутри цикла
data <- read.csv2("C:/Trees/Churn.csv", sep=";", na.strings="")

data$churn <- as.factor(data$churn)

data <- unique(data)

data$gender <- as.character(data$gender)
data$gender <- gsub("\\&\\*", "", data$gender)
data$gender <- as.factor(data$gender)

data$marital <- as.character(data$marital)
data$marital <- str_replace_all(data$marital, "[^[:alnum:]]", "")
data$marital <- as.factor(data$marital)

data$pay <- recode(data$pay, "'CD'='CC'")

set.seed(42)
data$random_number <- runif(nrow(data),0,1)
development <- data[ which(data$random_number > 0.3), ]
holdout     <- data[ which(data$random_number <= 0.3), ]
data$random_number <- NULL
development$random_number <- NULL
holdout$random_number <- NULL

# задаем количество бутстреп-выборок
R = 100
# задаем количество элементов для
# обучающих бутстреп-выборок
k = nrow(development)
# задаем количество элементов для
# контрольных бутстреп-выборок
n = nrow(holdout)

chaid1.auc_boot = NULL
chaid2.auc_boot = NULL

# выполняем бутстреп, На каждой итерации 
# формируем на основе исходной обучающей выборки 
# бутстреп-выборку, формируем на основе исходной
# контрольной выборки бутстреп-выборку, строим модель 
# по бутстреп-выборке, сгенерированной на основе 
# исходной обучающей выборки, проверяем ее на 
# бутстреп-выборке, сгенерированной на основе 
# исходной контрольной выборки
set.seed(701)
for(i in 1:R){
  
  obs_dev.boot <- sample(x = 1:k, size = k, replace = T)
  development.boot <- development[obs_dev.boot, ]
  
  # на обучающей бутстреп-выборке вычисляем статистики для импутации, 
  # с помощью этих статистик заменяем пропуски в обучающей 
  # бутстреп-выборке
  development.boot$income[is.na(development.boot$income)] <- mean(development.boot$income, na.rm=TRUE)
  mean <- mean(development.boot$income, na.rm=TRUE)
  
  values <- compute(development.boot)
  development.boot <- impute(development.boot, object=values)
  
  development.boot$longdist <- ordered(development.boot$longdist, 
                                  levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  development.boot$local <- ordered(development.boot$local, 
                               levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  development.boot$agecat <- ordered(development.boot$agecat, 
                                levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  
  development.boot$income <-cut(x=development.boot$income, 
                           breaks=c(-Inf,11000,21000,31000,41000,
                                    50000,59000,69000,80000,
                                    90000,+Inf), 
                           include.lowest=TRUE, 
                           ordered_result=TRUE)
  
  obs_hold.boot <- sample(x = 1:n, size = n, replace = T)
  holdout.boot <- holdout[obs_hold.boot, ]
  
  # пропуски в контрольной бутстреп-выборке заменяем статистиками,
  # вычисленными на обучающей бутстреп-выборке
  holdout.boot$income[is.na(holdout.boot$income)] <- mean
  holdout.boot <- impute(holdout.boot, object=values)
  
  holdout.boot$longdist <- ordered(holdout.boot$longdist, 
                              levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  holdout.boot$local <- ordered(holdout.boot$local, 
                           levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  holdout.boot$agecat <- ordered(holdout.boot$agecat, 
                            levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  
  holdout.boot$income <-cut(x=holdout.boot$income, 
                       breaks=c(-Inf,11000,21000,31000,41000,
                                50000,59000,69000,80000,
                                90000,+Inf), 
                       include.lowest=TRUE, 
                       ordered_result=TRUE)

  chaid1 <- chaid(churn ~ . , 
                  control = chaid_control
                  (minprob = 0.01,
                    minsplit = 1000, minbucket = 500),
                  data=development.boot)
  
  chaid2 <- chaid(churn ~ . , 
                  control = chaid_control
                  (minprob = 0.01,
                    minsplit = 200, minbucket = 100),
                  data=development.boot)
    
  chaid1.score_boot <- predict(chaid1, holdout.boot, type = "prob")
  chaid1.roc_boot <- roc(holdout.boot$churn, chaid1.score_boot[,2])
  chaid1.auc_boot[i] <- chaid1.roc_boot$auc
  
  chaid2.score_boot <- predict(chaid2, holdout.boot, type = "prob")
  chaid2.roc_boot <- roc(holdout.boot$churn, chaid2.score_boot[,2])
  chaid2.auc_boot[i] <- chaid2.roc_boot$auc  
  
}

# вычисляем бутстрепированный 95%-ный доверительный интервал 
# AUC для каждой модели
chaid1.ci = quantile(chaid1.auc_boot,c(.025,.975))
chaid2.ci = quantile(chaid2.auc_boot,c(.025,.975))

results <- list(chaid1.ci, chaid2.ci)
names(results) <- c("bootstrapped 95% CI AUC minsplit=1000 и minbucket=500", 
                    "bootstrapped 95% CI AUC minsplit=200 и minbucket=100")
results


# вычисляем 95%-ный доверительный интервал 
# разницы средних значений AUC
n=100
d=chaid1.auc_boot-chaid2.auc_boot

low_limit=mean(d)-1.96*(sd(d)/sqrt(n))
up_limit=mean(d)+1.96*(sd(d)/sqrt(n))

results <- list(low_limit, up_limit)
names(results) <- c("lower_limit", 
                    "upper_limit")
results


# вычисляем 95%-ный доверительный интервал 
# разницы вычисленных значений AUC
ci = quantile(d, c(.025,.975))
ci

# выполняем тест Уилкоксона для связанных выборок
options(scipen=999)
wilcox.test(chaid1.auc_boot, chaid2.auc_boot, paired=TRUE)

# 2.3.10 Перекрестная проверка как способ проверки модели

# подготавливаем данные перед выполнением перекрестной проверки,
# при этом импутацию пропусков и биннинг делаем отдельно на
# каждой итерации внутри цикла
data <- read.csv2("C:/Trees/Churn.csv", sep=";", na.strings="")
data$churn <- as.factor(data$churn)

data <- unique(data)

data$gender <- as.character(data$gender)
data$gender <- gsub("\\&\\*", "", data$gender)
data$gender <- as.factor(data$gender)

data$marital <- as.character(data$marital)
data$marital <- str_replace_all(data$marital, "[^[:alnum:]]", "")
data$marital <- as.factor(data$marital)

data$pay <- recode(data$pay, "'CD'='CC'")

set.seed(42)
index <- sample(1:10,nrow(data),replace=TRUE)
folds <- 1:10

chaid1_cv1.auc = NULL
chaid2_cv1.auc = NULL

# выполняем перекрестную проверку
for (i in 1:10){  
  development = subset(data, index %in% folds[-i])
  holdout = subset(data, index %in% c(i))
  
  # в каждом проходе перекрестной проверки отдельно вычисляем 
  # статистики на обучающих блоках и заменяем ими пропуски в 
  # обучающих блоках, затем отдельно вычисляем статистики 
  # на контрольном блоке и заменяем ими пропуски 
  # в контрольном блоке
  development$income[is.na(development$income)] <- mean(development$income, na.rm=TRUE)
  holdout$income[is.na(holdout$income)] <- mean(holdout$income, na.rm=TRUE)
  development <- impute(development, method = "median/mode")
  holdout <- impute(holdout, method = "median/mode")
  
  development$longdist <- ordered(development$longdist, 
                                  levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  development$local <- ordered(development$local, 
                               levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  development$agecat <- ordered(development$agecat, 
                                levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  holdout$longdist <- ordered(holdout$longdist, 
                              levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  holdout$local <- ordered(holdout$local, 
                           levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  holdout$agecat <- ordered(holdout$agecat, 
                            levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  
  development$income <-cut(x=development$income, 
                           breaks=c(-Inf,11000,21000,31000,41000,
                                    50000,59000,69000,80000,
                                    90000,+Inf), 
                           include.lowest=TRUE, 
                           ordered_result=TRUE)
  
  holdout$income <-cut(x=holdout$income, 
                       breaks=c(-Inf,11000,21000,31000,41000,
                                50000,59000,69000,80000,
                                90000,+Inf), 
                       include.lowest=TRUE, 
                       ordered_result=TRUE)
  
  chaid1_cv1.model  <- chaid(churn ~ . , 
                         control = chaid_control
                         (minprob = 0.01,
                           minsplit = 1000, minbucket = 500),
                         data=development)
  chaid1_cv1.score <- predict(chaid1_cv1.model, holdout, type = "prob")
  chaid1_cv1.roc <- roc(holdout$churn, chaid1_cv1.score[,2])
  chaid1_cv1.auc[i] <- chaid1_cv1.roc$auc
  
  chaid2_cv1.model  <- chaid(churn ~ . , 
                         control = chaid_control
                         (minprob = 0.01,
                           minsplit = 200, minbucket = 100),
                         data=development)
  chaid2_cv1.score <- predict(chaid2_cv1.model, holdout, type = "prob")
  chaid2_cv1.roc <- roc(holdout$churn, chaid2_cv1.score[,2])
  chaid2_cv1.auc[i] <- chaid2_cv1.roc$auc  
}

# смотрим оценки AUC обеих моделей, усредненные по 10
# контрольным блокам перекрестной проверки
results <- c(mean(chaid1_cv1.auc), mean(chaid2_cv1.auc))
names(results) <- c("Mean AUC cv minsplit=1000 и minbucket=500", 
                    "Mean AUC cv minsplit=200 и minbucket=100")
results

chaid1_cv2.auc = NULL
chaid2_cv2.auc = NULL

# выполняем перекрестную проверку
for (i in 1:10){  
  development = subset(data, index %in% folds[-i])
  holdout = subset(data, index %in% c(i))
  
  # в каждом проходе перекрестной проверки 
  # вычисляем статистики на обучающих блоках 
  # и заменяем ими пропуски как в обучающих блоках, 
  # так и в контрольном блоке
  development$income[is.na(development$income)] <- mean(development$income, na.rm=TRUE)
  holdout$income[is.na(holdout$income)] <- mean(development$income, na.rm=TRUE)
  values <- compute(development)
  development <- impute(development, object=values)
  holdout <- impute(holdout, object=values)
  
  development$longdist <- ordered(development$longdist, 
                                  levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  development$local <- ordered(development$local, 
                               levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  development$agecat <- ordered(development$agecat, 
                                levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  holdout$longdist <- ordered(holdout$longdist, 
                              levels = c("<2", "2-8", "9-14", "15-20", "21+"))
  holdout$local <- ordered(holdout$local, 
                           levels = c("<8", "8-20", "21-35", "36-55", "56+"))
  holdout$agecat <- ordered(holdout$agecat, 
                            levels = c("<31", "31-45", "46-58", "59-70", "71+"))
  
  development$income <-cut(x=development$income, 
                           breaks=c(-Inf,11000,21000,31000,41000,
                                    50000,59000,69000,80000,
                                    90000,+Inf), 
                           include.lowest=TRUE, 
                           ordered_result=TRUE)
  
  holdout$income <-cut(x=holdout$income, 
                       breaks=c(-Inf,11000,21000,31000,41000,
                                50000,59000,69000,80000,
                                90000,+Inf), 
                       include.lowest=TRUE, 
                       ordered_result=TRUE)
  
  chaid1_cv2.model  <- chaid(churn ~ . , 
                             control = chaid_control
                             (minprob = 0.01,
                               minsplit = 1000, minbucket = 500),
                             data=development)
  chaid1_cv2.score <- predict(chaid1_cv2.model, holdout, type = "prob")
  chaid1_cv2.roc <- roc(holdout$churn, chaid1_cv2.score[,2])
  chaid1_cv2.auc[i] <- chaid1_cv2.roc$auc
  
  chaid2_cv2.model  <- chaid(churn ~ . , 
                             control = chaid_control
                             (minprob = 0.01,
                               minsplit = 200, minbucket = 100),
                             data=development)
  chaid2_cv2.score <- predict(chaid2_cv2.model, holdout, type = "prob")
  chaid2_cv2.roc <- roc(holdout$churn, chaid2_cv2.score[,2])
  chaid2_cv2.auc[i] <- chaid2_cv2.roc$auc  
}

# смотрим оценки AUC обеих моделей, усредненные по 10
# контрольным блокам перекрестной проверки
results <- c(mean(chaid1_cv2.auc), mean(chaid2_cv2.auc))
names(results) <- c("Mean AUC cv minsplit=1000 и minbucket=500", 
                    "Mean AUC cv minsplit=200 и minbucket=100")
results

# 2.3.11 Однократное случайное разбиение на обучающую, контрольную и тестовую 
# выборки как наилучший способ проверки модели

# выполняем преобразования, которые можно осуществить 
# до разбиения на обучающую, контрольную и
# тестовую выборки
data <- read.csv2("C:/Trees/Churn.csv", sep=";", na.strings="")
data$churn <- as.factor(data$churn)

data <- unique(data)

data$gender <- as.character(data$gender)
data$gender <- gsub("\\&\\*", "", data$gender)
data$gender <- as.factor(data$gender)

data$marital <- as.character(data$marital)
data$marital <- str_replace_all(data$marital, "[^[:alnum:]]", "")
data$marital <- as.factor(data$marital)

data$pay <- recode(data$pay, "'CD'='CC'")

# задаем процентные доли обучающей, 
# контрольной и тестовой выборок
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# вычисляем размеры выборок
sampleSizeTraining   <- floor(fractionTraining   * nrow(data))
sampleSizeValidation <- floor(fractionValidation * nrow(data))
sampleSizeTest       <- floor(fractionTest       * nrow(data))

# присваиваем индексы
set.seed(42)
indicesTraining    <- sort(sample(seq_len(nrow(data)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# формируеи обучающую, контрольную
# и тестовую выборки
Training   <- data[indicesTraining, ]
Validation <- data[indicesValidation, ]
Test       <- data[indicesTest, ]

# смотрим количество наблюдений в каждой выборке
results <- c(nrow(Training), nrow(Validation), nrow(Test))
names(results) <- c("Training", "Validation", "Test")
results

# взглянем на распределение классов зависимой переменной
# churn в обучающей, контрольной и тестовой выборках
myList <- list(Training, Validation, Test)
lapply(myList, function(x) summary(x[,10]))

# осуществляем преобразования, которые нужно выполнять
# после разбиения на обучающую, контрольную и тестовую
# выборки 
Training$income[is.na(Training$income)] <- mean(Training$income, na.rm=TRUE)
Validation$income[is.na(Validation$income)] <- mean(Validation$income, na.rm=TRUE)
Test$income[is.na(Test$income)] <- mean(Test$income, na.rm=TRUE)

Training <- impute(Training, method = "median/mode")
Validation <- impute(Validation, method = "median/mode")
Test <- impute(Test, method = "median/mode")


Training$longdist <- ordered(Training$longdist, 
                             levels = c("<2", "2-8", "9-14", "15-20", "21+"))
Training$local <- ordered(Training$local, 
                          levels = c("<8", "8-20", "21-35", "36-55", "56+"))
Training$agecat <- ordered(Training$agecat, 
                           levels = c("<31", "31-45", "46-58", "59-70", "71+"))
Validation$longdist <- ordered(Validation$longdist, 
                               levels = c("<2", "2-8", "9-14", "15-20", "21+"))
Validation$local <- ordered(Validation$local, 
                            levels = c("<8", "8-20", "21-35", "36-55", "56+"))
Validation$agecat <- ordered(Validation$agecat, 
                             levels = c("<31", "31-45", "46-58", "59-70", "71+"))
Test$longdist <- ordered(Test$longdist, 
                         levels = c("<2", "2-8", "9-14", "15-20", "21+"))
Test$local <- ordered(Test$local, 
                      levels = c("<8", "8-20", "21-35", "36-55", "56+"))
Test$agecat <- ordered(Test$agecat, 
                       levels = c("<31", "31-45", "46-58", "59-70", "71+"))

Training$income <-cut(x=Training$income, 
                      breaks=c(-Inf,11000,21000,31000,41000,
                               50000,59000,69000,80000,
                               90000,+Inf), 
                      include.lowest=TRUE, 
                      ordered_result=TRUE)
Validation$income <-cut(x=Validation$income, 
                        breaks=c(-Inf,11000,21000,31000,41000,
                                 50000,59000,69000,80000,
                                 90000,+Inf), 
                        include.lowest=TRUE, 
                        ordered_result=TRUE)
Test$income <-cut(x=Test$income, 
                  breaks=c(-Inf,11000,21000,31000,41000,
                           50000,59000,69000,80000,
                           90000,+Inf), 
                  include.lowest=TRUE, 
                  ordered_result=TRUE)

# строим модели с разными комбинациями значений 
# параметров на обучающей выборке
chd1  <- chaid(churn ~ . , control = chaid_control
               (minprob = 0.01,
                 minsplit = 1000, minbucket = 500),
               data=Training)

chd1_score <- predict(chd1, Validation, type = "prob")
chd1_ci.auc <- ci.auc(Validation$churn, chd1_score[,2])

chd2  <- chaid(churn ~ . , control = chaid_control
               (minprob = 0.01,
                 minsplit = 200, minbucket = 100),
               data=Training)

chd2_score <- predict(chd2, Validation, type = "prob")
chd2_ci.auc <- ci.auc(Validation$churn, chd2_score[,2])

# смотрим доверительные интервалы AUC
# моделей на контрольной выборке
results <- list(chd1_ci.auc, chd2_ci.auc)

names(results) <- c("95% CI AUC minsplit=1000 и minbucket=500", 
                    "95% CI AUC minsplit=200 и minbucket=100")
results

# проверяем качество наилучшей модели
# на тестовой выборке
chd2_test_score <- predict(chd2, Test, type = "prob")
chd2_test_ci.auc <- ci.auc(Test$churn, chd2_test_score[,2])
chd2_test_ci.auc

# объединяем обучающую, контрольную
# и тестовые выборки
TrainValTest <- rbind(Training, Validation, Test)

# строим модель с наилучшей комбинацией 
# параметров на объединенных данных
chd_best  <- chaid(churn ~ . , control = chaid_control
                   (minprob = 0.01,
                     minsplit = 200, minbucket = 100),
                   data= TrainValTest)

# 2.3.12 Применение модели к новым данным

# загружаем новые данные
newdata <- read.csv2("C:/Trees/Churn_new.csv", sep=";", na.strings="")

# смотрим типы переменных
str(newdata)

# проверяем наличие новых категорий, которые
# отсутствуют в обучающей выборке
lapply(newdata[sapply(newdata, is.factor)], 
       function(x) summary(x))

# проверяем наличие пропусков
sapply(newdata, function(x) sum(is.na(x)))

# импутируем пропуски
newdata$income[is.na(newdata$income)] <- mean(newdata$income, na.rm=TRUE)
newdata <- impute(newdata, method = "median/mode")

# преобразовываем переменные longdist, local, 
# agecat в упорядоченные факторы
newdata$longdist <- ordered(newdata$longdist, 
                            levels = c("<2", "2-8", "9-14", "15-20", "21+"))
newdata$local <- ordered(newdata$local, 
                         levels = c("<8", "8-20", "21-35", "36-55", "56+"))
newdata$agecat <- ordered(newdata$agecat, 
                          levels = c("<31", "31-45", "46-58", "59-70", "71+"))

# выполняем биннинг переменной income
newdata$income <-cut(x=newdata$income, 
                     breaks=c(-Inf,11500,21000,32000,42000,
                              50000,59000,69000,81000,
                              91000,+Inf), 
                     include.lowest=TRUE, 
                     ordered_result=TRUE)

# вычисляем вероятности для новых данных
newdata_score <- predict(chd_best, newdata, type="prob")

# записываем датафрейм results, содержащий 
# новые данные и вычисленные вероятности
results <-data.frame(newdata, result=newdata_score)

# записываем на основе датафрейма results
# одноименный CSV-файл
write.csv(results, "C:/Trees/Temp/results.csv", row.names=FALSE)

# записываем на основе датафрейма results
# одноименный XLS-файл
library(xlsx)
write.xlsx2(results, "C:/Trees/Temp/results.xlsx", row.names=FALSE)

