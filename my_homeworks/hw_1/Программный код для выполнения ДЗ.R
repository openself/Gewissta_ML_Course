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