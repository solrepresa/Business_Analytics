# 23/01/2020
# Tarea integradora: Inteligencia de Negocio y Big Data Analytics
# Autora: Sol Represa


# # # # # # # # # # # # # # # # # # # # # 

## 1) Instalaci�n de paquetes ####

# # # # # # # # # # # # # # # # # # # # # 


#install.packages("class")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("e1071")


# # # # # # # # # # # # # # # # # # # # # 

## 2) Cargamos librer�as ####

# # # # # # # # # # # # # # # # # # # # # 

library(ggplot2)
library (caret)



# # # # # # # # # # # # # # # # # # # # # 

## 3) Cargamos los datos  ####

# # # # # # # # # # # # # # # # # # # # # 


data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                  sep = ",", 
                  header = F, 
                  col.names = c("age", "type_employer", "fnlwgt", "education", 
                                "education_num", "marital", "occupation", "relationship",
                                "race", "sex", "capital_gain", "capital_loss", "hr_per_week", 
                                "country", "income"), 
                  fill = FALSE, 
                  strip.white = T)

dim(data)  #dimension de la tabla

head(data)   #cabecera de la tabla
head(data[1:6]) 
head(data, n = 6)

head(data[7:11]) #explorar las primeras filas
data[7:11]

head(data[12:15])

tail(data) #final de la tabla


summary(data) #resumen de los datos. Es distinto si es cuantitativo y cualitativo
str(data) #Estructura de la tabla y sus datos


# Por las dudas, se hace una copia de los datos originales
data2 <- data


# # # # # # # # # # # # # # # # # # # # # 

## 4) Limpieza de los datos ####

# # # # # # # # # # # # # # # # # # # # # 


# Queremos quitar las columnas que no nos interesan: "education_num" , "fnlwgt"

# Opci�n 1  # Decimos que esas culmunas son NuLL
data[["education_num"]] = NULL
data[["fnlwgt"]] = NULL


# Opci�n 2   # Decimos que queremos la tabla sin esas columnas
names(data)
data <- data[ -c(3, 5)]


# Comprobamos que se han eliminado las variables indicadas
head(data)



## 4.1 Reducir dimensiones de las variables ####

# Queremos reducir la profundidad de algunas de las variables


## Primero se convierten las variables para manipularlas adecuadamente
# �C�mo estaban? �A qu� pasan?

data$type_employer = as.character(data$type_employer)
data$occupation = as.character(data$occupation)
data$country = as.character(data$country)
data$education = as.character(data$education)
data$race = as.character(data$race)
data$marital = as.character(data$marital)


# Manejo de expresiones regulares en R
# VER: https://www.r-bloggers.com/demystifying-regular-expressions-in-r/

# type_employer
# En tipo de empleador se agrupan algunas variables que tienen poca frecuencia y son similares
data$type_employer = gsub("^Federal-gov","Federal-Govt",data$type_employer)
data$type_employer = gsub("^Local-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^State-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^Private","Private",data$type_employer)
data$type_employer = gsub("^Self-emp-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Self-emp-not-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Without-pay","Not-Working",data$type_employer)
data$type_employer = gsub("^Never-worked","Not-Working",data$type_employer)

## En ocupaci�n se pueden agrupar algunas para restarle profundidad a la variable
data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar",data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar",data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar",data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar",data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional",data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations",data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations",data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar",data$occupation)

## En Country logicamente la mayor�a de las observaciones son de USA pero hay mucha 
# diversidad de pa�ses y es interesante agruparlos por regi�n:
data$country[data$country=="Cambodia"] = "SE-Asia"
data$country[data$country=="Canada"] = "British-Commonwealth"
data$country[data$country=="China"] = "China"
data$country[data$country=="Columbia"] = "South-America"
data$country[data$country=="Cuba"] = "Other"
data$country[data$country=="Dominican-Republic"] = "Latin-America"
data$country[data$country=="Ecuador"] = "South-America"
data$country[data$country=="El-Salvador"] = "South-America"
data$country[data$country=="England"] = "British-Commonwealth"
data$country[data$country=="France"] = "Euro_1"
data$country[data$country=="Germany"] = "Euro_1"
data$country[data$country=="Greece"] = "Euro_2"
data$country[data$country=="Guatemala"] = "Latin-America"
data$country[data$country=="Haiti"] = "Latin-America"
data$country[data$country=="Holand-Netherlands"] = "Euro_1"
data$country[data$country=="Honduras"] = "Latin-America"
data$country[data$country=="Hong"] = "China"
data$country[data$country=="Hungary"] = "Euro_2"
data$country[data$country=="India"] = "British-Commonwealth"
data$country[data$country=="Iran"] = "Other"
data$country[data$country=="Ireland"] = "British-Commonwealth"
data$country[data$country=="Italy"] = "Euro_1"
data$country[data$country=="Jamaica"] = "Latin-America"
data$country[data$country=="Japan"] = "Other"
data$country[data$country=="Laos"] = "SE-Asia"
data$country[data$country=="Mexico"] = "Latin-America"
data$country[data$country=="Nicaragua"] = "Latin-America"
data$country[data$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
data$country[data$country=="Peru"] = "South-America"
data$country[data$country=="Philippines"] = "SE-Asia"
data$country[data$country=="Poland"] = "Euro_2"
data$country[data$country=="Portugal"] = "Euro_2"
data$country[data$country=="Puerto-Rico"] = "Latin-America"
data$country[data$country=="Scotland"] = "British-Commonwealth"
data$country[data$country=="South"] = "Euro_2"
data$country[data$country=="Taiwan"] = "China"
data$country[data$country=="Thailand"] = "SE-Asia"
data$country[data$country=="Trinadad&Tobago"] = "Latin-America"
data$country[data$country=="United-States"] = "United-States"
data$country[data$country=="Vietnam"] = "SE-Asia"
data$country[data$country=="Yugoslavia"] = "Euro_2"

## En educaci�n, tambi�n se agrupan algunos, la idea es restarle profundidad
data$education = gsub("^10th","Dropout",data$education)
data$education = gsub("^11th","Dropout",data$education)
data$education = gsub("^12th","Dropout",data$education)
data$education = gsub("^1st-4th","Dropout",data$education)
data$education = gsub("^5th-6th","Dropout",data$education)
data$education = gsub("^7th-8th","Dropout",data$education)
data$education = gsub("^9th","Dropout",data$education)
data$education = gsub("^Assoc-acdm","Associates",data$education)
data$education = gsub("^Assoc-voc","Associates",data$education)
data$education = gsub("^Bachelors","Bachelors",data$education)
data$education = gsub("^Doctorate","Doctorate",data$education)
data$education = gsub("^HS-Grad","HS-Graduate",data$education)
data$education = gsub("^Masters","Masters",data$education)
data$education = gsub("^Preschool","Dropout",data$education)
data$education = gsub("^Prof-school","Prof-School",data$education)
data$education = gsub("^Some-college","HS-Graduate",data$education)

## De igual forma se agrupan los estados maritales
data$marital[data$marital=="Never-married"] = "Never-Married"
data$marital[data$marital=="Married-AF-spouse"] = "Married"
data$marital[data$marital=="Married-civ-spouse"] = "Married"
data$marital[data$marital=="Married-spouse-absent"] = "Not-Married"
data$marital[data$marital=="Separated"] = "Not-Married"
data$marital[data$marital=="Divorced"] = "Not-Married"
data$marital[data$marital=="Widowed"] = "Widowed"

## La etnia se cambia para que sea m�s f�cil de leer
data$race[data$race=="White"] = "White"
data$race[data$race=="Black"] = "Black"
data$race[data$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
data$race[data$race=="Asian-Pac-Islander"] = "Asian"
data$race[data$race=="Other"] = "Other"

## Se regresan a factores las variables categ�ricas
data$marital = factor(data$marital)
data$education = factor(data$education)
data$country = factor(data$country)
data$type_employer = factor(data$type_employer)
data$occupation = factor(data$occupation)
data$race = factor(data$race)
data$sex = factor(data$sex)
data$relationship = factor(data$relationship)

## Se recodifica la variable a predecir a S/N
data$income = as.factor(ifelse(data$income == data$income[1],"N","S"))


#Corroborar que todo este bien:
as.data.frame(table(data$country))  # table() conteo de frecuencia
as.data.frame(table(data2$country))


## 4.2 Discretizaci�n de las variables ####

# La funci�n cut divide el rango de x en intervalos y codifica los valores en x seg�n el
# intervalo que caen. El intervalo m�s a la izquierda corresponde al nivel uno, 
# el siguiente m�s a la izquierda al nivel dos y as� sucesivamente.

data[["capital_gain"]] <- ordered(cut(data$capital_gain,
                                      c(-Inf, 0, 
                                        median(data[["capital_gain"]][data[["capital_gain"]] >0]),
                                        Inf)),
                                  labels = c("None", "Low", "High"))

data[["capital_loss"]] <- ordered(cut(data$capital_loss,
                                      c(-Inf, 0, 
                                        median(data[["capital_loss"]][data[["capital_loss"]] >0]), 
                                        Inf)), 
                                  labels = c("None", "Low", "High"))


table(data[,"capital_gain"])  # Se muestra la distribuci�n de valores
table(data[,"capital_loss"])

data$age = scale(data$age)  ## La edad y los horas por semana se reescalan, centradas y reducidas
data$hr_per_week = scale(data$hr_per_week)

summary(data$age) # Resumen de las variables
summary(data$hr_per_week)



### 4.3 Tratamiento de los valores NULL ####

sum(is.na(data)) # Suma la cantidad de nulos

as.data.frame(table(data$country)) # Visualizamos un recuento de la variable country

### EJERCICIO 1 ####

## Opci�n 1  # Reemplazamos "?" con null y posteriormente los contamos
is.na(data) = data =='?'
is.na(data) = data ==' ?'

data = na.omit(data)  # Se eliminan los registros con datos nulos


# Opci�n 2  # buscamos los "?" y nos quedamos sin esas filas
ind <- which(data2$type_employer == "?")  
data <- data[-ind,]

# ATENCI�N: repetirlo para cada columna que tenga "?"


# Opci�n 3  # Cambiamos los valores ? por NA y luego usamos la funci�n complete.cases
data[data == "?" & data == " ?"] <- NA
data <- data[complete.cases(data),]


dim(data)  # Evaluamos la cantidad de registros eliminados
dim(data2)
dim(data2)[1]-dim(data)[1]   # �cuantos datos faltaban?
(dim(data2)[1]-dim(data)[1])/dim(data2)[1]*100  #�Qu� % representan?


# # # # # # # # # # # # # # # # # # # # # 

## 5) Fase Modelado ####

# # # # # # # # # # # # # # # # # # # # # 

# 5.1 Tomamos una muestra aleatoria de los datos ####
# Realizamos un split aleatorio de los datos para definir conjunto de entrenamiento / prueba

set.seed(1234)

ind <- sample(2, 
              nrow(data), 
              replace=TRUE, 
              prob = c(0.7, 0.3))

trainData <- data[ind == 1,]
dim(trainData)[1]/dim(data)[1]  #Corroboramos que tenemos ~70%

testData <- data[ind==2, ]
dim(testData)[1]/dim(data)[1]  #Corroboramos que tenemos ~30%


# 5.2 Visualizaci�n de los datos####

## EJERCICIO 2 ####
# Gr�fica que contenga las dos variables cuantitativas age y hr_per_week 
# y el segundo que est� asociado a una variable categ�rica.

plot(data$hr_per_week, data$age)
plot(data$hr_per_week, data$age, 
     xlab = "Hr per week", ylab = "Age",
     col = "red")


ggplot(data, aes( y = age, x = hr_per_week, col = capital_gain)) + 
  geom_point() + 
  theme_bw()


boxplot( hr_per_week ~ sex, data = data )
boxplot( age ~ sex, data = data )

ggplot(data, aes( x= capital_gain, y= age, fill = sex)) + 
  geom_boxplot() + 
  theme_bw() 


# 5.3 Modelamos los datos ####

## EJERCICIO 3 ####
# Aplicar un modelo KNN 
# utilizando como variables en imput la edad, las horas trabajadas por semana y el sexo. 
# Recordad que para poder a�adir el sexo tendr�is que expresarla como variable
# "cuantitativa" de presencia y ausencia. 
# Valorar la predicci�n utilizando distintos n�meros de vecinos entre 20 y 30

# Male = 0
# Female = 1

data$sex <- as.character(data$sex)
data[which(data$sex == "Male"), 8] <- 0
data[which(data$sex == "Female"), 8] <- 1


### KNN ####

# Ver: https://rpubs.com/njvijay/16444
library (caret)
set.seed(400)

ctrl <- trainControl( method = "repeatedcv", repeats = 5) # Metodo de cross validation 
knnFit <- train(capital_gain ~ age + hr_per_week + sex, 
                data = trainData, 
                method = "knn", 
                trControl = ctrl, 
                # preProcess = c("center","scale"), 
                tuneLength = 20)


knnFit #�Qu� dio?
plot(knnFit) 



#### ARBOL DE DECISION ####
# VER: https://rpubs.com/jboscomendoza/arboles_decision_clasificacion

# EJERCICIO 4

# library(rpart)
# arbol_1 <- rpart(capital_gain ~ age + hr_per_week + sex, data = trainData)
#Ver grafica con rpart del arbol


arbol_1 <- train(capital_gain ~ age + hr_per_week + sex, 
                data = trainData, 
                method = "rpart", 
                trControl = ctrl, 
                # preProcess = c("center","scale"), 
                tuneLength = 20)

arbol_1





#### RANDOM FOREST ####

# EJERCICIO 5
library(randomForest)
set.seed(400)

ctrl <- trainControl( method = "repeatedcv", repeats = 5) # Metodo de cross validation 
RFFit <- train(capital_gain ~ age + hr_per_week + sex, 
                data = trainData, 
                method = "rf", 
               # preProcess = c("center","scale"), 
                trControl = ctrl) 


RFFit
plot(RFFit)



### 5.4. Seleccionar el mejor modelo ####

# Ejercicio 6 ###
results <- resamples(list( kNN=knnFit, arbol=arbol_1, RF=RFFit)) # juntamos los modelos

summary(results)  #analizamos las distribuciones
bwplot(results) # boxplots de los results
dotplot(results) #dotplot



## Visualizar matrices de confuci�n

knnPredict <- predict(knnFit, newdata = testData )
confusionMatrix(knnPredict, testData$capital_gain)   #ESTUDIAR mejor las salidas del modelo

arbolPredict <- predict(arbol_1, newdata = testData)
confusionMatrix(arbolPredict, testData$capital_gain)  

RFPredict <- predict(RFFit, newdata = testData)
confusionMatrix(RFPredict, testData$capital_gain)  



# # # # # # # # # # # # # # # # # # # # # 

# Clusters

# # # # # # # # # # # # # # # # # # # # # 

# VER: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

data3 = data2[, c("capital_gain","capital_loss","hr_per_week","age")]

# EJERCICIO 6
# Realice una segmentaci�n kmeans para 5 clusters
km <- kmeans(data3, centers = 5, iter.max = 10, nstart = 1)
print(km)

km$size  #tama�o de cluster


# EJERCICIO 7
# Describa los perfiles de clientes representados por los clusters


# EJERCICIO 8
#Para poder interpretar los clusters es habitual representarlos mediante el valor medio de las variables
aggregate(data3, by = list(cluster=km$cluster), mean)
