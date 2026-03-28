# =============================================================================
# Business Analytics — Caso Integrador
# Máster en BI · Universitat Oberta de Catalunya
# Autora: Sol Represa
# Fecha original: 23/01/2020
# =============================================================================
# Dataset: Adult Census Income (UCI ML Repository)
# Objetivo: Predecir si el ingreso anual supera los 50K USD
#           + segmentación de perfiles mediante clustering
# =============================================================================

# --- Paquetes ----------------------------------------------------------------

library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)

# --- 1. Carga de datos -------------------------------------------------------

load_census_data <- function() {
  data <- read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
    sep = ",",
    header = FALSE,
    col.names = c(
      "age", "type_employer", "fnlwgt", "education",
      "education_num", "marital", "occupation", "relationship",
      "race", "sex", "capital_gain", "capital_loss",
      "hr_per_week", "country", "income"
    ),
    fill = FALSE,
    strip.white = TRUE,
    na.strings = "?"
  )
  
  cat("Registros cargados:", nrow(data), "\n")
  cat("Variables:", ncol(data), "\n")
  data
}

# --- 2. Limpieza y feature engineering ---------------------------------------

#' Elimina columnas redundantes y registros con NA
clean_data <- function(data) {
  # Eliminar variables que no aportan al análisis
  data$fnlwgt <- NULL
  data$education_num <- NULL
  
  # Eliminar registros incompletos (originalmente codificados como "?")
  n_before <- nrow(data)
  data <- na.omit(data)
  n_removed <- n_before - nrow(data)
  cat(sprintf(
    "Registros eliminados por NA: %d (%.1f%%)\n",
    n_removed, n_removed / n_before * 100
  ))
  
  data
}

#' Agrupa categorías de baja frecuencia para reducir dimensionalidad
recode_categories <- function(data) {
  
  # -- Tipo de empleador --
  employer_map <- c(
    "Federal-gov"    = "Federal-Govt",
    "Local-gov"      = "Other-Govt",
    "State-gov"      = "Other-Govt",
    "Private"        = "Private",
    "Self-emp-inc"   = "Self-Employed",
    "Self-emp-not-inc" = "Self-Employed",
    "Without-pay"    = "Not-Working",
    "Never-worked"   = "Not-Working"
  )
  data$type_employer <- factor(employer_map[as.character(data$type_employer)])
  
  # -- Ocupación --
  occupation_map <- c(
    "Adm-clerical"     = "Admin",
    "Armed-Forces"     = "Military",
    "Craft-repair"     = "Blue-Collar",
    "Exec-managerial"  = "White-Collar",
    "Farming-fishing"  = "Blue-Collar",
    "Handlers-cleaners" = "Blue-Collar",
    "Machine-op-inspct" = "Blue-Collar",
    "Other-service"    = "Service",
    "Priv-house-serv"  = "Service",
    "Prof-specialty"   = "Professional",
    "Protective-serv"  = "Other-Occupations",
    "Sales"            = "Sales",
    "Tech-support"     = "Other-Occupations",
    "Transport-moving" = "Blue-Collar"
  )
  data$occupation <- factor(occupation_map[as.character(data$occupation)])
  
  # -- País → Región --
  country_map <- c(
    "Cambodia" = "SE-Asia", "Canada" = "British-Commonwealth",
    "China" = "China", "Columbia" = "South-America",
    "Cuba" = "Other", "Dominican-Republic" = "Latin-America",
    "Ecuador" = "South-America", "El-Salvador" = "South-America",
    "England" = "British-Commonwealth", "France" = "Euro_1",
    "Germany" = "Euro_1", "Greece" = "Euro_2",
    "Guatemala" = "Latin-America", "Haiti" = "Latin-America",
    "Holand-Netherlands" = "Euro_1", "Honduras" = "Latin-America",
    "Hong" = "China", "Hungary" = "Euro_2",
    "India" = "British-Commonwealth", "Iran" = "Other",
    "Ireland" = "British-Commonwealth", "Italy" = "Euro_1",
    "Jamaica" = "Latin-America", "Japan" = "Other",
    "Laos" = "SE-Asia", "Mexico" = "Latin-America",
    "Nicaragua" = "Latin-America",
    "Outlying-US(Guam-USVI-etc)" = "Latin-America",
    "Peru" = "South-America", "Philippines" = "SE-Asia",
    "Poland" = "Euro_2", "Portugal" = "Euro_2",
    "Puerto-Rico" = "Latin-America", "Scotland" = "British-Commonwealth",
    "South" = "Euro_2", "Taiwan" = "China",
    "Thailand" = "SE-Asia", "Trinadad&Tobago" = "Latin-America",
    "United-States" = "United-States", "Vietnam" = "SE-Asia",
    "Yugoslavia" = "Euro_2"
  )
  data$country <- factor(country_map[as.character(data$country)])
  
  # -- Educación --
  education_map <- c(
    "10th" = "Dropout", "11th" = "Dropout", "12th" = "Dropout",
    "1st-4th" = "Dropout", "5th-6th" = "Dropout",
    "7th-8th" = "Dropout", "9th" = "Dropout",
    "Preschool" = "Dropout",
    "Assoc-acdm" = "Associates", "Assoc-voc" = "Associates",
    "Bachelors" = "Bachelors", "Doctorate" = "Doctorate",
    "HS-grad" = "HS-Graduate", "Some-college" = "HS-Graduate",
    "Masters" = "Masters", "Prof-school" = "Prof-School"
  )
  data$education <- factor(education_map[as.character(data$education)])
  
  # -- Estado civil --
  marital_map <- c(
    "Never-married"         = "Never-Married",
    "Married-AF-spouse"     = "Married",
    "Married-civ-spouse"    = "Married",
    "Married-spouse-absent" = "Not-Married",
    "Separated"             = "Not-Married",
    "Divorced"              = "Not-Married",
    "Widowed"               = "Widowed"
  )
  data$marital <- factor(marital_map[as.character(data$marital)])
  
  # -- Raza (renombrar para legibilidad) --
  race_map <- c(
    "White" = "White", "Black" = "Black",
    "Amer-Indian-Eskimo" = "Amer-Indian",
    "Asian-Pac-Islander" = "Asian", "Other" = "Other"
  )
  data$race <- factor(race_map[as.character(data$race)])
  
  data
}

#' Discretiza variables de capital y estandariza numéricas
transform_features <- function(data) {
  
  # Discretizar capital_gain y capital_loss en 3 niveles
  discretize_capital <- function(x) {
    median_positive <- median(x[x > 0])
    ordered(
      cut(x, breaks = c(-Inf, 0, median_positive, Inf)),
      labels = c("None", "Low", "High")
    )
  }
  data$capital_gain <- discretize_capital(data$capital_gain)
  data$capital_loss <- discretize_capital(data$capital_loss)
  
  # Recodificar variable objetivo a S/N
  data$income <- factor(ifelse(data$income == "<=50K", "N", "S"))
  
  # Estandarizar variables numéricas (centrar y escalar)
  data$age <- scale(data$age)
  data$hr_per_week <- scale(data$hr_per_week)
  
  # Convertir sexo a dummy numérica para los modelos que lo requieren
  data$sex <- ifelse(data$sex == "Female", 1, 0)
  
  data
}

# --- 3. Visualización exploratoria -------------------------------------------

plot_exploratory <- function(data) {
  
  p1 <- ggplot(data, aes(x = hr_per_week, y = age, color = capital_gain)) +
    geom_point(alpha = 0.4) +
    labs(x = "Horas por semana (estandarizada)",
         y = "Edad (estandarizada)",
         color = "Capital Gain") +
    theme_bw()
  
  p2 <- ggplot(data, aes(x = capital_gain, y = age, fill = factor(sex))) +
    geom_boxplot() +
    scale_fill_discrete(labels = c("Male", "Female")) +
    labs(x = "Capital Gain", y = "Edad (estandarizada)", fill = "Sexo") +
    theme_bw()
  
  print(p1)
  print(p2)
}

# --- 4. Modelado supervisado -------------------------------------------------

#' Particiona los datos en entrenamiento (70%) y prueba (30%)
split_data <- function(data, seed = 1234) {
  set.seed(seed)
  idx <- sample(
    x       = c(TRUE, FALSE),
    size    = nrow(data),
    replace = TRUE,
    prob    = c(0.7, 0.3)
  )
  list(
    train = data[idx, ],
    test  = data[!idx, ]
  )
}

#' Entrena los tres modelos de clasificación con cross-validation repetida
train_models <- function(train_data, target_formula, seed = 400) {
  set.seed(seed)
  
  ctrl <- trainControl(method = "repeatedcv", repeats = 5)
  
  cat("Entrenando KNN...\n")
  knn_fit <- train(
    target_formula,
    data       = train_data,
    method     = "knn",
    trControl  = ctrl,
    tuneLength = 20
  )
  
  cat("Entrenando Árbol de Decisión...\n")
  tree_fit <- train(
    target_formula,
    data       = train_data,
    method     = "rpart",
    trControl  = ctrl,
    tuneLength = 20
  )
  
  cat("Entrenando Random Forest...\n")
  rf_fit <- train(
    target_formula,
    data       = train_data,
    method     = "rf",
    trControl  = ctrl
  )
  
  list(knn = knn_fit, tree = tree_fit, rf = rf_fit)
}

#' Compara los modelos y muestra matrices de confusión
evaluate_models <- function(models, test_data, target_var = "capital_gain") {
  
  # Comparación de métricas de resampling
  results <- resamples(list(
    KNN   = models$knn,
    Arbol = models$tree,
    RF    = models$rf
  ))
  
  cat("\n=== Resumen de métricas (resampling) ===\n")
  print(summary(results))
  print(bwplot(results))
  
  # Matrices de confusión sobre test
  cat("\n=== Matrices de confusión (test set) ===\n\n")
  for (name in names(models)) {
    predictions <- predict(models[[name]], newdata = test_data)
    cat(sprintf("--- %s ---\n", toupper(name)))
    cm <- confusionMatrix(predictions, test_data[[target_var]])
    print(cm)
    cat("\n")
  }
}

# --- 5. Clustering (K-Means) ------------------------------------------------

run_clustering <- function(original_data, k = 5) {
  # Usar datos originales (sin estandarizar) para las variables numéricas
  cluster_data <- original_data[, c("capital_gain", "capital_loss",
                                     "hr_per_week", "age")]
  
  set.seed(400)
  km <- kmeans(cluster_data, centers = k, iter.max = 25, nstart = 10)
  
  cat(sprintf("\n=== K-Means con k = %d ===\n", k))
  cat("Tamaño de clusters:", km$size, "\n\n")
  
  cat("Perfiles medios por cluster:\n")
  profiles <- aggregate(cluster_data,
                        by = list(Cluster = km$cluster),
                        FUN = mean)
  print(profiles)
  
  km
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

# 1. Cargar
raw_data <- load_census_data()

# 2. Limpiar y transformar
data <- clean_data(raw_data)
data <- recode_categories(data)

# Guardar copia antes de transformar (para clustering con valores originales)
data_for_clustering <- data

data <- transform_features(data)

# 3. Explorar visualmente
plot_exploratory(data)

# 4. Modelado supervisado
splits <- split_data(data)
cat(sprintf(
  "Train: %d (%.0f%%) | Test: %d (%.0f%%)\n",
  nrow(splits$train), 100 * nrow(splits$train) / nrow(data),
  nrow(splits$test),  100 * nrow(splits$test) / nrow(data)
))

formula_supervised <- capital_gain ~ age + hr_per_week + sex
models <- train_models(splits$train, formula_supervised)
evaluate_models(models, splits$test)

# 5. Clustering
km_result <- run_clustering(data_for_clustering, k = 5)
