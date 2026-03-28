# Business Analytics — Caso Integrador
 
Ejercicio integrador del **Máster en Business Intelligence** de la [Universitat Oberta de Catalunya (UOC)](https://www.uoc.edu/). El proyecto aplica técnicas de clasificación supervisada y segmentación no supervisada sobre datos reales del censo de EE.UU.
 
## Descripción del proyecto
 
El objetivo es predecir si una persona percibe ingresos superiores a 50 000 USD anuales a partir de variables sociodemográficas y laborales. Además, se realiza una segmentación de perfiles mediante clustering.
 
Se trabaja con el dataset **Adult (Census Income)** del [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/adult), que contiene ~32 500 registros con 15 atributos originales (edad, tipo de empleo, nivel educativo, estado civil, ocupación, raza, sexo, horas trabajadas por semana, país de origen, entre otros).
 
## Pipeline de análisis
 
El flujo completo implementado en `codigo_Anne.R` sigue estas etapas:
 
**1. Carga y exploración** — Lectura directa desde el repositorio UCI. Inspección con `dim()`, `head()`, `summary()` y `str()`.
 
**2. Limpieza y preprocesamiento** — Eliminación de variables redundantes (`fnlwgt`, `education_num`). Agrupación de categorías de baja frecuencia en variables como tipo de empleador, ocupación, país, educación y estado civil para reducir la dimensionalidad categórica. Discretización de `capital_gain` y `capital_loss` en tres niveles (None / Low / High). Estandarización de variables numéricas (`age`, `hr_per_week`). Tratamiento de valores faltantes (`?`) con eliminación de registros incompletos.
 
**3. Partición de datos** — Split aleatorio 70/30 (entrenamiento / prueba) con `set.seed(1234)` para reproducibilidad.
 
**4. Visualización exploratoria** — Scatter plots, boxplots y gráficos con `ggplot2` para explorar relaciones entre variables cuantitativas y categóricas.
 
**5. Modelado supervisado** — Tres modelos de clasificación entrenados con validación cruzada repetida (5 repeticiones):
 
| Modelo | Método | Librería |
|---|---|---|
| K-Nearest Neighbors | `knn` | `caret` |
| Árbol de Decisión | `rpart` | `caret` / `rpart` |
| Random Forest | `rf` | `caret` / `randomForest` |
 
**6. Evaluación y comparación** — Comparación de modelos mediante `resamples()`, boxplots de métricas, y matrices de confusión sobre el conjunto de prueba.
 
**7. Clustering (K-Means)** — Segmentación no supervisada en 5 clusters usando las variables `capital_gain`, `capital_loss`, `hr_per_week` y `age`. Descripción de perfiles mediante valores medios por cluster.
 
## Estructura del repositorio
 
```
Business_Analytics/
├── codigo_Anne.R              # Script principal con todo el pipeline
├── Actividad-Global (1).pdf   # Consigna de la actividad integradora
└── README.md
```
 
## Requisitos
 
- **R** ≥ 3.5
- Paquetes: `ggplot2`, `caret`, `e1071`, `class`, `rpart`, `rpart.plot`, `randomForest`
 
Para instalar las dependencias:
 
```r
install.packages(c("ggplot2", "caret", "e1071", "class", "rpart", "rpart.plot", "randomForest"))
```
 
## Cómo ejecutar
 
1. Clonar el repositorio:
   ```bash
   git clone https://github.com/solrepresa/Business_Analytics.git
   ```
2. Abrir `codigo_Anne.R` en RStudio (o cualquier entorno R).
3. Ejecutar el script secuencialmente — los datos se descargan automáticamente desde el repositorio UCI.
 
## Dataset
 
El dataset [Adult / Census Income](https://archive.ics.uci.edu/ml/datasets/adult) fue extraído de la base de datos del censo de 1994 por Ronny Kohavi y Barry Becker. Contiene información demográfica y laboral, con la variable objetivo binaria: `income` (≤50K / >50K).
 
## Autora
 
**Sol Represa** — [GitHub](https://github.com/solrepresa)
 
## Licencia
 
Proyecto académico — UOC, Máster en Business Intelligence.
