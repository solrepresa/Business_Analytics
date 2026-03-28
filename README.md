# Business Analytics — Caso Integrador

Ejercicio integrador del **Máster en Business Intelligence** de la [Universitat Oberta de Catalunya (UOC)](https://www.uoc.edu/). El proyecto aplica técnicas de clasificación supervisada y segmentación no supervisada sobre datos reales del censo de EE.UU.

## Descripción del proyecto

Se trabaja con el dataset **Adult (Census Income)** del [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/adult), que contiene ~32 500 registros con 15 atributos sociodemográficos y laborales (edad, tipo de empleo, nivel educativo, estado civil, ocupación, raza, sexo, horas trabajadas por semana, país de origen, entre otros).

El análisis tiene dos objetivos: clasificar el nivel de ganancia de capital (`capital_gain`) a partir de edad, horas trabajadas y sexo mediante modelos supervisados, y segmentar perfiles de personas mediante clustering no supervisado.

## Pipeline de análisis

El script `codigo_Anne.R` está organizado en funciones modulares que siguen este flujo:

**1. Carga y exploración** (`load_census_data`) — Lectura directa desde el repositorio UCI. Los valores faltantes (`?`) se capturan como `NA` desde la lectura con `na.strings`.

**2. Limpieza** (`clean_data`) — Eliminación de variables redundantes (`fnlwgt`, `education_num`) y de registros incompletos con `na.omit()`.

**3. Reducción de categorías** (`recode_categories`) — Agrupación de niveles de baja frecuencia en tipo de empleador, ocupación, país (→ región), educación, estado civil y raza mediante mapas de lookup (named vectors).

**4. Transformación de features** (`transform_features`) — Discretización de `capital_gain` y `capital_loss` en tres niveles (None / Low / High). Estandarización de `age` y `hr_per_week`. Conversión de `sex` a dummy numérica. Recodificación de `income` a S/N.

**5. Visualización exploratoria** (`plot_exploratory`) — Scatter plots y boxplots con `ggplot2` para explorar relaciones entre variables.

**6. Modelado supervisado** (`train_models`, `evaluate_models`) — Tres modelos entrenados con validación cruzada repetida (5 repeticiones) sobre la fórmula `capital_gain ~ age + hr_per_week + sex`:

| Modelo | Método | Librería |
|---|---|---|
| K-Nearest Neighbors | `knn` | `caret` |
| Árbol de Decisión | `rpart` | `caret` / `rpart` |
| Random Forest | `rf` | `caret` / `randomForest` |

Comparación mediante `resamples()` y matrices de confusión sobre el conjunto de prueba (split 70/30).

**7. Clustering K-Means** (`run_clustering`) — Segmentación en 5 clusters usando `capital_gain`, `capital_loss`, `hr_per_week` y `age` (datos sin estandarizar). Descripción de perfiles mediante valores medios por cluster.

## Estructura del repositorio

```
Business_Analytics/
├── codigo_Anne.R              # Script principal (funciones modulares + ejecución)
├── Actividad-Global (1).pdf   # Consigna de la actividad integradora
└── README.md
```

## Requisitos

- **R** ≥ 3.5
- Paquetes: `ggplot2`, `caret`, `e1071`, `rpart`, `rpart.plot`, `randomForest`

```r
install.packages(c("ggplot2", "caret", "e1071", "rpart", "rpart.plot", "randomForest"))
```

## Cómo ejecutar

1. Clonar el repositorio:
   ```bash
   git clone https://github.com/solrepresa/Business_Analytics.git
   ```
2. Abrir `codigo_Anne.R` en RStudio o cualquier entorno R.
3. Ejecutar el script — los datos se descargan automáticamente desde UCI.

## Dataset

[Adult / Census Income](https://archive.ics.uci.edu/ml/datasets/adult) — Extraído de la base de datos del censo de EE.UU. de 1994 (Ronny Kohavi y Barry Becker). Contiene información demográfica y laboral con la variable objetivo binaria `income` (≤50K / >50K).

## Autora

**Sol Represa** — [GitHub](https://github.com/solrepresa)

## Licencia

Proyecto académico — UOC, Máster en Business Intelligence.
