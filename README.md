# Pronóstico de demanda semanal en una red de centros de distribución de comida

**Trabajo Práctico Integrador — Módulo R**

Laboratorio de Programación en Python y R · Maestría en Econometría · Universidad Torcuato Di Tella

------------------------------------------------------------------------

## Qué hay en este repositorio

Un análisis para construir un sistema de pronóstico de demanda a diez semanas para una empresa de reparto de comida que opera 77 centros de distribución, con modelos de series de tiempo (ARIMA / SARIMA / ARIMAX) sobre 145 semanas de historia.

El trabajo completo está narrado en [**`informe.pdf`**](informe.pdf), que teje resultados e imágenes creadas con distintas librerías de R.

## El dataset

**Food Demand Forecasting**, publicado en Kaggle a partir de un problema real de la plataforma Analytics Vidhya: <https://www.kaggle.com/datasets/kannanaikkal/food-demand-forecasting>

Los cuatro archivos originales están incluidos en `data/`, de modo que el proyecto corre sin necesidad de descargar nada:

| Archivo | Filas | Contenido |
|----|---:|----|
| `train.csv` | 456.548 | pedidos semanales por centro y plato, semanas 1 a 145 |
| `test.csv` | 32.573 | mismas columnas sin la variable a predecir, semanas 146 a 155 |
| `meal_info.csv` | 51 | catálogo de platos: categoría y tipo de cocina |
| `fulfilment_center_info.csv` | 77 | catálogo de centros: ciudad, región, tipo y área de operación |

La variable dependiente es `num_orders`, el número de pedidos de la semana. Las variables explicativas disponibles son el precio cobrado, el precio de lista y dos indicadores binarios de acción promocional.

## La técnica aplicada

**Series de tiempo: ARIMA, SARIMA y ARIMAX**, implementadas con `tsibble` / `fable` en lugar de `ts` / `forecast`. La técnica econométrica es la misma que se vio en clase; el cambio de paquete responde a que el dataset es un panel de miles de series y `fable` estima el mismo modelo sobre todas ellas en una sola llamada.

La evaluación se hace de dos maneras: *backtesting* sobre una ventana de diez semanas (1–135 para entrenar, 136–145 para validar) y validación cruzada de origen móvil sobre once ventanas. Los modelos se comparan además en tres niveles de agregación: red completa, categoría de plato y combinación centro-plato.

## Cómo correr el proyecto

Requiere R ≥ 4.4 y los siguientes paquetes:

``` r
install.packages(c("tidyverse", "tsibble", "fable", "feasts", "tseries", "scales"))
```

Desde la raíz del repositorio:

``` bash
Rscript R/run_all.R
```

Tarda menos de un minuto y regenera todas las figuras (`output/figuras/`) y tablas (`output/tablas/`) citadas en el informe. Todas las rutas son relativas y la semilla aleatoria está fijada en 2026, de modo que los resultados son idénticos en cualquier máquina.

Para regenerar el informe en PDF hace falta además Quarto y una distribución LaTeX:

``` bash
quarto render informe.qmd --to pdf
```

## Estructura

```         
├── data/                        archivos originales del dataset (sin modificar)
├── R/
│   ├── 00_setup.R               librerías, semilla, rutas, tema gráfico y utilidades
│   ├── 01_datos.R               carga, variables derivadas y construcción de los tsibbles
│   ├── 02_eda.R                 Parte 2: análisis exploratorio (15 figuras)
│   ├── 03_modelos.R             Parte 3: modelado de la demanda total y evaluación
│   ├── 04_categorias_combos.R   Parte 3: categorías de plato y combinaciones centro-plato
│   ├── 05_forecast.R            proyección final de las semanas 146 a 155
│   └── run_all.R                ejecuta todo en orden
├── output/
│   ├── figuras/                 32 figuras en PNG
│   ├── tablas/                  33 tablas de resultados en CSV
│   └── rds/                     objetos intermedios (datos y modelos estimados)
├── informe.qmd                  fuente del informe
├── informe.pdf                  informe final, sin código
└── README.md
```

Los scripts están numerados en el orden en que deben ejecutarse. Cada uno invoca a `00_setup.R` y lee lo que necesita de `output/rds/`, de modo que también pueden correrse por separado una vez ejecutado `01_datos.R`.

## Principales resultados

1.  **Evaluar con una sola ventana de backtesting elige el modelo equivocado.** El pronóstico ingenuo gana en la ventana 136–145 y queda quinto de seis en validación cruzada móvil.

2.  **Forzar estacionalidad anual no aporta nada.** Con 2,8 ciclos de historia, la búsqueda automática elige el mismo modelo con y sin componente estacional habilitado.

3.  **Incorporar el plan comercial de la empresa mejora el pronóstico sólo donde los regresores efectivamente varían en el período proyectado**: gana en 11 de 12 categorías de producto, y pierde en el agregado y en las series individuales, donde durante las semanas proyectadas no hubo campañas que aprovechar.
