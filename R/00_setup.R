# =============================================================================
# TP Integrador - Modulo R | Maestria en Econometria - UTDT
# Laboratorio de Programacion en Python y R
# Docente: Ian Evangelos Bounos
#
# 00_setup.R - Librerias, semilla, rutas y utilidades comunes
#
# Este script define el entorno que usan todos los demás
# Se invoca desde cada script con source("R/00_setup.R")
# =============================================================================

# >> LIBRERÍAS <<
# tidyverse   -> manipulación (dplyr/tidyr) y visualización (ggplot2)
# tsibble     -> series de tiempo en formato tidy
# fable       -> modelos de series de tiempo (ARIMA, ETS, TSLM) sobre tsibbles
# feasts      -> descomposición STL, ACF/PACF y features de series
# fabletools  -> model(), forecast(), accuracy()
# tseries     -> test ADF (Dickey-Fuller aumentado)

suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)
  library(fable)
  library(feasts)
  library(tseries)
  library(scales)
})

# >>> Semilla <<<
# Fijada una sola vez para todo el proyecto: la unica aleatoriedad relevante
# aparece en el muestreo de combos para graficos exploratorios.
SEMILLA <- 2026
set.seed(SEMILLA)

# --- Rutas relativas ---------------------------------------------------------
# Todas las rutas cuelgan de la raiz del proyecto, de modo que el repositorio
# corre sin modificaciones en cualquier maquina (requisito de la entrega).
RUTA_DATOS    <- "data"
RUTA_FIGURAS  <- "output/figuras"
RUTA_TABLAS   <- "output/tablas"
RUTA_RDS      <- "output/rds"

for (d in c(RUTA_FIGURAS, RUTA_TABLAS, RUTA_RDS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# --- Parámetros del analisis -------------------------------------------------

SEMANA_MAX       <- 145
H_PROYECCION     <- 10          # horizonte de proyección (10 semanas)
SEMANA_CORTE     <- SEMANA_MAX - H_PROYECCION  # ultima semana de entrenamiento = 135

# El dataset no trae fechas calendario
# Anclaje de la semana 1 a la primera semana de 2016 para poder usar
# yearweek() y que los modelos reconozcan una frecuencia semanal (periodo 52).

SEMANA_BASE <- yearweek("2016 W01")

# --- Estética / formato ----------------------------------------------------------

tema_tp <- function(base_size = 13.5) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.15)),
      plot.subtitle   = element_text(color = "grey30", size = rel(0.92)),
      plot.caption    = element_text(color = "grey45", size = rel(0.75), hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
theme_set(tema_tp())

PALETA <- c(
  azul    = "#2C6E9B",
  naranja = "#D1642B",
  verde   = "#3F8F5C",
  rojo    = "#B4353A",
  gris    = "#7A7A7A",
  violeta = "#6B5B95"
)

PALETA_TIPO <- c("TYPE_A" = "#2C6E9B", "TYPE_B" = "#D1642B", "TYPE_C" = "#3F8F5C")

# --- Utilidades --------------------------------------------------------------

# PAra uardar un ggplot en output/figuras con tamano y resolución uniformes:
guardar_fig <- function(plot, nombre, ancho = 8.5, alto = 5.0, dpi = 220) {
  # ajuste de subtítulos largos
  envolver <- function(x, ancho_txt) {
    if (is.null(x) || !is.character(x)) return(x)
    paste(strwrap(paste(x, collapse = " "), width = ancho_txt), collapse = "\n")
  }
  if (!is.null(plot$labels$caption))
    plot <- plot + labs(caption  = envolver(plot$labels$caption, 108))
  if (!is.null(plot$labels$subtitle))
    plot <- plot + labs(subtitle = envolver(plot$labels$subtitle, 82))

  ruta <- file.path(RUTA_FIGURAS, paste0(nombre, ".png"))
  ggsave(ruta, plot = plot, width = ancho, height = alto, dpi = dpi, bg = "white")
  message("  figura guardada: ", ruta)
  invisible(ruta)
}

# Para guardar una tabla de resultados en output/tablas
guardar_tabla <- function(df, nombre) {
  ruta <- file.path(RUTA_TABLAS, paste0(nombre, ".csv"))
  readr::write_csv(df, ruta)
  message("  tabla guardada: ", ruta)
  invisible(ruta)
}

# Convierte el 'indice de semana en yearweek
a_yearweek <- function(week) SEMANA_BASE + (week - 1)

# Convierte yearweek al indice entero de semana
a_week <- function(yw) as.integer(as.numeric(yw - SEMANA_BASE)) + 1L

# Convierte eje X de yearweek al índice de semana correspondiente
eje_semana <- function(nombre = "Semana del panel") {
  scale_x_yearweek(name = nombre, labels = function(b) a_week(b))
}

# Formateador de miles para los ejes
fmt_miles <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
# Mensaje de finalización
message("Setup listo | semilla = ", SEMILLA,
        " | corte train/test = semana ", SEMANA_CORTE,
        " | horizonte = ", H_PROYECCION, " semanas")
