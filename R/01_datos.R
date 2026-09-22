# =============================================================================
# 01_datos.R
# Carga, integración y armado de los tsibbles (ETL)
#
# Sale de los cuatro CSV que conforman la base de datos para formar
# tres objetos de serie de tiempo que usan el EDA y el modelado
# 
# =============================================================================

source("R/00_setup.R")

message("\n== 01_datos.R ==")

# -----------------------------------------------------------------------------
# 1. Carga de los archivos
# -----------------------------------------------------------------------------
# train.csv   -> 456.548 filas con pedidos: una por semana, centro y plato
# test.csv    -> 32.573 filas (mismas columnas que train.csv pero sin num_orders / semanas 146-155)
# meal_info   -> 51 platos. Dimensiones: categoría y cocina
# center_info -> 77 centros. Dimensiones: ciudad, región, tipo de centro y area de operación

train_raw  <- read_csv(file.path(RUTA_DATOS, "train.csv"), show_col_types = FALSE)
test_raw   <- read_csv(file.path(RUTA_DATOS, "test.csv"),  show_col_types = FALSE)
meals      <- read_csv(file.path(RUTA_DATOS, "meal_info.csv"), show_col_types = FALSE)
centros    <- read_csv(file.path(RUTA_DATOS, "fulfilment_center_info.csv"),
                       show_col_types = FALSE)

cat("train:", nrow(train_raw), "filas x", ncol(train_raw), "columnas\n")
cat("test :", nrow(test_raw),  "filas x", ncol(test_raw),  "columnas\n")
cat("platos:", nrow(meals), " | centros:", nrow(centros), "\n")

# -----------------------------------------------------------------------------
# 2. Variables derivadas y join con tablas de dimensiones
# -----------------------------------------------------------------------------
# descuento: (base_price - checkout_price) / base_price
# promocionado: indicador de que el plato tuvo email o destaque en home.

preparar <- function(df) {
  df |>
    left_join(meals,   by = "meal_id") |>
    left_join(centros, by = "center_id") |>
    mutate(
      semana       = a_yearweek(week),
      descuento    = (base_price - checkout_price) / base_price,
      recargo      = checkout_price > base_price,
      promocionado = as.integer(emailer_for_promotion == 1 | homepage_featured == 1),
      center_type  = factor(center_type),
      category     = factor(category),
      cuisine      = factor(cuisine)
    )
}

train <- preparar(train_raw)
test  <- preparar(test_raw)

cat("cobertura train: semanas", min(train$week), "-", max(train$week), "\n")
cat("cobertura test : semanas", min(test$week),  "-", max(test$week),  "\n")

# -----------------------------------------------------------------------------
# 3. Diagnóstico de la estructura de panel
# -----------------------------------------------------------------------------

# Análisis exploratorio de los datos
panel_diag <- train |>
  group_by(center_id, meal_id) |>
  summarise(
    n_semanas    = n(),
    primera      = min(week),
    ultima       = max(week),
    pedidos_tot  = sum(num_orders),
    .groups = "drop"
  ) |>
  mutate(
    span           = ultima - primera + 1,
    huecos_int     = span - n_semanas,
    alta_tardia    = primera > 1,
    baja_temprana  = ultima  < SEMANA_MAX,
    completa       = n_semanas == SEMANA_MAX
  )

# MEnsajes
cat("combos centro-plato:", nrow(panel_diag), "\n")
cat("  con historia completa (145 sem):", sum(panel_diag$completa), "\n")
cat("  con huecos internos:", sum(panel_diag$huecos_int > 0),
    "| semanas faltantes internas:", sum(panel_diag$huecos_int), "\n")
cat("  alta tardia:", sum(panel_diag$alta_tardia),
    "| baja temprana:", sum(panel_diag$baja_temprana), "\n")
cat("mínimo de num_orders en todo el dataset:", min(train$num_orders),
    " (nunca hay ceros registrados)\n")

# -----------------------------------------------------------------------------
# 4. NIVEL 1 - Demanda total semanal
# -----------------------------------------------------------------------------
#   descuento_medio -> descuento promedio del menu ofrecido esa semana
#   share_emailer   -> proporción de items con campaña de email
#   share_home      -> proporcion de items destacados en homepage
#   n_items         -> amplitud del mení activo 

agregar_total <- function(df, con_y = TRUE) {
  out <- df |>
    group_by(week, semana) |>
    summarise(
      descuento_medio = mean(descuento),
      share_emailer   = mean(emailer_for_promotion),
      share_home      = mean(homepage_featured),
      n_items         = n(),
      precio_medio    = mean(checkout_price),
      pedidos         = if (con_y) sum(num_orders) else NA_real_,
      .groups = "drop"
    )
  as_tsibble(out, index = semana)
}

ts_total      <- agregar_total(train, con_y = TRUE)
ts_total_fut  <- agregar_total(test,  con_y = FALSE) |> select(-pedidos)
# Mensajes resultados
cat("\nNivel 1 - serie total:", nrow(ts_total), "semanas |",
    "regular:", is_regular(ts_total), "| con huecos:", has_gaps(ts_total)$.gaps, "\n")
cat("  pedidos semanales: min", fmt_miles(min(ts_total$pedidos)),
    "| mediana", fmt_miles(median(ts_total$pedidos)),
    "| max", fmt_miles(max(ts_total$pedidos)), "\n")

# -----------------------------------------------------------------------------
# 5. NIVEL 2 - Demanda semanal por categoría de plato
# -----------------------------------------------------------------------------


agregar_categoria <- function(df, con_y = TRUE) {
  out <- df |>
    group_by(category, week, semana) |>
    summarise(
      descuento_medio = mean(descuento),
      share_emailer   = mean(emailer_for_promotion),
      share_home      = mean(homepage_featured),
      n_items         = n(),
      pedidos         = if (con_y) sum(num_orders) else NA_real_,
      .groups = "drop"
    )
  as_tsibble(out, index = semana, key = category)
}

ts_categoria     <- agregar_categoria(train, con_y = TRUE)
ts_categoria_fut <- agregar_categoria(test,  con_y = FALSE) |> select(-pedidos)

# No todas las cateogórias existen todas las semanas
cobertura_cat <- ts_categoria |>
  as_tibble() |>
  group_by(category) |>
  summarise(
    n_semanas = n(),
    primera   = min(week),
    ultima    = max(week),
    .groups   = "drop"
  ) |>
  mutate(
    huecos_int = (ultima - primera + 1) - n_semanas,
    completa   = n_semanas == SEMANA_MAX
  ) |>
  arrange(n_semanas)

print(cobertura_cat)

categorias_completas <- cobertura_cat |> filter(completa) |> pull(category) |> as.character()
# Mensajes resultados
cat("\nNivel 2 - series por categoría:", n_distinct(ts_categoria$category), "series |",
    "con historia completa:", length(categorias_completas), "\n")
cat("  excluidas del modelado en batch:",
    paste(setdiff(levels(droplevels(ts_categoria$category)), categorias_completas),
          collapse = ", "), "\n")

# Subconjunto que va al modelado: solo categorias con las 145 semanas, para que
# la comparación de modelos sea entre familias con la misma ventana temporal
ts_categoria_mod <- ts_categoria |> filter(category %in% categorias_completas)

# -----------------------------------------------------------------------------
# 6. NIVEL 3 - Combinaciones centro-plato principales
# -----------------------------------------------------------------------------
# Seleccionamos las 6 combinaciones con más pedidos acumulados entre las que
# tienen historia completa. Mejores para ARIMAX

combos_elegidos <- panel_diag |>
  filter(completa) |>
  slice_max(pedidos_tot, n = 6) |>
  mutate(combo = paste0("C", center_id, "-M", meal_id)) |>
  select(center_id, meal_id, combo, pedidos_tot)

print(combos_elegidos |> left_join(meals, by = "meal_id") |>
        left_join(centros |> select(center_id, center_type, op_area), by = "center_id"))

armar_combos <- function(df, con_y = TRUE) {
  out <- df |>
    inner_join(combos_elegidos |> select(center_id, meal_id, combo),
               by = c("center_id", "meal_id")) |>
    transmute(
      combo, week, semana,
      descuento             = descuento,
      checkout_price, base_price,
      emailer_for_promotion = as.numeric(emailer_for_promotion),
      homepage_featured     = as.numeric(homepage_featured),
      pedidos               = if (con_y) as.numeric(num_orders) else NA_real_
    )
  as_tsibble(out, index = semana, key = combo)
}

ts_combos     <- armar_combos(train, con_y = TRUE)
ts_combos_fut <- armar_combos(test,  con_y = FALSE) |> select(-pedidos)
#Mensajes
cat("\nNivel 3 - combos seleccionados:", n_distinct(ts_combos$combo), "series |",
    "con huecos:", any(has_gaps(ts_combos)$.gaps), "\n")
cat("  combos en test para proyección final:", n_distinct(ts_combos_fut$combo), "\n")

# -----------------------------------------------------------------------------
# 7. Datos conservados
# -----------------------------------------------------------------------------
saveRDS(
  list(
    train = train, test = test, meals = meals, centros = centros,
    panel_diag = panel_diag, combos_elegidos = combos_elegidos,
    ts_total = ts_total, ts_total_fut = ts_total_fut,
    ts_categoria = ts_categoria, ts_categoria_fut = ts_categoria_fut,
    ts_categoria_mod = ts_categoria_mod, cobertura_cat = cobertura_cat,
    categorias_completas = categorias_completas,
    ts_combos = ts_combos, ts_combos_fut = ts_combos_fut
  ),
  file.path(RUTA_RDS, "datos.rds")
)
message("datos.rds guardado en ", RUTA_RDS)
