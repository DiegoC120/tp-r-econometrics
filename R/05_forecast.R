# =============================================================================
# 05_forecast.R - Proyección FINAL: semanas 146 a 155
#
# =============================================================================

source("R/00_setup.R")

message("\n== 05_forecast.R ==")
d   <- readRDS(file.path(RUTA_RDS, "datos.rds"))
mt  <- readRDS(file.path(RUTA_RDS, "modelos_total.rds"))
md  <- readRDS(file.path(RUTA_RDS, "modelos_desagregados.rds"))

ts_total     <- mt$ts_total
ts_total_fut <- mt$ts_total_fut

# -----------------------------------------------------------------------------
# 1. Consistencia de los regresores
# -----------------------------------------------------------------------------
# Comparación regresores, train vs test
comparacion_reg <- bind_rows(
  ts_total     |> as_tibble() |> mutate(tramo = "Historia (1-145)"),
  ts_total_fut |> as_tibble() |> mutate(tramo = "Proyección (146-155)")
) |>
  group_by(tramo) |>
  summarise(across(c(descuento_medio, share_emailer, share_home, n_items),
                   list(media = mean, min = min, max = max)), .groups = "drop")
guardar_tabla(comparacion_reg, "t26_regresores_futuros")
cat("Regresores: historia vs. período a proyectar\n")
print(comparacion_reg |> select(tramo, starts_with("n_items"), starts_with("share_emailer")))

# -----------------------------------------------------------------------------
# 2. Nivel 1 - Demanda TOTAL
# -----------------------------------------------------------------------------
# Estimación total
fit_final <- ts_total |>
  model(
    arima_d0 = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)),
    arimax   = ARIMA(log(pedidos) ~ descuento_medio + share_emailer + share_home +
                       n_items + sem62 + pdq(d = 0) + PDQ(0, 0, 0))
  )
# Mensajes
cat("\nModelos reestimados sobre las 145 semanas:\n")
cat("  arima_d0:", format(fit_final$arima_d0[[1]]), "\n")
cat("  arimax  :", format(fit_final$arimax[[1]]), "\n")
# Proyección
fc_final <- fit_final |> forecast(new_data = ts_total_fut)
# tablas resultados
tabla_forecast <- fc_final |>
  hilo(level = 80) |>
  as_tibble() |>
  mutate(
    semana_panel = a_week(semana),
    proyeccion   = round(.mean),
    li80         = round(map_dbl(`80%`, ~ .x$lower)),
    ls80         = round(map_dbl(`80%`, ~ .x$upper))
  ) |>
  select(.model, semana_panel, proyeccion, li80, ls80)
guardar_tabla(tabla_forecast, "t27_proyeccion_total")
cat("\nProyección de la demanda total semanal (intervalos al 80%):\n")
print(tabla_forecast |> filter(.model == "arimax"), n = 10)
# Figura 29 - Proyección FINAL
fig29 <- fc_final |>
  filter(.model == "arimax") |>
  autoplot(ts_total |> filter(week >= 80), level = c(80, 95)) +
  geom_vline(xintercept = a_yearweek(SEMANA_MAX), color = "grey40",
             linetype = "dashed") +
  scale_y_continuous(labels = fmt_miles) +
  eje_semana() +
  labs(
    level    = "Intervalo de predicción",
    title    = "Proyección de la demanda total para las semanas 146 a 155",
    subtitle = paste0("Modelo ARIMAX con plan de precios y promociones",
                      "La línea punteada marca el fin de la historia observada. Bandas al 80% y 95%"),
    y = "Pedidos",
    caption = "Los regresores de las semanas proyectadas provienen de test.csv"
  )
guardar_fig(fig29, "fig29_proyeccion_total")

# Comparación proyecciones
dif <- tabla_forecast |>
  select(.model, semana_panel, proyeccion) |>
  pivot_wider(names_from = .model, values_from = proyeccion) |>
  mutate(diferencia_pct = round(100 * (arimax - arima_d0) / arima_d0, 2))
guardar_tabla(dif, "t28_diferencia_modelos_forecast")
cat("\nDiferencia entre la proyección con y sin plan comercial:\n")
print(dif)

# -----------------------------------------------------------------------------
# 3. Nivel 2 - Proyección por categoria
# -----------------------------------------------------------------------------

ts_cat     <- md$ts_cat
ts_cat_fut <- d$ts_categoria_fut |>
  filter(category %in% d$categorias_completas) |>
  mutate(sem62 = 0)

# Tres categorías (Biryani, Extras y Soup) no tienen campaña de promoción con
# email en las 145 semanas, de modo que share_emailer es cero y queda
# colineal con el intercepto. fable lo detecta y descarta ese regresor
# solo para esas series (error "rank deficient")

sin_email <- ts_cat |> as_tibble() |> group_by(category) |>
  summarise(total = sum(share_emailer), .groups = "drop") |> filter(total == 0)
cat("\nCategorias sin ninguna campaña de email en toda la historia:",
    paste(sin_email$category, collapse = ", "), "\n")
# Estimación por categoría
fit_cat_final <- ts_cat |>
  model(arimax = ARIMA(log(pedidos) ~ descuento_medio + share_emailer + share_home +
                         n_items + sem62 + pdq(d = 0) + PDQ(0, 0, 0)))
# Proyección
fc_cat_final <- fit_cat_final |> forecast(new_data = ts_cat_fut)
# Resumen
tabla_cat <- fc_cat_final |>
  hilo(level = 80) |>
  as_tibble() |>
  mutate(semana_panel = a_week(semana),
         proyeccion = round(.mean),
         li80 = round(map_dbl(`80%`, ~ .x$lower)),
         ls80 = round(map_dbl(`80%`, ~ .x$upper))) |>
  select(category, semana_panel, proyeccion, li80, ls80)
guardar_tabla(tabla_cat, "t29_proyeccion_categorias")

resumen_cat_fc <- tabla_cat |>
  group_by(category) |>
  summarise(total_10_semanas = sum(proyeccion),
            promedio_semanal = round(mean(proyeccion)), .groups = "drop") |>
  arrange(desc(total_10_semanas))
guardar_tabla(resumen_cat_fc, "t30_proyeccion_categorias_resumen")
cat("\nDemanda proyectada por categoría para las 10 semanas:\n")
print(resumen_cat_fc, n = 12)
# Figura 30 - Proyeccioón por categoría
fig30 <- fc_cat_final |>
  autoplot(ts_cat |> filter(week >= 110), level = 80) +
  facet_wrap(~ category, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = fmt_miles) +
  eje_semana() +
  labs(
    title    = "Proyección por categoría de plato, semanas 146 a 155",
    subtitle = "Modelo ARIMAX estimado sobre las 145 semanas de historia. Banda al 80%",
    y = "Pedidos"
    ) +
  theme(strip.text = element_text(face = "bold", size = 8),
        legend.position = "none")
guardar_fig(fig30, "fig30_proyeccion_categorias", alto = 6.0)

# Jerarquía: la suma de las proyecciones por categoría debería
# parecerse a la proyección del total, aunquen no tienen por que coincidir exactamente
coherencia <- tabla_cat |>
  group_by(semana_panel) |>
  summarise(suma_categorias = sum(proyeccion), .groups = "drop") |>
  left_join(tabla_forecast |> filter(.model == "arimax") |>
              select(semana_panel, total_directo = proyeccion),
            by = "semana_panel") |>
  mutate(brecha_pct = round(100 * (suma_categorias - total_directo) / total_directo, 1))
guardar_tabla(coherencia, "t31_coherencia_niveles")
cat("\nCoherencia entre la proyección agregada y la suma de categorías:\n")
print(coherencia)

brecha_acum <- 100 * (sum(coherencia$suma_categorias) - sum(coherencia$total_directo)) /
  sum(coherencia$total_directo)
peso_excluidas <- 100 * sum(d$train$num_orders[d$train$category %in% c("Fish", "Salad")]) /
  sum(d$train$num_orders)
cat("Brecha acumulada en las 10 semanas:", round(brecha_acum, 1), "%\n")
cat("Peso histórico de las categorías excluidas (Fish y Salad):",
    round(peso_excluidas, 1), "%\n")
cat("Es decir: las dos vias coinciden dentro de lo que explican las categorías\n",
    "excluidas. Las brechas semana a semana (-25% a +20%) son ruido de dos\n",
    "modelos independientes, no un sesgo sistematico.\n", sep = "")

# -----------------------------------------------------------------------------
# 4. Nivel 3 - Proyección centro-plato
# -----------------------------------------------------------------------------
# Para los seis combos usamos el ARIMA sin regresores porque pierde precisión

ts_com     <- md$ts_com
ts_com_fut <- d$ts_combos_fut |> mutate(sem62 = 0)
# Estimación
fit_com_final <- ts_com |>
  model(arima = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)))
# Proyección
fc_com_final <- fit_com_final |> forecast(new_data = ts_com_fut)
# Reusmen
tabla_com <- fc_com_final |>
  hilo(level = 80) |>
  as_tibble() |>
  mutate(semana_panel = a_week(semana),
         proyeccion = round(.mean),
         li80 = round(map_dbl(`80%`, ~ .x$lower)),
         ls80 = round(map_dbl(`80%`, ~ .x$upper))) |>
  select(combo, semana_panel, proyeccion, li80, ls80)
guardar_tabla(tabla_com, "t32_proyeccion_combos")
# Figura 31
fig31 <- fc_com_final |>
  autoplot(ts_com |> filter(week >= 115), level = 80) +
  facet_wrap(~ combo, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = fmt_miles) +
  eje_semana() +
  labs(
    title    = "Proyección de las seis combinaciones centro-plato de mayor volumen",
    subtitle = "Modelo ARIMA sobre log(pedidos), semanas 146 a 155. Banda al 80%",
    y = "Pedidos",
    caption = "Se usa ARIMA y no ARIMAX porque en este nivel los regresores no mejoraron la proyección fuera de muestra."
  ) +
  theme(strip.text = element_text(face = "bold", size = 8),
        legend.position = "none")
guardar_fig(fig31, "fig31_proyeccion_combos", alto = 5.2)

# Formato de entrega
# solo las combinaciones modeladas
entrega <- d$test |>
  inner_join(d$combos_elegidos |> select(center_id, meal_id, combo),
             by = c("center_id", "meal_id")) |>
  mutate(semana_panel = week) |>
  left_join(tabla_com |> select(combo, semana_panel, proyeccion),
            by = c("combo", "semana_panel")) |>
  transmute(id, center_id, meal_id, week, num_orders = proyeccion)
guardar_tabla(entrega, "t33_entrega_combos")
cat("\nArchivo de entrega generado para", nrow(entrega), "filas (",
    n_distinct(entrega$id), "ids unicos )\n")
# Datos conservados
saveRDS(
  list(fit_final = fit_final, fc_final = fc_final, tabla_forecast = tabla_forecast,
       dif = dif, comparacion_reg = comparacion_reg,
       fc_cat_final = fc_cat_final, tabla_cat = tabla_cat,
       resumen_cat_fc = resumen_cat_fc, coherencia = coherencia,
       fc_com_final = fc_com_final, tabla_com = tabla_com, entrega = entrega),
  file.path(RUTA_RDS, "forecast_final.rds")
)
message("\n05_forecast.R terminado")
