# =============================================================================
# 03_modelos.R - PARTE 3 (nivel 1): modelado de la demanda total semanal
#
# Estrategia:
#   1. Estimar una bateria de modelos sobre las semanas 1-135
#   2. Medir el error out of sample en las semanas 136-145 (backtesting)
#   3. Repetir la medición con cross-validation de origen móvil.
#   4. Diagnosticar residuos e interpretar los coeficientes del ARIMAX.
# =============================================================================

source("R/00_setup.R")

message("\n== 03_modelos.R ==")
d <- readRDS(file.path(RUTA_RDS, "datos.rds"))
ts_total     <- d$ts_total
ts_total_fut <- d$ts_total_fut

# -----------------------------------------------------------------------------
# 1. Variable dummy para la semana 62
# -----------------------------------------------------------------------------

ts_total     <- ts_total     |> mutate(sem62 = as.numeric(week == 62))
ts_total_fut <- ts_total_fut |> mutate(sem62 = 0)

# -----------------------------------------------------------------------------
# 2. Partición
# -----------------------------------------------------------------------------

train_ts <- ts_total |> filter(week <= SEMANA_CORTE)
valid_ts <- ts_total |> filter(week >  SEMANA_CORTE)
# Mensajes
cat("Entrenamiento: semanas", min(train_ts$week), "-", max(train_ts$week),
    "(", nrow(train_ts), "obs)\n")
cat("Validación   : semanas", min(valid_ts$week), "-", max(valid_ts$week),
    "(", nrow(valid_ts), "obs)\n")

# -----------------------------------------------------------------------------
# 3. Batería de modelos
# -----------------------------------------------------------------------------
# Se trabaja con log(pedidos). fable des-transforma automaticamente con correccion de sesgo
#
#   naive     -> pronóstico ingenuo (para comparar)
#   snaive    -> ingenuo estacional: misma semana del año anterior
#   ets       -> suavizado exponencial automático
#   arima     -> ARIMA con selección automática del orden de integracion
#   arima_d0  -> ARIMA forzando d = 0 (serie estacionaria en niveles)
#   sarima    -> permite componente estacional de periodo 52
#   arimax    -> ARIMA estacionario con regresores

# estimación modelos
fit <- train_ts |>
  model(
    naive    = NAIVE(pedidos),
    snaive   = SNAIVE(pedidos),
    ets      = ETS(log(pedidos)),
    arima    = ARIMA(log(pedidos) ~ PDQ(0, 0, 0)),
    arima_d0 = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)),
    sarima   = ARIMA(log(pedidos) ~ PDQ(period = 52)),
    arimax   = ARIMA(log(pedidos) ~ descuento_medio + share_emailer + share_home +
                       n_items + sem62 + pdq(d = 0) + PDQ(0, 0, 0))
  )

especificaciones <- tibble(
  modelo = c("arima", "arima_d0", "sarima", "arimax"),
  especificacion = c(format(fit$arima[[1]]),  format(fit$arima_d0[[1]]),
                     format(fit$sarima[[1]]), format(fit$arimax[[1]]))
)
guardar_tabla(especificaciones, "t09_especificaciones")
cat("\nEspecificación elegida por cada modelo:\n")
print(especificaciones)

# resumen modelos
resumen_fit <- glance(fit) |>
  select(.model, sigma2, log_lik, AIC, AICc, BIC) |>
  arrange(AICc)
guardar_tabla(resumen_fit, "t10_glance_modelos")
cat("\nAjuste dentro de muestra (menor AICc = mejor):\n")
print(resumen_fit)


# -----------------------------------------------------------------------------
# 4. Backtesting (semanas 136-145)
# -----------------------------------------------------------------------------
# Los regresores del ARIMAX son conocidos en la ventana de validación\

# Proyección:
fc_valid <- fit |> forecast(new_data = valid_ts)
# Desempeño
precision_ventana <- accuracy(fc_valid, ts_total) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)
guardar_tabla(precision_ventana, "t11_precision_ventana")
cat("\nPrecision fuera de muestra en la ventana 136-145:\n")
print(precision_ventana)
# Figura 15
fig15 <- fc_valid |>
  filter(.model %in% c("naive", "snaive", "ets", "arima", "arima_d0", "arimax")) |>
  autoplot(ts_total |> filter(week >= 100), level = NULL, linewidth = 0.85) +
  geom_line(data = ts_total |> filter(week >= 100),
            aes(x = semana, y = pedidos), color = "grey25", linewidth = 0.9,
            inherit.aes = FALSE) +
  scale_y_continuous(labels = fmt_miles) +
  scale_color_manual(values = unname(PALETA[c("azul", "naranja", "verde",
                                              "rojo", "violeta", "gris")])) +
  eje_semana() +
  labs(
    title    = "Proyección a 10 semanas contra los valores efectivamente observados",
    subtitle = "Línea gris: demanda real. Cada color es un modelo estimado solo con datos hasta la semana 135",
    y = "Pedidos", color = "Modelo",
    caption = "Backtesting: ningún modelo vio las semanas 136-145 al estimarse."
  )
guardar_fig(fig15, "fig15_backtest_ventana")

# -----------------------------------------------------------------------------
# 5. Validación cruzada móvil
# -----------------------------------------------------------------------------
# k = 5

cv_data <- ts_total |>
  stretch_tsibble(.init = 90, .step = 5) |>
  filter(.id < max(.id))

cat("\nValidación cruzada de origen móvil:", n_distinct(cv_data$.id), "ventanas\n")

fit_cv <- cv_data |>
  model(
    naive    = NAIVE(pedidos),
    snaive   = SNAIVE(pedidos),
    ets      = ETS(log(pedidos)),
    arima    = ARIMA(log(pedidos) ~ PDQ(0, 0, 0)),
    arima_d0 = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)),
    arimax   = ARIMA(log(pedidos) ~ descuento_medio + share_emailer + share_home +
                       n_items + sem62 + pdq(d = 0) + PDQ(0, 0, 0))
  )

# datos futuros (nuevos)
nuevos_cv <- cv_data |>
  as_tibble() |>
  group_by(.id) |>
  summarise(ultima = max(week), .groups = "drop") |>
  mutate(week = map(ultima, ~ seq(.x + 1, min(.x + H_PROYECCION, SEMANA_MAX)))) |>
  unnest(week) |>
  mutate(h = week - ultima) |>
  left_join(ts_total |> as_tibble(), by = "week") |>
  as_tsibble(index = semana, key = .id)

# Proyección
fc_cv <- fit_cv |> forecast(new_data = nuevos_cv)
# Desempeño
precision_cv <- fc_cv |>
  accuracy(ts_total, by = ".model") |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)
guardar_tabla(precision_cv, "t12_precision_cv")
cat("\nPrecision promedio en validación cruzada de origen móvil:\n")
print(precision_cv)
# Comparación
modelo_ganador <- precision_cv$.model[1]
cat("\nModelo elegido (menor RMSE en CV):", modelo_ganador, "\n")
cat("Posición de naive: ", which(precision_cv$.model == "naive"), " de ",
    nrow(precision_cv), " en CV, contra ",
    which(precision_ventana$.model == "naive"), " de ", nrow(precision_ventana),
    " en la ventana única.\n", sep = "")

# Comparación directa de los dos criterios
comparacion_criterios <- precision_ventana |>
  select(.model, RMSE_ventana = RMSE) |>
  full_join(precision_cv |> select(.model, RMSE_cv = RMSE), by = ".model") |>
  arrange(RMSE_cv)
guardar_tabla(comparacion_criterios, "t13_comparacion_criterios")
# Figura 16 - Compraración
fig16 <- comparacion_criterios |>
  filter(!is.na(RMSE_cv)) |>
  pivot_longer(-.model, names_to = "criterio", values_to = "rmse") |>
  mutate(criterio = recode(criterio,
                           RMSE_ventana = "Una sola ventana (136-145)",
                           RMSE_cv      = "Validación cruzada de origen móvil")) |>
  ggplot(aes(x = rmse, y = fct_reorder(.model, -rmse), fill = criterio)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66, alpha = 0.9) +
  scale_x_continuous(labels = fmt_miles) +
  scale_fill_manual(values = unname(PALETA[c("gris", "azul")])) +
  labs(
    title    = "Un solo backtest puede elegir el modelo equivocado",
    subtitle = "RMSE del mismo conjunto de modelos bajo dos esquemas de evaluación",
    x = "RMSE (pedidos semanales)", y = NULL, fill = NULL,
    caption = paste0("naive gana en la ventana única y queda ",
                     which(precision_cv$.model == "naive"),
                     "o de ", nrow(precision_cv), " en validación cruzada.")
  )
guardar_fig(fig16, "fig16_ventana_vs_cv")

# Cómo se degrada la precisión a medida que se proyecta mas lejos
precision_h <- fc_cv |>
  as_tibble() |>
  select(.model, .id, semana, .mean) |>
  left_join(nuevos_cv |> as_tibble() |> select(.id, semana, h), by = c(".id", "semana")) |>
  left_join(ts_total |> as_tibble() |> select(semana, real = pedidos), by = "semana") |>
  group_by(.model, h) |>
  summarise(RMSE = sqrt(mean((real - .mean)^2)), .groups = "drop")
guardar_tabla(precision_h, "t14_precision_por_horizonte")
# Figura 17 - RMSE en horizonte de proyección por modelo
fig17 <- precision_h |>
  ggplot(aes(x = h, y = RMSE, color = .model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = fmt_miles) +
  scale_x_continuous(breaks = 1:H_PROYECCION) +
  scale_color_manual(values = unname(PALETA[c("azul", "naranja", "verde",
                                              "rojo", "violeta", "gris")])) +
  labs(
    title    = "Error de proyección según cuán lejos se proyecta",
    subtitle = paste0("RMSE promedio sobre ", n_distinct(cv_data$.id),
                      " ventanas de validación cruzada de origen móvil"),
    x = "Horizonte de proyección (semanas)", y = "RMSE",
    color = "Modelo",
    caption = "El error no crece, principalmente porque la serie no tiene tendencia y vuelve rápido a su media."
  )
guardar_fig(fig17, "fig17_cv_horizonte")

# -----------------------------------------------------------------------------
# 6. Diagnóstico de residuos
# -----------------------------------------------------------------------------
# Análisis residual ARIMAX
res <- fit |> select(arimax) |> augment()
#L-JUNG BOX
lb <- res |> features(.innov, ljung_box, lag = 24, dof = 6)
guardar_tabla(lb, "t15_ljung_box")
cat("\nLjung-Box sobre los residuos del ARIMAX (H0: no hay autocorrelación):\n")
print(lb)

lim_res <- 1.96 / sqrt(nrow(train_ts))
# Figura 18 - residuos ARIMAX
fig18 <- res |>
  ggplot(aes(x = semana, y = .innov)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = PALETA[["azul"]], linewidth = 0.6) +
  eje_semana() +
  labs(title    = "Residuos del ARIMAX en el tiempo",
       subtitle = "Innovaciones en escala logarítmica",
       y = "Residuo")
guardar_fig(fig18, "fig18_residuos_serie", alto = 3.6)
# Figura 19 - ACF ARIMAX
fig19 <- res |>
  ACF(.innov, lag_max = 30) |>
  as_tibble() |>
  mutate(lag_num = as.numeric(lag)) |>
  ggplot(aes(x = lag_num, y = acf)) +
  geom_hline(yintercept = c(-lim_res, lim_res), color = PALETA[["rojo"]],
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_segment(aes(xend = lag_num, yend = 0), color = PALETA[["azul"]], linewidth = 0.7) +
  labs(title    = "Autocorrelación de los residuos del ARIMAX",
       subtitle = paste0("Ljung-Box con 24 rezagos: p = ", round(lb$lb_pvalue, 3),
                         ifelse(lb$lb_pvalue > 0.05,
                                " (no se rechaza ruido blanco)",
                                " (queda estructura sin modelar)")),
       x = "Rezago (semanas)", y = "ACF")
guardar_fig(fig19, "fig19_residuos_acf", alto = 3.6)
# Figura 20 - Distribución resiudos ARIMAX
fig20 <- res |>
  ggplot(aes(x = .innov)) +
  geom_histogram(bins = 25, fill = PALETA[["azul"]], color = "white", alpha = 0.9) +
  labs(title = "Distribución de los residuos del ARIMAX",
       subtitle = "Aproximadamente simétrica y centrada en cero",
       x = "Residuo", y = "Frecuencia")
guardar_fig(fig20, "fig20_residuos_hist", alto = 3.6)

# -----------------------------------------------------------------------------
# 7. Coeficientes del ARIMAX - interpreación económica
# -----------------------------------------------------------------------------
# La variable dependiente esta en logaritmos: un coeficiente beta sobre un
# regresor x significa que aumentar x en una unidad multiplica los pedidos por
# exp(beta). Como los regresores son proporciones (entre 0 y 1), una unidad
# implica un cambio enorme;

coef_arimax <- fit |>
  select(arimax) |>
  tidy() |>
  mutate(
    efecto_pct = if_else(term %in% c("descuento_medio", "share_emailer",
                                     "share_home", "n_items", "sem62"),
                         100 * (exp(estimate) - 1), NA_real_),
    signif = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                       p.value < 0.10 ~ "*", TRUE ~ "")
  ) |>
  select(term, estimate, std.error, statistic, p.value, efecto_pct, signif)
guardar_tabla(coef_arimax, "t16_coeficientes_arimax")
cat("\nCoeficientes del ARIMAX:\n")
print(coef_arimax, n = 30)

# Efecto de moverse del percentil 25 al percentil 75 de cada regresor
q <- ts_total |>
  as_tibble() |>
  summarise(across(c(descuento_medio, share_emailer, share_home),
                   list(p25 = ~ quantile(.x, .25), p75 = ~ quantile(.x, .75))))

efectos <- coef_arimax |>
  filter(term %in% c("descuento_medio", "share_emailer", "share_home")) |>
  mutate(
    delta_p25_p75 = c(q$descuento_medio_p75 - q$descuento_medio_p25,
                      q$share_emailer_p75  - q$share_emailer_p25,
                      q$share_home_p75     - q$share_home_p25),
    efecto_pct = 100 * (exp(estimate * delta_p25_p75) - 1)
  ) |>
  select(term, estimate, delta_p25_p75, efecto_pct, p.value)
guardar_tabla(efectos, "t17_efectos_economicos")
cat("\nEfecto de moverse del percentil 25 al 75 de cada palanca:\n")
print(efectos)
# Figura 21 - efectos descuentos, promociones y otros
fig21 <- coef_arimax |>
  filter(term %in% c("descuento_medio", "share_emailer", "share_home", "sem62")) |>
  mutate(
    term = recode(term,
                  descuento_medio = "Descuento medio del menú",
                  share_emailer   = "Proporción de items con email",
                  share_home      = "Proporción de items en home",
                  sem62           = "Shock sistémico de la semana 62"),
    li = estimate - 1.96 * std.error,
    ls = estimate + 1.96 * std.error
  ) |>
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0.16,
                color = PALETA[["azul"]], linewidth = 0.8, orientation = "y") +
  geom_point(size = 3.2, color = PALETA[["azul"]]) +
  labs(
    title    = "Efecto estimado de cada palanca comercial sobre la demanda total",
    subtitle = "Coeficientes del ARIMAX sobre log(pedidos), con intervalos al 95%",
    x = "Coeficiente (efecto semilogaritmico)", y = NULL,
    caption = "El coeficiente de la semana 62 implica una caida de 48% respecto de lo que el modelo esperaba para esa semana."
  )
guardar_fig(fig21, "fig21_coeficientes")

# -----------------------------------------------------------------------------
# 8. Datos conservados
# -----------------------------------------------------------------------------
saveRDS(
  list(fit = fit, fc_valid = fc_valid,
       precision_ventana = precision_ventana, precision_cv = precision_cv,
       precision_h = precision_h, comparacion_criterios = comparacion_criterios,
       especificaciones = especificaciones, resumen_fit = resumen_fit,
       modelo_ganador = modelo_ganador, coef_arimax = coef_arimax,
       efectos = efectos, lb = lb,
       ts_total = ts_total, ts_total_fut = ts_total_fut,
       train_ts = train_ts, valid_ts = valid_ts),
  file.path(RUTA_RDS, "modelos_total.rds")
)
message("\n03_modelos.R terminado")
