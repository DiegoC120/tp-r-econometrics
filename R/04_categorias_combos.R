# =============================================================================
# 04_categorias_combos.R - PARTE 3 (niveles 2 y 3)
#
#   Nivel 2: 12 categorías de plato con serie completa
#   Nivel 3: 6 combinaciones centro-plato de mayor volumen
#
# Hipótesis: la promoción debería mejorar la proyección mientras más
# desagregado es el nivel
#
# =============================================================================

source("R/00_setup.R")

message("\n== 04_categorias_combos.R ==")
d <- readRDS(file.path(RUTA_RDS, "datos.rds"))

# =============================================================================
# NIVEL 2 - Demanda por categoría de plato
# =============================================================================

ts_cat <- d$ts_categoria_mod |> mutate(sem62 = as.numeric(week == 62))

cat("Categorías modeladas:", n_distinct(ts_cat$category),
    "| excluidas por historia incompleta: Fish, Salad\n")

train_cat <- ts_cat |> filter(week <= SEMANA_CORTE)
valid_cat <- ts_cat |> filter(week >  SEMANA_CORTE)

# Estimación de los 4 modelos sobre todas las series
fit_cat <- train_cat |>
  model(
    snaive = SNAIVE(pedidos),
    ets    = ETS(log(pedidos)),
    arima  = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)),
    arimax = ARIMA(log(pedidos) ~ descuento_medio + share_emailer + share_home +
                     n_items + sem62 + pdq(d = 0) + PDQ(0, 0, 0))
  )

cat("Modelos estimados:", nrow(fit_cat) * 4, "(", nrow(fit_cat), "series x 4 modelos )\n")
# Proyección
fc_cat <- fit_cat |> forecast(new_data = valid_cat)

# Precisión por categoria y por modelo. MAPE y MASE son comaprables
precision_cat <- accuracy(fc_cat, ts_cat) |>
  select(category, .model, RMSE, MAE, MAPE, MASE)
guardar_tabla(precision_cat, "t18_precision_categorias")

resumen_cat <- precision_cat |>
  group_by(.model) |>
  summarise(MASE_promedio = mean(MASE), MAPE_promedio = mean(MAPE),
            veces_mejor = NA_integer_, .groups = "drop")

mejor_por_cat <- precision_cat |>
  group_by(category) |>
  slice_min(MASE, n = 1) |>
  ungroup() |>
  count(.model, name = "veces_mejor")

resumen_cat <- resumen_cat |>
  select(-veces_mejor) |>
  left_join(mejor_por_cat, by = ".model") |>
  mutate(veces_mejor = replace_na(veces_mejor, 0L)) |>
  arrange(MASE_promedio)
guardar_tabla(resumen_cat, "t19_resumen_categorias")
cat("\nDesempeno por modelo sobre las 12 categorías (ventana 136-145):\n")
print(resumen_cat)
# Figura 22 - Precisión modelos sobre series desagregadas
fig22 <- precision_cat |>
  mutate(category = fct_reorder(category, MASE, .fun = min)) |>
  ggplot(aes(x = MASE, y = category, color = .model)) +
  geom_vline(xintercept = 1, color = "grey50", linetype = "dashed") +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = unname(PALETA[c("azul", "naranja", "verde", "gris")])) +
  labs(
    title    = "Error de proyección por categoría de plato",
    subtitle = "MASE en la ventana 136-145. Valores menores a 1 superan al modelo ingenuo",
    x = "MASE", y = NULL, color = "Modelo",
    caption = "La línea punteada marca MASE = 1: el nivel de un pronóstico naive"
  )
guardar_fig(fig22, "fig22_mase_categorias", alto = 5.6)

# Las cuatro categorías de mayor volumen, proyección vs real
top4 <- ts_cat |>
  as_tibble() |>
  group_by(category) |>
  summarise(v = sum(pedidos), .groups = "drop") |>
  slice_max(v, n = 4) |>
  pull(category)
# Figura 23 - proyección por categoría
fig23 <- fc_cat |>
  filter(category %in% top4, .model %in% c("arima", "arimax", "snaive")) |>
  autoplot(ts_cat |> filter(category %in% top4, week >= 110),
           level = NULL, linewidth = 0.8) +
  geom_line(data = ts_cat |> filter(category %in% top4, week >= 110),
            aes(x = semana, y = pedidos), color = "grey25", linewidth = 0.8,
            inherit.aes = FALSE) +
  facet_wrap(~ category, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = fmt_miles) +
  scale_color_manual(values = unname(PALETA[c("azul", "naranja", "gris")])) +
  eje_semana() +
  labs(
    title    = "Proyección por categoría: las cuatro familias de mayor volumen",
    subtitle = "Línea gris: demanda observada. Modelos estimados con datos hasta la semana 135",
    y = "Pedidos", color = "Modelo"
  ) +
  theme(strip.text = element_text(face = "bold"))
guardar_fig(fig23, "fig23_forecast_categorias", alto = 5.8)

# =============================================================================
# NIVEL 3 - Combinaciones centro-plato
# =============================================================================

ts_com <- d$ts_combos |> mutate(sem62 = as.numeric(week == 62))

train_com <- ts_com |> filter(week <= SEMANA_CORTE)
valid_com <- ts_com |> filter(week >  SEMANA_CORTE)

cat("\nCombos modelados:", n_distinct(ts_com$combo), "\n")
print(ts_com |> as_tibble() |> group_by(combo) |>
        summarise(pedidos_medios = round(mean(pedidos)),
                  semanas_con_email = sum(emailer_for_promotion),
                  semanas_en_home   = sum(homepage_featured),
                  descuento_medio   = round(mean(descuento), 3), .groups = "drop"))

fit_com <- train_com |>
  model(
    snaive = SNAIVE(pedidos),
    arima  = ARIMA(log(pedidos) ~ pdq(d = 0) + PDQ(0, 0, 0)),
    arimax = ARIMA(log(pedidos) ~ descuento + emailer_for_promotion +
                     homepage_featured + sem62 + pdq(d = 0) + PDQ(0, 0, 0))
  )

fc_com <- fit_com |> forecast(new_data = valid_com)

precision_com <- accuracy(fc_com, ts_com) |>
  select(combo, .model, RMSE, MAE, MAPE, MASE)
guardar_tabla(precision_com, "t20_precision_combos")

resumen_com <- precision_com |>
  group_by(.model) |>
  summarise(MASE_promedio = round(mean(MASE), 3),
            MAPE_promedio = round(mean(MAPE), 2), .groups = "drop") |>
  arrange(MASE_promedio)
guardar_tabla(resumen_com, "t21_resumen_combos")
cat("\nDesempeno por modelo sobre los 6 combos (ventana 136-145):\n")
print(resumen_com)

# Mejora relativa del ARIMAX sobre el ARIMA
mejora <- tibble(
  nivel = c("Red completa (1 serie)", "Categorías (12 series)",
            "Centro-plato (6 series)"),
  MASE_arima  = c(NA, resumen_cat$MASE_promedio[resumen_cat$.model == "arima"],
                  resumen_com$MASE_promedio[resumen_com$.model == "arima"]),
  MASE_arimax = c(NA, resumen_cat$MASE_promedio[resumen_cat$.model == "arimax"],
                  resumen_com$MASE_promedio[resumen_com$.model == "arimax"])
)
mod_tot <- readRDS(file.path(RUTA_RDS, "modelos_total.rds"))
mejora$MASE_arima[1]  <- mod_tot$precision_cv$MASE[mod_tot$precision_cv$.model == "arima_d0"]
mejora$MASE_arimax[1] <- mod_tot$precision_cv$MASE[mod_tot$precision_cv$.model == "arimax"]
mejora <- mejora |>
  mutate(mejora_pct = round(100 * (MASE_arima - MASE_arimax) / MASE_arima, 1))
guardar_tabla(mejora, "t22_mejora_por_nivel")
cat("\nGanancia del ARIMAX sobre el ARIMA según el nivel de agregación:\n")
print(mejora)
# Figura 24 - comparación modelos con y sin regresores
fig24 <- mejora |>
  pivot_longer(starts_with("MASE"), names_to = "modelo", values_to = "mase") |>
  mutate(modelo = recode(modelo, MASE_arima = "ARIMA (sin regresores)",
                         MASE_arimax = "ARIMAX (con promociones y precios)"),
         nivel = factor(nivel, levels = mejora$nivel)) |>
  ggplot(aes(x = nivel, y = mase, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, alpha = 0.9) +
  geom_text(aes(label = round(mase, 2)),
            position = position_dodge(width = 0.72), vjust = -0.5, size = 3.1) +
  scale_fill_manual(values = unname(PALETA[c("gris", "azul")])) +
  labs(
    title    = "La ganancia del ARIMAX no es monótona en el nivel de agregación",
    subtitle = "MASE fuera de muestra del mismo par de modelos en tres niveles de desagregación",
    x = NULL, y = "MASE (menor es mejor)", fill = NULL,
    caption = "El ARIMAX solo mejora en el nivel de categorías. La razón no es la agregación: es cuánto se mueven los regresores en la ventana proyectada (ver figura siguiente)."
  )
guardar_fig(fig24, "fig24_mejora_por_nivel", alto = 4.8)
# Figura 25
fig25 <- fc_com |>
  filter(.model %in% c("arima", "arimax")) |>
  autoplot(ts_com |> filter(week >= 110), level = NULL, linewidth = 0.8) +
  geom_line(data = ts_com |> filter(week >= 110),
            aes(x = semana, y = pedidos), color = "grey25", linewidth = 0.8,
            inherit.aes = FALSE) +
  facet_wrap(~ combo, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = fmt_miles) +
  scale_color_manual(values = unname(PALETA[c("gris", "azul")])) +
  eje_semana() +
  labs(
    title    = "Proyección de las seis combinaciones centro-plato de mayor volumen",
    subtitle = "Línea gris: pedidos observados. C13-M1885 (centro 13, plato 1885)",
    y = "Pedidos", color = "Modelo"
  ) +
  theme(strip.text = element_text(face = "bold", size = 8))
guardar_fig(fig25, "fig25_forecast_combos", alto = 5.4)

# Coeficientes promocionales combo por combo
coef_com <- fit_com |>
  select(arimax) |>
  tidy() |>
  filter(term %in% c("descuento", "emailer_for_promotion", "homepage_featured")) |>
  mutate(
    efecto_pct = 100 * (exp(estimate) - 1),
    term = recode(term,
                  descuento             = "Descuento de 100 p.p.",
                  emailer_for_promotion = "Campaña de email",
                  homepage_featured     = "Destaque en home")
  )
guardar_tabla(coef_com, "t23_coeficientes_combos")
cat("\nEfectos promocionales por combo (en % sobre los pedidos):\n")
print(coef_com |> select(combo, term, estimate, p.value, efecto_pct), n = 30)
# FIGURA 26 
fig26 <- coef_com |>
  mutate(li = 100 * (exp(estimate - 1.96 * std.error) - 1),
         ls = 100 * (exp(estimate + 1.96 * std.error) - 1)) |>
  ggplot(aes(x = efecto_pct, y = combo, color = term)) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0.2, linewidth = 0.7,
                orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = unname(PALETA[c("naranja", "azul", "verde")])) +
  labs(
    title    = "Efecto de cada acción comercial, por combinación",
    subtitle = "Cambio porcentual estimado en los pedidos semanales, con intervalos al 95%",
    x = "Efecto sobre los pedidos (%)", y = NULL, color = NULL,
    caption = "Estimado con ARIMAX sobre log(pedidos) para cada serie centro-plato por separado."
  )
guardar_fig(fig26, "fig26_coeficientes_combos", alto = 5.0)

# -----------------------------------------------------------------------------
# Análisis: por qué el ARIMAX pierde en el nivel centro-plato?
# -----------------------------------------------------------------------------

variacion_reg <- bind_rows(
  ts_cat |> as_tibble() |>
    transmute(nivel = "Categorías (12 series)",
              tramo = if_else(week <= SEMANA_CORTE, "Entrenamiento (1-135)",
                              "Validación (136-145)"),
              email = share_emailer, home = share_home, descuento = descuento_medio),
  ts_com |> as_tibble() |>
    transmute(nivel = "Centro-plato (6 series)",
              tramo = if_else(week <= SEMANA_CORTE, "Entrenamiento (1-135)",
                              "Validación (136-145)"),
              email = emailer_for_promotion, home = homepage_featured,
              descuento = descuento)
) |>
  group_by(nivel, tramo) |>
  summarise(across(c(email, home, descuento), sd), .groups = "drop")
guardar_tabla(variacion_reg, "t24_variacion_regresores")
cat("\nDesvio estándar de los regresores por nivel y tramo:\n")
print(variacion_reg)

cat("\nCampanas de email sobre los 6 combos en la ventana de validación:",
    sum(valid_com$emailer_for_promotion), "de", nrow(valid_com), "semanas-combo\n")
# Figura 27 - regresores en horizonte de proyeccón
fig27 <- variacion_reg |>
  pivot_longer(c(email, home, descuento), names_to = "regresor", values_to = "sd") |>
  mutate(regresor = recode(regresor, email = "Campaña de email",
                           home = "Destaque en home", descuento = "Descuento")) |>
  ggplot(aes(x = regresor, y = sd, fill = tramo)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, alpha = 0.9) +
  facet_wrap(~ nivel) +
  scale_fill_manual(values = unname(PALETA[c("gris", "rojo")])) +
  labs(
    title    = "Los regresores dejan de moverse justo en la ventana que hay que proyectar",
    subtitle = "Desvío estándar de cada palanca comercial, por nivel de agregación y tramo de la muestra",
    x = NULL, y = "Desvío estándar", fill = NULL,
    caption = paste0("En el nivel centro-plato el desvío de las campañas de email en validación es cero.",
                     "No hubo campañas en las semanas 136-145.")
  ) +
  theme(strip.text = element_text(face = "bold"))
guardar_fig(fig27, "fig27_variacion_regresores", alto = 4.4)

# Detalle por serie: ventajas y desventajas del ARIMAX
detalle_ratio <- bind_rows(
  precision_cat |> filter(.model %in% c("arima", "arimax")) |>
    select(serie = category, .model, MASE) |>
    mutate(nivel = "Categorias", serie = as.character(serie)),
  precision_com |> filter(.model %in% c("arima", "arimax")) |>
    select(serie = combo, .model, MASE) |> mutate(nivel = "Centro-plato")
) |>
  pivot_wider(names_from = .model, values_from = MASE) |>
  mutate(ratio = arimax / arima) |>
  arrange(nivel, ratio)
guardar_tabla(detalle_ratio, "t25_ratio_arimax_arima")

fig28 <- detalle_ratio |>
  ggplot(aes(x = ratio, y = fct_reorder(serie, -ratio), fill = ratio < 1)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_vline(xintercept = 1, color = "grey30", linetype = "dashed") +
  facet_wrap(~ nivel, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(`TRUE` = unname(PALETA[["verde"]]),
                               `FALSE` = unname(PALETA[["rojo"]])),
                    labels = c(`TRUE` = "ARIMAX mejor", `FALSE` = "ARIMA mejor")) +
  labs(
    title    = "Serie por serie: cuando agregar promociones mejora la proyección",
    subtitle = "Cociente entre el MASE del ARIMAX y el del ARIMA. Menor a 1 significa que el ARIMAX proyecta mejor",
    x = "MASE(ARIMAX) / MASE(ARIMA)", y = NULL, fill = NULL,
    caption = "11 de 12 categorías mejoran con regresores; 5 de 6 combinaciones centro-plato empeoran."
  ) +
  theme(strip.text = element_text(face = "bold"))
guardar_fig(fig28, "fig28_ratio_por_serie", alto = 5.2)
# Datos conservados
saveRDS(
  list(fit_cat = fit_cat, fc_cat = fc_cat, precision_cat = precision_cat,
       variacion_reg = variacion_reg, detalle_ratio = detalle_ratio,
       resumen_cat = resumen_cat, ts_cat = ts_cat,
       fit_com = fit_com, fc_com = fc_com, precision_com = precision_com,
       resumen_com = resumen_com, coef_com = coef_com, ts_com = ts_com,
       mejora = mejora),
  file.path(RUTA_RDS, "modelos_desagregados.rds")
)
message("\n04_categorias_combos.R terminado")
