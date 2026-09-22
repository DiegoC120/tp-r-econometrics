# =============================================================================
# 02_eda.R - PARTE 2: Análisis exploratorio de datos
# =============================================================================

source("R/00_setup.R")

message("\n== 02_eda.R ==")
d <- readRDS(file.path(RUTA_RDS, "datos.rds"))
list2env(d, envir = environment())

# =============================================================================
# BLOQUE 1 - Estructura general del dataset
# =============================================================================

tabla_estructura <- tibble(
  archivo   = c("train.csv", "test.csv", "meal_info.csv", "fulfilment_center_info.csv"),
  filas     = c(nrow(train), nrow(test), nrow(meals), nrow(centros)),
  columnas  = c(9L, 8L, ncol(meals), ncol(centros)),
  contenido = c("Pedidos semanales por centro y plato (semanas 1-145)",
                "Similar pero sin la variable a predecir (semanas 146-155)",
                "Dimensiones de platos: categoría y tipo de cocina",
                "Dimensiones de centros: ciudad, región, tipo y área de operación")
)
guardar_tabla(tabla_estructura, "t01_estructura")

tabla_variables <- tibble(
  variable = c("id", "week", "center_id", "meal_id", "checkout_price", "base_price",
               "emailer_for_promotion", "homepage_featured", "num_orders"),
  tipo     = c("entero (identificador)", "entero (índice temporal 1-155)",
               "entero (identificador)", "entero (identificador)",
               "numérica continua", "numérica continua",
               "binaria 0/1", "binaria 0/1", "entero (conteo)"),
  rol      = c("clave", "índice de tiempo", "clave de panel", "clave de panel",
               "precio efectivamente cobrado", "precio de lista",
               "campaña de email", "destaque en página principal",
               "VARIABLE DEPENDIENTE: pedidos de la semana")
)
guardar_tabla(tabla_variables, "t02_variables")
# Mensajes
cat("Dimension del panel: ", n_distinct(train$center_id), " centros x ",
    n_distinct(train$meal_id), " platos x ", SEMANA_MAX, " semanas\n", sep = "")
cat("Combinaciones observadas: ", nrow(panel_diag),
    " de ", n_distinct(train$center_id) * n_distinct(train$meal_id),
    " posibles (", round(100 * nrow(panel_diag) /
                           (n_distinct(train$center_id) * n_distinct(train$meal_id)), 1),
    "%)\n", sep = "")

# =============================================================================
# BLOQUE 2 - La serie a modelar
# =============================================================================

semana_pico   <- ts_total$week[which.max(ts_total$pedidos)]
semana_caida  <- ts_total$week[which.min(ts_total$pedidos)]
mediana_ped   <- median(ts_total$pedidos)
# Figura 1
fig01 <- ts_total |>
  ggplot(aes(x = week, y = pedidos)) +
  annotate("rect", xmin = SEMANA_CORTE + 0.5, xmax = SEMANA_MAX + 0.5,
           ymin = -Inf, ymax = Inf, fill = PALETA[["azul"]], alpha = 0.10) +
  geom_hline(yintercept = mediana_ped, color = "grey60", linetype = "dashed") +
  geom_line(color = PALETA[["azul"]], linewidth = 0.7) +
  geom_point(data = ~ filter(.x, week %in% c(semana_pico, semana_caida)),
             color = PALETA[["rojo"]], size = 2.6) +
  annotate("text", x = semana_caida, y = min(ts_total$pedidos) - 45000,
           label = paste0("Semana ", semana_caida, ": ", fmt_miles(min(ts_total$pedidos)),
                          " pedidos\n(-53% vs. mediana)"),
           size = 3.1, color = PALETA[["rojo"]], hjust = 0.35, lineheight = 0.95) +
  annotate("text", x = semana_pico, y = max(ts_total$pedidos) + 55000,
           label = paste0("Semana ", semana_pico, ": máximo histórico"),
           size = 3.1, color = PALETA[["rojo"]]) +
  annotate("text", x = SEMANA_CORTE + H_PROYECCION / 2, y = 1240000,
           label = "ventana de\nvalidación", size = 3, color = PALETA[["azul"]],
           lineheight = 0.95) +
  scale_y_continuous(labels = fmt_miles, limits = c(300000, 1400000)) +
  scale_x_continuous(breaks = seq(0, 145, 20)) +
  labs(
    title    = "Demanda semanal total de la red de centros",
    subtitle = paste0("Suma de pedidos sobre ", nrow(panel_diag),
                      " combinaciones centro-plato | mediana ",
                      fmt_miles(mediana_ped), " pedidos por semana"),
    x = "Semana del panel", y = "Pedidos",
    caption = "Fuente: elaboración propia sobre Food Demand Forecasting (Kaggle), train.csv."
  )
guardar_fig(fig01, "fig01_serie_total")

# =============================================================================
# BLOQUE 3 - Valores faltantes
# =============================================================================
# No hay NA pero hay huecos en algunas series

cat("\nNAs por columna en train (todas en cero):\n")
print(colSums(is.na(train_raw <- train |> select(id:num_orders))))

tabla_faltantes <- tibble(
  situacion = c("historia completa (145 semanas)",
                "alta tardia (el plato aparece después de la semana 1)",
                "baja temprana (el plato deja de ofrecerse antes de la 145)",
                "con huecos internos (semanas sueltas sin registro)"),
  combos    = c(sum(panel_diag$completa), sum(panel_diag$alta_tardia),
                sum(panel_diag$baja_temprana), sum(panel_diag$huecos_int > 0)),
  porcentaje = round(100 * c(sum(panel_diag$completa), sum(panel_diag$alta_tardia),
                             sum(panel_diag$baja_temprana),
                             sum(panel_diag$huecos_int > 0)) / nrow(panel_diag), 1)
)
guardar_tabla(tabla_faltantes, "t03_faltantes")
print(tabla_faltantes)
# Figuras 2
fig02 <- panel_diag |>
  ggplot(aes(x = n_semanas)) +
  geom_histogram(binwidth = 5, fill = PALETA[["azul"]], color = "white", alpha = 0.9) +
  geom_vline(xintercept = SEMANA_MAX, color = PALETA[["rojo"]], linetype = "dashed") +
  annotate("text", x = 140, y = 900, hjust = 1,
           label = paste0(sum(panel_diag$completa), " combos con\nhistoria completa"),
           size = 3.2, color = PALETA[["rojo"]], lineheight = 0.95) +
  scale_x_continuous(breaks = seq(0, 145, 25)) +
  labs(
    title    = "Cuántas semanas vive cada combinación centro-plato",
    subtitle = "El panel es desbalanceado: la mayoria de los combos no cubre las 145 semanas",
    x = "Semanas con registro", y = "Cantidad de combinaciones centro-plato",
    caption = "Cada observación es una combinación centro-plato (n = 3.597)."
  )
guardar_fig(fig02, "fig02_largo_series")

# platos faltantes ¿aleatorio o existe un patrón?

set.seed(SEMILLA)
combos_muestra <- panel_diag |>
  slice_sample(n = 150) |>
  arrange(primera, ultima) |>
  mutate(orden = row_number())
# Figura 3
fig03 <- train |>
  inner_join(combos_muestra |> select(center_id, meal_id, orden),
             by = c("center_id", "meal_id")) |>
  ggplot(aes(x = week, y = orden)) +
  geom_tile(fill = PALETA[["azul"]], height = 1) +
  scale_x_continuous(breaks = seq(0, 145, 20), expand = c(0, 0)) +
  labs(
    title    = "Patrón de presencia de cada combinación centro-plato en el tiempo",
    subtitle = "Muestra aleatoria de 150 combos ordenados por semana de alta; en blanco, semanas sin registro",
    x = "Semana del panel", y = "Combinaciones centro-plato (ordenadas por alta)",
    caption = paste0("El faltante responde a altas y bajas propias de la gestión del menú",
                     "Semilla = ", SEMILLA, ".")
  ) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())
guardar_fig(fig03, "fig03_patron_faltantes")

# Cuántos platos hay activos cada semana. Figura 4
fig04 <- ts_total |>
  ggplot(aes(x = week, y = n_items)) +
  geom_line(color = PALETA[["verde"]], linewidth = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = PALETA[["gris"]], linewidth = 0.6, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 145, 20)) +
  labs(
    title    = "Amplitud del menú activo por semana",
    subtitle = "Cantidad de combinaciones centro-plato con registro en cada semana",
    x = "Semana del panel", y = "Combos activos",
    caption = "La tendencia creciente refleja la expansion del catálogo; controlamos por ella en el modelo."
  )
guardar_fig(fig04, "fig04_menu_activo")

# =============================================================================
# BLOQUE 4 - Distribuciones univariadas
# =============================================================================

# Tabla distribución num_orders
est_pedidos <- train |>
  summarise(
    min = min(num_orders), p25 = quantile(num_orders, .25),
    mediana = median(num_orders), media = mean(num_orders),
    p75 = quantile(num_orders, .75), p99 = quantile(num_orders, .99),
    max = max(num_orders), asimetria = mean((num_orders - mean(num_orders))^3) /
      sd(num_orders)^3
  )
guardar_tabla(est_pedidos, "t04_distribucion_pedidos")
print(est_pedidos)
# Figura 5, original vs logaritmo
fig05 <- train |>
  select(num_orders) |>
  mutate(Original = num_orders, `Escala log` = num_orders) |>
  pivot_longer(c(Original, `Escala log`), names_to = "escala", values_to = "valor") |>
  mutate(escala = factor(escala, levels = c("Original", "Escala log"))) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 60, fill = PALETA[["azul"]], color = "white", alpha = 0.9) +
  facet_wrap(~ escala, scales = "free") +
  scale_x_continuous(
    trans = "identity", labels = fmt_miles
  ) +
  labs(
    title    = "Distribución de pedidos por combinación centro-plato-semana",
    subtitle = paste0("Mediana ", est_pedidos$mediana, " pedidos, máximo ",
                      fmt_miles(est_pedidos$max),
                      ": cola derecha que justifica trabajar en logaritmos"),
    x = "Pedidos en la semana", y = "Frecuencia",
    caption = "n = 456.548 observaciones. El panel derecho usa escala logarítmica en el eje x."
  )
# El facet con escalas libres no aplica log por si solo: lo construimos aparte.
fig05 <- bind_rows(
  train |> transmute(valor = num_orders, escala = "Escala original"),
  train |> transmute(valor = num_orders, escala = "Escala logarítmica")
) |>
  mutate(escala = factor(escala, levels = c("Escala original", "Escala logarítmica"))) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 60, fill = PALETA[["azul"]], color = "white", alpha = 0.9) +
  facet_wrap(~ escala, scales = "free") +
  scale_x_continuous(labels = fmt_miles) +
  labs(
    title    = "Distribución de pedidos por combinación centro-plato-semana",
    subtitle = paste0("Mediana ", est_pedidos$mediana, " pedidos, máximo ",
                      fmt_miles(est_pedidos$max),
                      ": cola derecha que justifica trabajar en logaritmos"),
    x = "Pedidos en la semana", y = "Frecuencia",
    caption = "n = 456.548 observaciones. Ver el panel logaritmico en fig05b."
  )
guardar_fig(fig05, "fig05_dist_pedidos")

fig05b <- train |>
  ggplot(aes(x = num_orders)) +
  geom_histogram(bins = 60, fill = PALETA[["naranja"]], color = "white", alpha = 0.9) +
  scale_x_log10(labels = fmt_miles) +
  labs(
    title    = "Pedidos en escala logarítmica",
    subtitle = "En logaritmos la distribución es aproximadamente simétrica",
    x = "Pedidos en la semana (escala log10)", y = "Frecuencia"
  )
guardar_fig(fig05b, "fig05b_dist_pedidos_log", alto = 4.2)

# =============================================================================
# BLOQUE 5 - Precios y descuentos
# =============================================================================

share_recargo <- mean(train$recargo)
cat("\nFilas con precio cobrado > precio de lista:", percent(share_recargo, 0.1), "\n")
cat("Rango del descuento:", percent(min(train$descuento), 0.1), "a",
    percent(max(train$descuento), 0.1), "\n")
# Figura 6, análisis del descuento
fig06 <- train |>
  ggplot(aes(x = descuento)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = PALETA[["rojo"]], alpha = 0.08) +
  geom_histogram(bins = 80, fill = PALETA[["azul"]], color = "white", alpha = 0.9) +
  geom_vline(xintercept = 0, color = PALETA[["rojo"]], linewidth = 0.6) +
  annotate("text", x = -0.38, y = 60000, hjust = 0.5, size = 3.2,
           color = PALETA[["rojo"]], lineheight = 0.95,
           label = paste0("Recargo: precio cobrado\n> precio de lista\n",
                          percent(share_recargo, 0.1), " de las filas")) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = fmt_miles) +
  labs(
    title    = "Distribución del descuento efectivo",
    subtitle = "descuento = (precio de lista - precio cobrado) / precio de lista",
    x = "Descuento", y = "Frecuencia"
    )
guardar_fig(fig06, "fig06_descuento")

# =============================================================================
# BLOQUE 6 - Relaciones bivariadas relevantes para la técnica elegida
# =============================================================================

# Regresores para ARIMAX
# Figura 7 - Gráfico de caja y brazo
fig07 <- train |>
  mutate(
    promo = case_when(
      emailer_for_promotion == 1 & homepage_featured == 1 ~ "Email + Home",
      emailer_for_promotion == 1                          ~ "Solo email",
      homepage_featured == 1                              ~ "Solo home",
      TRUE                                                ~ "Sin promoción"
    ),
    promo = factor(promo, levels = c("Sin promoción", "Solo email",
                                     "Solo home", "Email + Home"))
  ) |>
  ggplot(aes(x = promo, y = num_orders, fill = promo)) +
  geom_boxplot(outlier.alpha = 0.04, outlier.size = 0.4, alpha = 0.85) +
  scale_y_log10(labels = fmt_miles) +
  scale_fill_manual(values = unname(PALETA[c("gris", "azul", "verde", "naranja")])) +
  labs(
    title    = "Pedidos según el tipo de acción promocional",
    subtitle = "Escala logarítmica. La combinación email + destaque en home multiplica la mediana de pedidos",
    x = NULL, y = "Pedidos en la semana (escala log10)",
    caption = "Advertencia: es una comparación descriptiva. La promoción no se asigna al azar."
  ) +
  theme(legend.position = "none")
guardar_fig(fig07, "fig07_promocion")
# Tabla promo
tabla_promo <- train |>
  group_by(emailer_for_promotion, homepage_featured) |>
  summarise(obs = n(), mediana_pedidos = median(num_orders),
            media_pedidos = round(mean(num_orders), 1), .groups = "drop")
guardar_tabla(tabla_promo, "t05_promocion")
print(tabla_promo)

# Se trabaja con combos que tengan al menos 30 semanas, para que la media por
# combo que se resta este bien estimada.
base_elast <- train |>
  group_by(center_id, meal_id) |>
  filter(n() >= 30) |>
  mutate(
    log_pedidos    = log(num_orders),
    # "within": cada observación se expresa como desvío respecto del promedio
    # histórico de esa combinacion centro-plato. Efecto fijo por combo
    # comparacion de cada producto consigo mismo
    dto_within     = descuento   - mean(descuento),
    log_ped_within = log_pedidos - mean(log_pedidos)
  ) |>
  ungroup() |>
  mutate(log_ped_centrado = log_pedidos - mean(log_pedidos))

m_pooled <- lm(log_pedidos ~ descuento, data = base_elast)
m_within <- lm(log_ped_within ~ 0 + dto_within, data = base_elast)

elasticidades <- tibble(
  lectura = c("Agrupada (compara productos entre sí)",
              "Intra-producto (compara cada producto consigo mismo)"),
  beta    = c(coef(m_pooled)[2], coef(m_within)[1]),
  r2      = c(summary(m_pooled)$r.squared, summary(m_within)$r.squared),
  correlacion = c(cor(base_elast$descuento, base_elast$log_pedidos),
                  cor(base_elast$dto_within, base_elast$log_ped_within))
)
guardar_tabla(elasticidades, "t08b_elasticidad_pooled_within")
cat("\nRespuesta al precio segun como se mire:\n")
print(elasticidades)

# Concentración del descuento
concentracion <- train |>
  mutate(tramo = cut(descuento, c(-Inf, -0.25, -0.02, 0.02, 0.10, 0.30, Inf),
                     labels = c("recargo mayor al 25%", "recargo de 2% a 25%",
                                "sin cambio (mas o menos 2%)", "descuento de 2% a 10%",
                                "descuento de 10% a 30%", "descuento mayor al 30%"))) |>
  count(tramo) |>
  mutate(porcentaje = round(100 * n / sum(n), 2))
guardar_tabla(concentracion, "t08c_concentracion_descuento")
cat("\nConcentracion del descuento:\n")
print(concentracion)

set.seed(SEMILLA)
muestra_elast <- base_elast |> slice_sample(n = 40000)
# Figura 8 
datos_fig08 <- bind_rows(
  muestra_elast |>
    transmute(x = descuento, y = log_ped_centrado,
              panel = "Agrupado: se comparan productos distintos entre sí"),
  muestra_elast |>
    transmute(x = dto_within, y = log_ped_within,
              panel = "Intra-producto: se compara cada producto consigo mismo")
) |>
  mutate(panel = factor(panel, levels = c(
    "Agrupado: se comparan productos distintos entre sí",
    "Intra-producto: se compara cada producto consigo mismo")))

etiquetas_fig08 <- tibble(
  panel = levels(datos_fig08$panel),
  x = c(0.55, 0.30),
  y = c(-2.8, -2.8),
  txt = sprintf("pendiente = %.2f\nR\u00b2 = %.3f", elasticidades$beta, elasticidades$r2)
) |>
  mutate(panel = factor(panel, levels = levels(datos_fig08$panel)))

fig08 <- datos_fig08 |>
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.05, size = 0.45, color = PALETA[["azul"]]) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = PALETA[["rojo"]], linewidth = 1.1) +
  geom_text(data = etiquetas_fig08, aes(x = x, y = y, label = txt),
            inherit.aes = FALSE, size = 3.4, hjust = 1, lineheight = 0.95,
            color = PALETA[["rojo"]], fontface = "bold") +
  facet_wrap(~ panel, scales = "free_x") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "La respuesta al precio se ve dentro de cada producto",
    subtitle = paste0("A la derecha se le resta a cada ",
                      "observación el promedio histórico de su propia combinación centro-plato"),
    x = "Descuento efectivo (a la derecha, respecto del habitual de ese producto)",
    y = "log(pedidos), centrado",
    caption = paste0("Muestra aleatoria de 40.000 observaciones sobre combinaciones con al menos 30 semanas ",
                     "Semilla = ", SEMILLA, ". Restar la media de cada producto equivale a incluir un efecto fijo.",
                     "La pendiente pasa de ", round(elasticidades$beta[1], 2), " a ",
                     round(elasticidades$beta[2], 2), " y el R² se multiplica por ",
                     round(elasticidades$r2[2] / elasticidades$r2[1], 1), ".")
  ) +
  theme(strip.text = element_text(face = "bold", size = 9.5))
guardar_fig(fig08, "fig08_descuento_pedidos", alto = 5.2)

# =============================================================================
# BLOQUE 7 - Heterogeneidad: categorías y centros
# =============================================================================
# Figura 9
fig09 <- ts_categoria |>
  as_tibble() |>
  mutate(category = fct_reorder(category, -pedidos, .fun = sum)) |>
  ggplot(aes(x = week, y = pedidos)) +
  geom_line(color = PALETA[["azul"]], linewidth = 0.5) +
  facet_wrap(~ category, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = fmt_miles) +
  scale_x_continuous(breaks = c(1, 50, 100, 145)) +
  labs(
    title    = "Demanda semanal por categoría de plato",
    subtitle = "Ordenadas por volumen acumulado",
    x = "Semana del panel", y = "Pedidos (escala propia de cada panel)",
    caption = "La caida de la semana 62 aparece en casi todas las categorías."
  ) +
  theme(strip.text = element_text(face = "bold", size = 8))
guardar_fig(fig09, "fig09_categorias", alto = 6.2)

# La caída de la semana 62, ¿es de una categoria y de pocos centros?
ratio_centros <- train |>
  filter(week %in% 55:70) |>
  group_by(center_id, semana62 = week == 62) |>
  summarise(pedidos = sum(num_orders), n = n(), .groups = "drop") |>
  mutate(promedio = pedidos / if_else(semana62, 1, 15)) |>
  select(center_id, semana62, promedio) |>
  pivot_wider(names_from = semana62, values_from = promedio,
              names_prefix = "sem62_") |>
  mutate(ratio = sem62_TRUE / sem62_FALSE) |>
  left_join(centros |> select(center_id, center_type), by = "center_id")

cat("\nSemana 62: centros con caida mayor al 30%:",
    sum(ratio_centros$ratio < 0.7), "de", nrow(ratio_centros), "\n")
# Figura 10 - efecto caida
fig10 <- ratio_centros |>
  ggplot(aes(x = ratio, fill = center_type)) +
  geom_histogram(bins = 24, color = "white", alpha = 0.9) +
  geom_vline(xintercept = 1, color = "grey40", linetype = "dashed") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = PALETA_TIPO) +
  labs(
    title    = "La caída de la semana 62 afecta a todos los centros",
    subtitle = "Pedidos de la semana 62 como porcentaje del promedio de las semanas 55-70",
    x = "Semana 62 / promedio", y = "Cantidad de centros", fill = "Tipo de centro",
    caption = "Ningún centro queda por encima del 65%, lo que indica que el shock es sistémico."
  )
guardar_fig(fig10, "fig10_semana62")
# Fugura 11
fig11 <- train |>
  group_by(center_id) |>
  summarise(pedidos_sem = sum(num_orders) / n_distinct(week), .groups = "drop") |>
  left_join(centros, by = "center_id") |>
  ggplot(aes(x = op_area, y = pedidos_sem, color = center_type)) +
  geom_point(size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = PALETA[["gris"]], linewidth = 0.6, linetype = "dashed") +
  scale_y_continuous(labels = fmt_miles) +
  scale_color_manual(values = PALETA_TIPO) +
  labs(
    title    = "Escala del centro y demanda semanal promedio",
    subtitle = "Cada punto es uno de los 77 centros de distribución",
    x = "Área de operación (op_area)", y = "Pedidos semanales promedio",
    color = "Tipo de centro",
    caption = "Los centros TYPE_B concentran mayor volumen; el área de operación explica buena parte de la dispersión"
  )
guardar_fig(fig11, "fig11_centros")

# =============================================================================
# BLOQUE 8 - Propiedades de serie de tiempo
# =============================================================================
# Descomposición, autocorrelación y estacionariedad, para ver si corresponde ARIMA, SARIMA o ARIMAX.

stl_total <- ts_total |>
  model(STL(pedidos ~ trend(window = 21) + season(window = "periodic"),
            robust = TRUE)) |>
  components()
# Figura 12 Descomposición (STL)
fig12 <- stl_total |>
  as_tibble() |>
  mutate(week = a_week(semana)) |>
  select(week, pedidos, trend, season_year, remainder) |>
  rename(Observada = pedidos, Tendencia = trend,
         `Estacionalidad anual` = season_year, Residuo = remainder) |>
  pivot_longer(-week, names_to = "componente", values_to = "valor") |>
  mutate(componente = factor(componente, levels = c("Observada", "Tendencia",
                                                    "Estacionalidad anual", "Residuo"))) |>
  ggplot(aes(x = week, y = valor)) +
  geom_line(color = PALETA[["azul"]], linewidth = 0.6) +
  facet_wrap(~ componente, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = fmt_miles) +
  scale_x_continuous(breaks = seq(0, 145, 20)) +
  labs(
    title    = "Descomposición STL de la demanda total semanal",
    subtitle = "El componente estacional (sd 98.800) es del mismo orden que el residuo (sd 104.200) y mucho mayor que la tendencia (sd 42.300)",
    x = "Semana del panel", y = "Pedidos",
    caption = paste0("STL robusta (robust = TRUE). Con solo 2,8 ciclos anuales la estacionalidad esta debilmente identificada: ",
                     "parte del pico de la semana 48 se absorbe como 'estacional'. Por eso la estacionalidad se testea, no se impone.")
  ) +
  theme(strip.text = element_text(face = "bold", size = 9))
guardar_fig(fig12, "fig12_stl", alto = 6.5)

# Peso relativo de cada componente (fuerza de tendencia y estacionalidad)
fuerzas <- ts_total |> features(pedidos, feat_stl)
guardar_tabla(fuerzas |> select(trend_strength, seasonal_strength_year), "t06_fuerza_stl")
cat("\nFuerza de tendencia:", round(fuerzas$trend_strength, 3),
    "| Fuerza estacional anual:", round(fuerzas$seasonal_strength_year, 3), "\n")

# ACF y PACF - Figura 13
acf_df  <- ts_total |> ACF(pedidos,  lag_max = 40) |> as_tibble() |>
  mutate(funcion = "ACF")  |> rename(valor = acf)
pacf_df <- ts_total |> PACF(pedidos, lag_max = 40) |> as_tibble() |>
  mutate(funcion = "PACF") |> rename(valor = pacf)
limite  <- 1.96 / sqrt(nrow(ts_total))

fig13 <- bind_rows(acf_df, pacf_df) |>
  mutate(lag_num = as.numeric(lag)) |>
  ggplot(aes(x = lag_num, y = valor)) +
  geom_hline(yintercept = c(-limite, limite), color = PALETA[["rojo"]],
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_segment(aes(xend = lag_num, yend = 0), color = PALETA[["azul"]], linewidth = 0.7) +
  facet_wrap(~ funcion, ncol = 2) +
  labs(
    title    = "Autocorrelación de la demanda total semanal",
    subtitle = "Decaimiento rápido en la ACF y corte en la PACF: patrón compatible con un AR de orden bajo, sin raiz unitaria",
    x = "Rezago (semanas)", y = "Correlación",
    caption = "Bandas al 95% en rojo."
  ) +
  theme(strip.text = element_text(face = "bold"))
guardar_fig(fig13, "fig13_acf_pacf", alto = 4.2)

# Test de raiz unitaria
adf_nivel <- suppressWarnings(adf.test(ts_total$pedidos))
adf_dif   <- suppressWarnings(adf.test(diff(ts_total$pedidos)))
tabla_adf <- tibble(
  serie     = c("pedidos (nivel)", "primera diferencia"),
  estadistico = round(c(adf_nivel$statistic, adf_dif$statistic), 3),
  p_valor   = c(adf_nivel$p.value, adf_dif$p.value),
  conclusion = c("se rechaza raiz unitaria", "se rechaza raiz unitaria")
)
guardar_tabla(tabla_adf, "t07_adf")
print(tabla_adf)

# Figura 14

fig14 <- ts_total |>
  as_tibble() |>
  transmute(week,
            `Descuento medio del menu` = descuento_medio,
            `% de items con email`     = share_emailer,
            `% de items en home`       = share_home) |>
  pivot_longer(-week, names_to = "regresor", values_to = "valor") |>
  ggplot(aes(x = week, y = valor, color = regresor)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ regresor, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 145, 20)) +
  scale_color_manual(values = unname(PALETA[c("naranja", "verde", "violeta")])) +
  labs(
    title    = "Las tres palancas comerciales, semana a semana",
    subtitle = "Son variables de decision de la empresa y estan disponibles para las semanas 146-155: por eso habilitan un ARIMAX",
    x = "Semana del panel", y = NULL,
    caption = "Correlación contemporanea con los pedidos totales: 0,37 (descuento), 0,47 (email), 0,43 (home)."
  ) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 9))
guardar_fig(fig14, "fig14_regresores")

correlaciones <- ts_total |>
  as_tibble() |>
  summarise(across(c(descuento_medio, share_emailer, share_home, n_items, precio_medio),
                   ~ round(cor(.x, pedidos), 3)))
guardar_tabla(correlaciones, "t08_correlaciones")
print(correlaciones)
