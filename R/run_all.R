# =============================================================================
# run_all.R
# Ejecuta el proyecto completo de principio a fin

# Uso desde la raiz del repositorio:
#   Rscript R/run_all.R

# Reproduce todas las figuras (output/figuras) y tablas (output/tablas) que
# aparecen en el informe.

# Tiempo aproximado: 3 a 5 minutos.
# =============================================================================

t0 <- Sys.time()

source("R/01_datos.R")             # Carga, limpieza y construcción de tsibbles
source("R/02_eda.R")               # Parte 2: análisis exploratorio
source("R/03_modelos.R")           # Parte 3: modelado de la demanda total
source("R/04_categorias_combos.R") # Parte 3: categorías y centro-plato
source("R/05_forecast.R")          # Proyección final  semanas 146-155

message("\n=====================================================")
message("Proyecto ejecutado en ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " minutos")
message("Figuras en output/figuras | Tablas en output/tablas")
message("=====================================================")
