
# ==============================================================================
# Datos de economía doméstica Guatemala
# Carga de datos:exportaciones, remesas, inflación y reservas internacionales
# ==============================================================================
# -----------------------------------------------------------------------------
# Datos del IMAE
# Fuente: https://banguat.gob.gt/page/indice-mensual-de-la-actividad-economica-imae-ano-de-referencia-2013
# -----------------------------------------------------------------------------
df_imae <- read_excel("input/imae_2.xlsx", sheet = 2)
df_imae <- df_imae |> dplyr::rename(fecha = 1, imae = 2)
df_imae <- df_imae |> mutate(fecha = as.Date(fecha))


# -----------------------------------------------------------------------------
# Datos de las remesas en millones de US dólares:
# Fuente: https://banguat.gob.gt/page/anos-2002-2026
# -----------------------------------------------------------------------------

# meses <- c(
#   "enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04",
#   "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08",
#   "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
# )
# df_temp <- read_excel("input/remesas.xlsx", sheet = 1, range = "B10:AA22")
# df_remesas <- df_temp |> 
#   tidyr::pivot_longer(cols = -Mes, names_to = "anio", values_to="remesas") |>
#   dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(Mes)], "01", sep="-"))) |>
#   dplyr::select(fecha, remesas) |>
#   dplyr::arrange(fecha)
# rm(df_temp)
# plot(df_remesas)

# -----------------------------------------------------------------------------
# Datos de RMI netas en millones de US dólares
# Fuente: https://banguat.gob.gt/page/mensuales-1995-la-fecha
# -----------------------------------------------------------------------------

# df_temp <- read_excel("input/rmi_netas.xlsx", sheet = 1, range = "A6:AG18")
# df_rmi <- df_temp |>
#   tidyr::pivot_longer(cols = -MES, names_to = "anio", values_to = "rmi") |>
#   dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(MES)], "01", sep="-"))) |>
#   dplyr::select(fecha, rmi) |>
#   dplyr::arrange(fecha)
# rm(df_temp)

# -----------------------------------------------------------------------------
# Datos de Índice de Precios al Consumidor (IPC) intermensual
# Fuente: https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# -----------------------------------------------------------------------------

df_ipc <- read_excel("input/ipc.xlsx", sheet = 2)
df_ipc <- df_ipc |>
  mutate(fecha = as.Date(PERIODO)) |>
  dplyr::rename(ipc = 2) |>
  dplyr::select(fecha, ipc)


# -----------------------------------------------------------------------------
# Datos de la inflación en guatemala (Variación Interanual)
# Fuente: https://banguat.gob.gt/page/indice-mensual-de-la-actividad-economica-imae-ano-de-referencia-2013
# -----------------------------------------------------------------------------
# 
# df_temp <- read_excel(
#   "input/inflacion_gt.xls",
#   sheet = 1,
#   skip = 5
# )
# 
# # Vector para convertir los meses a números
# meses <- c(
#   "Enero"      = 1,
#   "Febrero"    = 2,
#   "Marzo"      = 3,
#   "Abril"      = 4,
#   "Mayo"       = 5,
#   "Junio"      = 6,
#   "Julio"      = 7,
#   "Agosto"     = 8,
#   "Septiembre" = 9,
#   "Octubre"    = 10,
#   "Noviembre"  = 11,
#   "Diciembre"  = 12
# )
# 
# df_inflacion <- df_temp |>
#   
#   # Nos quedamos únicamente con las filas correspondientes a meses
#   filter(Periodo %in% names(meses)) |>
#   
#   # Pasar los años de columnas a filas
#   pivot_longer(
#     cols = matches("^\\d{4}$"),
#     names_to = "anio",
#     values_to = "inflacion"
#   ) |>
#   
#   # Construir una fecha mensual
#   mutate(
#     anio = as.integer(anio),
#     mes = meses[Periodo],
#     fecha = as.Date(sprintf("%04d-%02d-01", anio, mes)),
#     inflacion = as.numeric(inflacion)
#   ) |>
#   
#   # Dejar únicamente las variables que nos interesan
#   select(fecha, inflacion) |>
#   
#   # Orden cronológico
#   arrange(fecha)
# 
# df_inflacion <- df_inflacion |> filter(!is.na(inflacion))
# rm(df_temp)
#   

# -----------------------------------------------------------------------------
# Datos de la Blanza Comercial (Exportaciones - Importaciones) en US dólares
# Fuente: https://banguat.gob.gt/page/serie-de-comercio-exterior-por-inciso-arancerlario-8-y-10-digitos
# -----------------------------------------------------------------------------
# df_bc <- read_excel("input/balanza_comercial.xlsx", sheet = 2)
# df_bc <- df_bc |>
#   dplyr::mutate(fecha = as.Date(periodo), bc = exportaciones/importaciones) |>
#   dplyr::select(fecha, bc)

# -----------------------------------------------------------------------------
# Datos de la Blanza Comercial (Exportaciones - Importaciones) en US dólares
# Fuente: https://banguat.gob.gt/page/serie-de-comercio-exterior-por-inciso-arancerlario-8-y-10-digitos
# -----------------------------------------------------------------------------


# Leer archivo
comercio_raw <- read_excel(
  "input/balanza_comercial.xlsx"
)
# Construir dataframe mensual
df_bc <- comercio_raw %>%
  
  # Nos quedamos únicamente con las variables necesarias
  select(
    Año,
    Mes,
    `Nombre de Comercio Exterior`,
    USD
  ) %>%
  
  # Pasar Exportaciones e Importaciones de filas a columnas
  pivot_wider(
    names_from = `Nombre de Comercio Exterior`,
    values_from = USD
  ) %>%
  
  # Crear fecha y balanza comercial
  mutate(
    fecha = make_date(Año, Mes, 1),
    bc = Exportaciones / Importaciones
  ) %>%
  
  # Ordenar cronológicamente
  arrange(fecha) %>%
  
  # Elegir y ordenar las columnas finales
  select(
    fecha,
    # Año,
    # Mes,
    # Exportaciones,
    # Importaciones,
    bc
  )

rm(comercio_raw)
# -----------------------------------------------------------------------------
# Datos Tasa de Interés Líder de la Política Monetaria de Guatemala
# Fuente: https://banguat.gob.gt/indicadoresgt/
# -----------------------------------------------------------------------------
df_i <- read_excel("input/tasa_lider.xlsx")
df_i <- df_i |> 
  dplyr::mutate(fecha = dmy(fecha)) |>
  dplyr::rename(i = 7) |>
  dplyr::select(fecha, i)


# ------------------------------------------------------------------------------
# Datos Tipo de Cambio Bilateral Q-$ (TCR)
# Fuentes: https://fred.stlouisfed.org/series/CPIAUCSL
#          https://banguat.gob.gt/page/de-venta-promedio-del-mes
#          https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# ------------------------------------------------------------------------------
# df_tcr <- read_excel("input/construccion_tcr_bilateral.xlsx", sheet = 2)
# df_tcr <- df_tcr |>
#   dplyr::rename(tc = 2) |>
#   dplyr::select(fecha, tc)


# ------------------------------------------------------------------------------
# Tipo de cambio de referencia Q/USD
# Fuente: https://banguat.gob.gt/tipo_cambio/
# ------------------------------------------------------------------------------


df_tc <- read_csv(
  "input/tc.csv",
  skip = 3,
  show_col_types = FALSE
) |>
  filter(grepl("^\\d{2}/\\d{2}/\\d{4}$", Fecha)) |>
  mutate(
    fecha = dmy(Fecha)
  ) |>
  rename(
    tc = `TCR 1/`
  ) |>
  select(fecha, tc) |>
  filter(!is.na(tc))

# Convertir de frecuencia diaria a mensual
df_tc <- df_tc |>
  mutate(
    fecha = floor_date(fecha, "month")
  ) |>
  group_by(fecha) |>
  summarise(
    tc = mean(tc, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(fecha)

# ==============================================================================
# Datos de economía extranjeta (EE.UU.)
# ==============================================================================
# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de supply Industrial Production (index)
# Fuente: https://fred.stlouisfed.org/series/INDPRO
# -----------------------------------------------------------------------------
df_indpro <- read_excel("input/INDPRO.xlsx", sheet = 2)
df_indpro <- df_indpro |> dplyr::mutate(fecha = as.Date(observation_date)) |>
  rename(indpro = 2)

# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de shock monetario Tasa de Interes Efectiva Federal
# Fuente: https://fred.stlouisfed.org/series/DFF
# -----------------------------------------------------------------------------
df_fedfunds <- read_excel("input/FEDFUNDS.xlsx", sheet = 2)
df_fedfunds <- df_fedfunds |> dplyr::mutate(fecha = as.Date(observation_date)) |> 
  rename(fedfunds = 2)

# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de shock de Demanda Agregada Real Personal Consumption 
# Expenditures (PCEC96)
# Fuente: https://fred.stlouisfed.org/series/PCEC96
# -----------------------------------------------------------------------------

df_pce <- read_excel("input/PCEC96.xlsx", sheet = 2)
df_pce <- df_pce |> dplyr::mutate(fecha = as.Date(observation_date)) |>
  rename(pce = 2)


# ==============================================================================
# Creación del VAR: comenzando con 2007-01-01 (PCE)
# Creación del VAR: comenzando con 2010-01-01 (IPC) hasta diciembre 2025 (bc)
# ==============================================================================
# Recordar: as.Date("yyyy-mm-dd")
fecha_ini <- as.Date("2010-01-01")
fecha_fin <- as.Date("2026-06-30")

df_bc <- df_bc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_i <- df_i |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_imae <- df_imae |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

# df_remesas <- df_remesas |>
#   dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_tc <- df_tc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_indpro <- df_indpro |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_pce <- df_pce |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_fedfunds <- df_fedfunds |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_ipc <- df_ipc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

# df_inflacion <- df_inflacion |>
#   dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

# Pasar todas las fechas al primer día del mes
# lista_dfs <- list(df_inflacion, df_bc, df_i, df_imae, df_tc, df_fedfunds, df_indpro, df_pce)

lista_dfs <- list(df_ipc, df_bc, df_i, df_imae, df_tc, df_fedfunds, df_indpro, df_pce)

lista_dfs_mes <- map(lista_dfs, ~ .x |> 
   mutate(fecha = lubridate::floor_date(fecha, unit = "month"))
)
# Preparar la data que se usara en el modelo VAR
data <- reduce(lista_dfs_mes, inner_join, by = "fecha")
rm(lista_dfs_mes)
rm(lista_dfs)


