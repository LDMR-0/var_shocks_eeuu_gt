
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
meses <- c(
  "enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04",
  "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08",
  "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
)
df_temp <- read_excel("input/remesas.xlsx", sheet = 1, range = "B10:AA22")
df_remesas <- df_temp |> 
  tidyr::pivot_longer(cols = -Mes, names_to = "anio", values_to="remesas") |>
  dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(Mes)], "01", sep="-"))) |>
  dplyr::select(fecha, remesas) |>
  dplyr::arrange(fecha)
rm(df_temp)
plot(df_remesas)

# -----------------------------------------------------------------------------
# Datos de RMI netas en millones de US dólares
# Fuente: https://banguat.gob.gt/page/mensuales-1995-la-fecha
# -----------------------------------------------------------------------------

df_temp <- read_excel("input/rmi_netas.xlsx", sheet = 1, range = "A6:AG18")
df_rmi <- df_temp |>
  tidyr::pivot_longer(cols = -MES, names_to = "anio", values_to = "rmi") |>
  dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(MES)], "01", sep="-"))) |>
  dplyr::select(fecha, rmi) |>
  dplyr::arrange(fecha)
rm(df_temp)

# -----------------------------------------------------------------------------
# Datos de Índice de Precios al Consumidor (IPC) intermensual
# Fuente: https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# -----------------------------------------------------------------------------
df_ipc <- read_excel("input/ipc.xlsx", sheet = 2)
df_ipc <- df_ipc |> 
  mutate(fecha = as.Date(PERIODO)) |>
  dplyr::rename(ipc = 4) |>
  dplyr::select(fecha, ipc)
  

# -----------------------------------------------------------------------------
# Datos de la Blanza Comercial (Exportaciones - Importaciones) en US dólares
# Fuente: https://banguat.gob.gt/page/serie-de-comercio-exterior-por-inciso-arancerlario-8-y-10-digitos
# -----------------------------------------------------------------------------
df_bc <- read_excel("input/balanza_comercial.xlsx", sheet = 2)
df_bc <- df_bc |>
  dplyr::mutate(fecha = as.Date(periodo), bc = exportaciones/importaciones) |>
  dplyr::select(fecha, bc)

# -----------------------------------------------------------------------------
# Datos Tasa de Interés Líder de la Política Monetaria de Guatemala
# Fuente: https://banguat.gob.gt/indicadoresgt/
# -----------------------------------------------------------------------------
df_i <- read_excel("input/tasa_lider.xlsx")
df_i <- df_i |> 
  dplyr::mutate(fecha = as.Date(fecha)) |>
  dplyr::rename(i = 7) |>
  dplyr::select(fecha, i)

# ------------------------------------------------------------------------------
# Datos Tipo de Cambio Bilateral Q-$ (TCR)
# Fuentes: https://fred.stlouisfed.org/series/CPIAUCSL
#          https://banguat.gob.gt/page/de-venta-promedio-del-mes
#          https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# ------------------------------------------------------------------------------
df_tcr <- read_excel("input/construccion_tcr_bilateral.xlsx", sheet = 2)
df_tcr <- df_tcr |>
  dplyr::rename(tc = 2) |>
  dplyr::select(fecha, tc)
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
# Creación del VAR: comenzando con 2002-01-01 (remesas) hasta enero 2026 (imae)
# Creación del VAR: comenzando con 2010-01-01 (IPC) hasta diciembre 2025 (bc)
# ==============================================================================
# Recordar: as.Date("yyyy-mm-dd")
fecha_ini <- as.Date("2010-01-01")
fecha_fin <- as.Date("2025-12-31")

df_bc <- df_bc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_i <- df_i |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_imae <- df_imae |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_ipc <- df_ipc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_remesas <- df_remesas |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_tcr <- df_tcr |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_rmi <- df_rmi |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_indpro <- df_indpro |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_pce <- df_pce |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_fedfunds <- df_fedfunds |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

# Pasar todas las fechas al primer día del mes
lista_dfs <- list(df_bc, df_i, df_imae, df_ipc, df_remesas, df_tcr, df_rmi,df_fedfunds, df_indpro, df_pce)

lista_dfs_mes <- map(lista_dfs, ~ .x |> 
   mutate(fecha = lubridate::floor_date(fecha, unit = "month"))
)
