# ============================================================
# DESCOMPOSICIÓN DE LA VARIANZA DEL ERROR DE PRONÓSTICO
# ============================================================

library(vars)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 1. Calcular la FEVD a 12 meses
# ------------------------------------------------------------

fevd_obj <- fevd(
    var_bloques,
    n.ahead = 12
)


# ------------------------------------------------------------
# 2. Revisar cuánto suman originalmente las filas
# ------------------------------------------------------------

sapply(
    fevd_obj,
    function(x) range(rowSums(x))
)


# ------------------------------------------------------------
# 3. Normalizar la FEVD
#
# Para cada variable y horizonte:
#
# participación shock j =
# FEVD_j / suma de todos los shocks * 100
#
# De esta manera, todos los shocks del sistema suman 100 %
# ------------------------------------------------------------

fevd_norm <- lapply(
    fevd_obj,
    function(mat) {
        
        suma_fila <- rowSums(mat)
        
        sweep(
            mat,
            MARGIN = 1,
            STATS = suma_fila,
            FUN = "/"
        ) * 100
    }
)


# ------------------------------------------------------------
# 4. Comprobar que ahora cada horizonte suma 100 %
# ------------------------------------------------------------

sapply(
    fevd_norm,
    function(x) range(rowSums(x))
)

# Deberías observar valores cercanos a:
# 100 100


# ------------------------------------------------------------
# 5. Convertir las matrices FEVD a un data frame largo
# ------------------------------------------------------------

fevd_df <- imap_dfr(
    fevd_norm,
    function(mat, response) {
        
        as.data.frame(mat) |>
            mutate(
                Horizon = seq_len(nrow(mat)),
                Response = response
            ) |>
            pivot_longer(
                cols = -c(Horizon, Response),
                names_to = "Shock",
                values_to = "Share"
            )
    }
)


# ------------------------------------------------------------
# 6. Limpiar los nombres técnicos
# ------------------------------------------------------------

fevd_df <- fevd_df |>
    mutate(
        Response_clean = Response,
        Shock_clean = Shock,
        
        Response_clean = sub(
            "^xt_us\\.",
            "",
            Response_clean
        ),
        
        Response_clean = sub(
            "^zt_gt\\.",
            "",
            Response_clean
        ),
        
        Shock_clean = sub(
            "^xt_us\\.",
            "",
            Shock_clean
        ),
        
        Shock_clean = sub(
            "^zt_gt\\.",
            "",
            Shock_clean
        )
    )


# ------------------------------------------------------------
# 7. Etiquetas de las variables de Guatemala
# ------------------------------------------------------------

etiquetas_respuesta <- c(
    "GT_dlog_imae" = "Actividad económica (IMAE)",
    "GT_dlog_ipc"  = "Inflación (IPC)",
    "GT_dlog_bc"   = "Razón exportaciones/importaciones",
    "GT_d_i"       = "Tasa líder de política monetaria",
    "GT_tc"        = "Tipo de cambio nominal"
)


# ------------------------------------------------------------
# 8. Etiquetas de los shocks de Estados Unidos
# ------------------------------------------------------------

etiquetas_shock <- c(
    "US_dlog_indpro" = "Actividad productiva (INDPRO)",
    "US_dlog_pce"    = "Demanda (PCE)",
    "US_d_fedfunds"  = "Política monetaria (FEDFUNDS)"
)


# ------------------------------------------------------------
# 9. Seleccionar únicamente:
#
# Respuestas = variables de Guatemala
# Shocks     = variables de Estados Unidos
# ------------------------------------------------------------

fevd_us <- fevd_df |>
    filter(
        Response_clean %in% names(etiquetas_respuesta),
        Shock_clean %in% names(etiquetas_shock)
    ) |>
    mutate(
        
        Response_label = recode(
            Response_clean,
            !!!etiquetas_respuesta
        ),
        
        Shock_label = recode(
            Shock_clean,
            !!!etiquetas_shock
        ),
        
        # Orden deseado de las variables guatemaltecas
        Response_label = factor(
            Response_label,
            levels = c(
                "Actividad económica (IMAE)",
                "Inflación (IPC)",
                "Razón exportaciones/importaciones",
                "Tasa líder de política monetaria",
                "Tipo de cambio nominal"
            )
        ),
        
        # Orden de los shocks
        Shock_label = factor(
            Shock_label,
            levels = c(
                "Actividad productiva (INDPRO)",
                "Demanda (PCE)",
                "Política monetaria (FEDFUNDS)"
            )
        )
    )


# ------------------------------------------------------------
# 10. Paleta de colores
# ------------------------------------------------------------

colores_shocks <- c(
    "Actividad productiva (INDPRO)" = "#1F4E5F",
    "Demanda (PCE)"                 = "#5F7F73",
    "Política monetaria (FEDFUNDS)" = "#8A3B3B"
)


# ------------------------------------------------------------
# 11. Crear gráfica
# ------------------------------------------------------------

fig_fevd_us <- ggplot(
    fevd_us,
    aes(
        x = Horizon,
        y = Share,
        color = Shock_label
    )
) +
    
    geom_line(
        linewidth = 0.9
    ) +
    
    geom_point(
        size = 1.5
    ) +
    
    facet_wrap(
        ~ Response_label,
        ncol = 2,
        scales = "free_y",
        axes = "all_x",
        axis.labels = "all_x"
    ) +
    
    scale_color_manual(
        values = colores_shocks,
        name = NULL
    ) +
    
    scale_x_continuous(
        breaks = c(
            1, 3, 6, 9, 12
        ),
        limits = c(
            1, 12
        )
    ) +
    
    scale_y_continuous(
        labels = label_number(
            accuracy = 0.1,
            suffix = "%"
        ),
        expand = expansion(
            mult = c(0, 0.08)
        )
    ) +
    
    labs(
        x = "Horizonte de pronóstico (meses)",
        y = "Porcentaje de la varianza"
    ) +
    
    theme_minimal(
        base_size = 10
    ) +
    
    theme(
        
        # Encabezados de cada panel
        strip.background = element_rect(
            fill = "#D9E4E8",
            color = NA
        ),
        
        strip.text = element_text(
            face = "bold",
            size = 9,
            color = "#243238"
        ),
        
        # Cuadrícula
        panel.grid.major = element_line(
            color = "#E1E6E8",
            linewidth = 0.35
        ),
        
        panel.grid.minor = element_blank(),
        
        # Borde
        panel.border = element_rect(
            color = "#BBC5C9",
            fill = NA,
            linewidth = 0.4
        ),
        
        # Leyenda
        legend.position = "bottom",
        
        legend.text = element_text(
            size = 8
        ),
        
        # Ejes
        axis.text = element_text(
            color = "#4F5B60"
        ),
        
        axis.text.x = element_text(
            size = 8
        ),
        
        axis.title = element_text(
            color = "#243238"
        ),
        
        # Espaciado
        panel.spacing = unit(
            0.8,
            "lines"
        ),
        
        plot.margin = margin(
            10, 12, 8, 10
        )
    )


# ------------------------------------------------------------
# 12. Mostrar gráfica
# ------------------------------------------------------------

fig_fevd_us


# ------------------------------------------------------------
# 13. Guardar como PNG
# ------------------------------------------------------------

ggsave(
    "informe_quarto/figuras/fevd_dinamica.png",
    fig_fevd_us,
    width = 6.5,
    height = 5,
    units = "in",
    dpi = 600,
    bg = "white"
)
