# ============================================================
# FIGURA 3
# FEVD COMPLETA A 12 MESES
# Todos los shocks del sistema
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)


# ------------------------------------------------------------
# 1. Etiquetas de TODOS los shocks del sistema
# ------------------------------------------------------------

etiquetas_shock_completo <- c(
    
    # Estados Unidos
    "US_dlog_indpro" = "EE. UU.: INDPRO",
    "US_dlog_pce"    = "EE. UU.: PCE",
    "US_d_fedfunds"  = "EE. UU.: FEDFUNDS",
    
    # Guatemala
    "GT_dlog_imae" = "GT: IMAE",
    "GT_dlog_ipc"  = "GT: IPC",
    "GT_dlog_bc"   = "GT: Razón X/M",
    "GT_d_i"       = "GT: Tasa líder",
    "GT_tc"        = "GT: Tipo de cambio"
)


# ------------------------------------------------------------
# 2. Etiquetas de las variables de respuesta
# ------------------------------------------------------------

etiquetas_respuesta_completo <- c(
    "GT_dlog_imae" = "Actividad económica (IMAE)",
    "GT_dlog_ipc"  = "Inflación (IPC)",
    "GT_dlog_bc"   = "Razón exportaciones/importaciones",
    "GT_d_i"       = "Tasa líder de política monetaria",
    "GT_tc"        = "Tipo de cambio nominal"
)


# ------------------------------------------------------------
# 3. Seleccionar las variables de Guatemala
#    e incluir TODOS los shocks
# ------------------------------------------------------------

fevd_completa_12 <- fevd_df |>
    filter(
        Horizon == 12,
        Response_clean %in% names(etiquetas_respuesta_completo),
        Shock_clean %in% names(etiquetas_shock_completo)
    ) |>
    
    mutate(
        
        Response_label = recode(
            Response_clean,
            !!!etiquetas_respuesta_completo
        ),
        
        Shock_label = recode(
            Shock_clean,
            !!!etiquetas_shock_completo
        ),
        
        # Orden de las variables de respuesta
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
        
        # Orden en que aparecerán los componentes
        Shock_label = factor(
            Shock_label,
            levels = c(
                "EE. UU.: INDPRO",
                "EE. UU.: PCE",
                "EE. UU.: FEDFUNDS",
                "GT: IMAE",
                "GT: IPC",
                "GT: Razón X/M",
                "GT: Tasa líder",
                "GT: Tipo de cambio"
            )
        )
    )


# ------------------------------------------------------------
# 4. Comprobar que cada variable suma 100 %
# ------------------------------------------------------------

comprobacion_fevd <- fevd_completa_12 |>
    group_by(Response_label) |>
    summarise(
        Total = sum(Share),
        .groups = "drop"
    )

comprobacion_fevd

# ------------------------------------------------------------
# 5. Paleta
# ------------------------------------------------------------

colores_fevd_completa <- c(
    
    # Estados Unidos
    "EE. UU.: INDPRO"   = "#1F4E5F",
    "EE. UU.: PCE"      = "#5F7F73",
    "EE. UU.: FEDFUNDS" = "#8A3B3B",
    
    # Guatemala
    "GT: IMAE"           = "#718096",
    "GT: IPC"            = "#9AA5B1",
    "GT: Razón X/M"      = "#A8916D",
    "GT: Tasa líder"     = "#667A8A",
    "GT: Tipo de cambio" = "#B8B0A5"
)

# ------------------------------------------------------------
# 6. FEVD completa a 12 meses
# ------------------------------------------------------------

fig_fevd_completa <- ggplot(
    fevd_completa_12,
    aes(
        x = Response_label,
        y = Share,
        fill = Shock_label
    )
) +
    
    geom_col(
        width = 0.7
    ) +
    
    coord_flip() +
    
    scale_fill_manual(
        values = colores_fevd_completa,
        name = NULL
    ) +
    
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 20),
        labels = label_number(
            accuracy = 1,
            suffix = "%"
        ),
        expand = expansion(
            mult = c(0, 0)
        )
    ) +
    
    labs(
        title = "Descomposición completa de la varianza",
        subtitle = "Horizonte de pronóstico de 12 meses",
        x = NULL,
        y = "Porcentaje de la varianza del error de pronóstico"
    ) +
    
    theme_minimal(
        base_size = 10
    ) +
    
    theme(
        
        plot.title = element_text(
            face = "bold",
            size = 13,
            color = "#243238",
            hjust = 0.5
        ),
        
        plot.subtitle = element_text(
            size = 10,
            color = "#4F5B60",
            hjust = 0.5,
            margin = margin(b = 10)
        ),
        
        panel.grid.major.y = element_blank(),
        
        panel.grid.minor = element_blank(),
        
        panel.grid.major.x = element_line(
            color = "#E1E6E8",
            linewidth = 0.35
        ),
        
        axis.text = element_text(
            color = "#4F5B60"
        ),
        
        axis.title = element_text(
            color = "#243238"
        ),
        
        legend.position = "bottom",
        
        legend.text = element_text(
            size = 7.5
        ),
        
        legend.box = "vertical",
        
        plot.margin = margin(
            10, 12, 8, 10
        )
    )


fig_fevd_completa
