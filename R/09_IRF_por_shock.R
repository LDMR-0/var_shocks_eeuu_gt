library(dplyr)
library(ggplot2)
library(stringr)

# ============================================================
# 1. Limpiar nombres de impulsos y respuestas
# ============================================================

irf_plot <- irf_df |>
    mutate(
        Impulse_clean = sub("^xt_us\\.", "", Impulse),
        Response_clean = sub("^zt_gt\\.", "", Response)
    )


# ============================================================
# 2. Etiquetas de las variables de Guatemala
# ============================================================

etiquetas_respuesta <- c(
    "GT_dlog_imae" = "Actividad económica (IMAE)",
    "GT_dlog_ipc"  = "Inflación (IPC)",
    "GT_dlog_bc"   = "Razón exportaciones/importaciones",
    "GT_d_i"       = "Tasa líder de política monetaria",
    "GT_tc"        = "Tipo de cambio nominal"
)


# Orden de presentación
orden_respuestas <- c(
    "GT_dlog_imae",
    "GT_dlog_ipc",
    "GT_dlog_bc",
    "GT_d_i",
    "GT_tc"
)

irf_plot <- irf_plot |>
    mutate(
        Response_clean = factor(
            Response_clean,
            levels = orden_respuestas
        )
    )


# ============================================================
# 3. Títulos de los shocks
# ============================================================

titulos_shock <- c(
    "US_dlog_indpro" = "Shock asociado a la actividad productiva de Estados Unidos",
    "US_dlog_pce"    = "Shock asociado a la demanda de Estados Unidos",
    "US_d_fedfunds"  = "Shock de política monetaria de Estados Unidos"
)

subtitulos_shock <- c(
    "US_dlog_indpro" = "Innovación en la producción industrial (INDPRO)",
    "US_dlog_pce"    = "Innovación en el gasto de consumo personal real (PCE)",
    "US_d_fedfunds"  = "Innovación en la tasa de fondos federales (FEDFUNDS)"
)


# ============================================================
# 4. Función para graficar una IRF por shock
# ============================================================


graficar_irf_shock <- function(datos, shock) {
    
    datos_shock <- datos |>
        filter(Impulse_clean == shock)
    
    # Verificar que el shock exista
    if (nrow(datos_shock) == 0) {
        stop(paste("No se encontraron observaciones para el shock:", shock))
    }
    
    ggplot(
        datos_shock,
        aes(x = Period, y = Value)
    ) +
        
        # Intervalo de confianza
        geom_ribbon(
            aes(
                ymin = Lower,
                ymax = Upper
            ),
            fill = "#A9C6CF",
            alpha = 0.45
        ) +
        
        # Función impulso-respuesta
        geom_line(
            color = "#1F4E5F",
            linewidth = 0.9
        ) +
        
        # Línea de referencia en cero
        geom_hline(
            yintercept = 0,
            color = "#8A3B3B",
            linetype = "dashed",
            linewidth = 0.5
        ) +
        
        # Paneles para las cinco variables de Guatemala
        facet_wrap(
            ~ Response_clean,
            ncol = 2,
            scales = "free_y",
            labeller = labeller(
                Response_clean = as_labeller(etiquetas_respuesta)
            )
        ) +
        
        # Eje horizontal
        scale_x_continuous(
            breaks = seq(
                0,
                max(datos_shock$Period),
                by = 2
            ),
            expand = expansion(mult = c(0.01, 0.02))
        ) +
        
        # Títulos y etiquetas
        labs(
            title = titulos_shock[[shock]],
            subtitle = subtitulos_shock[[shock]],
            x = "Meses después del shock",
            y = "Respuesta",
            caption = paste(
                "Nota: la línea continua representa la respuesta estimada;",
                "el área sombreada corresponde al intervalo de confianza."
            )
        ) +
        
        theme_minimal(base_size = 11) +
        
        theme(
            
            # Título principal centrado
            plot.title = element_text(
                face = "bold",
                size = 14,
                color = "#243238",
                hjust = 0.5,
                margin = margin(b = 4)
            ),
            
            # Subtítulo centrado
            plot.subtitle = element_text(
                size = 10.5,
                color = "#4F5B60",
                hjust = 0.5,
                margin = margin(b = 12)
            ),
            
            # Encabezados de cada panel
            strip.background = element_rect(
                fill = "#D9E4E8",
                color = NA
            ),
            
            strip.text = element_text(
                face = "bold",
                size = 10,
                color = "#243238",
                hjust = 0.5,
                margin = margin(5, 5, 5, 5)
            ),
            
            # Cuadrícula
            panel.grid.major = element_line(
                color = "#E1E6E8",
                linewidth = 0.35
            ),
            
            panel.grid.minor = element_blank(),
            
            # Borde de cada panel
            panel.border = element_rect(
                color = "#BBC5C9",
                fill = NA,
                linewidth = 0.4
            ),
            
            # Ejes
            axis.title = element_text(
                color = "#243238"
            ),
            
            axis.text = element_text(
                color = "#4F5B60"
            ),
            
            # Nota de la figura
            plot.caption = element_text(
                size = 8.5,
                color = "#6B7478",
                hjust = 0,
                margin = margin(t = 10)
            ),
            
            # Separación entre gráficos
            panel.spacing = unit(0.8, "lines"),
            
            # Márgenes de la figura
            plot.margin = margin(
                t = 12,
                r = 15,
                b = 10,
                l = 12
            )
        )
}
# ============================================================
# 5. Generación de gráficas por Shock
# ============================================================

# Shock asociado a INDPRO
fig_indpro <- graficar_irf_shock(
    irf_plot,
    "US_dlog_indpro"
)

fig_indpro



# Shock asociado a PCE
fig_pce <- graficar_irf_shock(
    irf_plot,
    "US_dlog_pce"
)

fig_pce


# Shock de política monetaria
fig_fedfunds <- graficar_irf_shock(
    irf_plot,
    "US_d_fedfunds"
)

fig_fedfunds

# ============================================================
# 5. Generación de imagenes
# ============================================================

ggsave(
    "output/irf_indpro.png",
    fig_indpro,
    width = 9,
    height = 8,
    dpi = 300,
    bg = "white"
)

ggsave(
    "output/irf_pce.png",
    fig_pce,
    width = 9,
    height = 8,
    dpi = 300,
    bg = "white"
)

ggsave(
    "output/irf_fedfunds.png",
    fig_fedfunds,
    width = 9,
    height = 8,
    dpi = 300,
    bg = "white"
)
