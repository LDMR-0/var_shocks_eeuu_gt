# ============================================================
# FIGURA 2
# FEVD al horizonte de 12 meses
# ============================================================

fevd_12 <- fevd_us |>
    filter(Horizon == 12)


fig_fevd_12 <- ggplot(
    fevd_12,
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
        values = colores_shocks,
        name = NULL
    ) +
    
    scale_y_continuous(
        labels = scales::label_number(
            accuracy = 0.1,
            suffix = "%"
        ),
        expand = expansion(
            mult = c(0, 0.08)
        )
    ) +
    
    labs(
        # title = "Importancia de los shocks estadounidenses",
        subtitle = "Descomposición de la varianza a 12 meses",
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
        
        legend.position = "bottom",
        
        legend.text = element_text(
            size = 8
        ),
        
        axis.text = element_text(
            color = "#4F5B60"
        ),
        
        axis.title = element_text(
            color = "#243238"
        )
    )


fig_fevd_12

ggsave(
    "informe_quarto/figuras/fevd_12_meses.png",
    fig_fevd_12,
    width = 6.5,
    height = 4,
    units = "in",
    dpi = 600,
    bg = "white"
)


fevd_obj <- fevd(var_bloques, n.ahead = 12)

fevd_obj

fevd_obj[[1]]
