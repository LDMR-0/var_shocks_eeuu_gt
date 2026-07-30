extract_irf_data2 <- function(irf_obj) {
    
    impulse_names <- names(irf_obj$irf)
    
    response_names <- colnames(irf_obj$irf[[1]])
    
    # Respaldo por si la matriz no conserva nombres de columnas
    if (is.null(response_names)) {
        response_names <- irf_obj$response
    }
    
    df_list <- list()
    
    for (imp in impulse_names) {
        
        irf_matrix <- as.matrix(irf_obj$irf[[imp]])
        
        for (res in response_names) {
            
            period <- 0:(nrow(irf_matrix) - 1)
            
            val <- as.numeric(irf_matrix[, res])
            
            # Verificar si existen bandas de confianza
            if (
                !is.null(irf_obj$Lower) &&
                !is.null(irf_obj$Upper) &&
                !is.null(irf_obj$Lower[[imp]]) &&
                !is.null(irf_obj$Upper[[imp]])
            ) {
                
                lower <- as.numeric(irf_obj$Lower[[imp]][, res])
                upper <- as.numeric(irf_obj$Upper[[imp]][, res])
                
            } else {
                
                lower <- rep(NA_real_, length(val))
                upper <- rep(NA_real_, length(val))
            }
            
            df_list[[paste(imp, res, sep = "_")]] <- data.frame(
                Period = period,
                Impulse = imp,
                Response = res,
                Value = val,
                Lower = lower,
                Upper = upper,
                stringsAsFactors = FALSE
            )
        }
    }
    
    resultado <- do.call(rbind, df_list)
    rownames(resultado) <- NULL
    
    resultado
}