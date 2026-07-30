
# Esta función extrae los datos de un objeto IRF (Impulse Response Function)
# y los organiza en un data frame para facilitar su análisis y visualización.
extract_irf_data <- function(irf_obj) {
  impulse_names <- names(irf_obj$irf)
  response_names <- colnames(irf_obj$irf[[1]])
  
  df_list <- list()
  
  for (imp in impulse_names) {
    for (res in response_names) {
      period <- 0:(nrow(irf_obj$irf[[imp]]) - 1)
      val <- irf_obj$irf[[imp]][, res]
      lower <- irf_obj$Lower[[imp]][, res]
      upper <- irf_obj$Upper[[imp]][, res]
      
      df_list[[paste(imp, res)]] <- data.frame(
        Period = period,
        Impulse = imp,
        Response = res,
        Value = val,
        Lower = lower,
        Upper = upper
      )
    }
  }
  do.call(rbind, df_list)
}