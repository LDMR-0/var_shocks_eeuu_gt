# ------------------------------------------------------------------------------
# PRograma de Estudios Superiores (PES)
# Proyecto de Seminario: Transmisión de schocks reales de EE.UU. hacia Guatemala
# Fecha: 23/03/2026
# Atuor: Luis Daniel Monroy Rojas
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
cat("\014")
setwd("C:/Users/danie/Documents/Seminario/Proyecto")
# setwd("C:/Users/Daniel Monroy/Documents/Seminario/Proyecto")

#install.packages("tseries")
# importaciones de librerías
# tidyverse carga: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(vars)
library(tidyverse)
library(readxl)
library(tseries)
library(urca)

cat("\014")

