# ============================================================
# Adición y formateo de variables de control.
#
# Este script agrega al panel las variables de control utilizadas en el análisis,
# incluyendo la incorporación de datos del WEO (FMI) y del World Bank entre otros.
# También crea las etiquetas y realiza las transformaciones necesarias.
#
# Este archivo llama automáticamente a:
#   - variable_weo.R
#   - variable_wb.R
# ============================================================

# Librerias necesarias
library(dplyr)
library(summarytools)
library(tidyr)
library(readr)
library(zoo)
library(DescTools)
library(countrycode)

vdem_ihme <- read_csv("vdem_ihme.csv")

# Porcentaje anual de variación del IPC tomado del Banco Mundial (BM/WB)
ruta_archivo <- "Economia/inflacion/inflacion wb.csv"
nombre_asignado <- "cpi_wb"
avg <- TRUE
source("variable_wb.R")
vdem_ihme$cpi_wb_5yr_avg <- as.double(vdem_ihme$cpi_wb_5yr_avg)


# Porcentaje anual de variación del IPC tomado de Perspectivas de la Economía Mundial (WEO)
ruta_archivo <- "R/Nueva carpeta/Economia/inflacion/inflacion weo.csv"
nombre_asignado <- "cpi_weo"
avg <- TRUE
source("variable_weo.R")
vdem_ihme$cpi_weo_5yr_avg <- as.double(vdem_ihme$cpi_weo_5yr_avg)

# Deflactor del PBI (BM/WB)
ruta_archivo <- "Economia/inflacion/deflactor wb.csv"
nombre_asignado <- "deflactor_wb"
avg <- TRUE
source("variable_wb.R")
vdem_ihme$deflactor_wb_5yr_avg <- as.double(vdem_ihme$deflactor_wb_5yr_avg)


vdem_ihme$avg_inflation_wb <- ifelse(
  !is.na(vdem_ihme$cpi_wb_5yr_avg) & !is.na(vdem_ihme$deflactor_wb_5yr_avg),
  (vdem_ihme$cpi_wb_5yr_avg + vdem_ihme$deflactor_wb_5yr_avg) / 2,
  ifelse(
    !is.na(vdem_ihme$cpi_wb_5yr_avg),
    vdem_ihme$cpi_wb_5yr_avg,
    vdem_ihme$deflactor_wb_5yr_avg
  )
)
attr(vdem_ihme$avg_inflation_wb, "label") <- "Inflación anual promediando los datos del cambio porcentual del IPC y el cambio porcentual del deflactor (% anual) fuente WB"

vdem_ihme$inflation_index <- ifelse(
  !is.na(vdem_ihme$avg_inflation_wb) & !is.na(vdem_ihme$cpi_weo_5yr_avg),
  (vdem_ihme$avg_inflation_wb + vdem_ihme$cpi_weo_5yr_avg) / 2,
  ifelse(
    !is.na(vdem_ihme$avg_inflation_wb),
    vdem_ihme$avg_inflation_wb,
    vdem_ihme$cpi_weo_5yr_avg
  )
)

vdem_ihme <- vdem_ihme %>%
  mutate(
    inflation_index = sign(inflation_index) * log(1 + abs(inflation_index))
  )
attr(vdem_ihme$inflation_index, "label") <- "Inflación anual calculada como el promedio del cambio porcentual del IPC y del deflactor del PIB. Se transforma aplicando el logaritmo natural cuando el valor es mayor a 1, y de forma lineal (π – 1) cuando es menor o igual a 1"

vdem_ihme <- vdem_ihme %>% select(-c(cpi_wb_5yr_avg,cpi_weo_5yr_avg,deflactor_wb_5yr_avg,avg_inflation_wb))

# Balance de cuenta corriente (WEO)
ruta_archivo <- "R/Nueva carpeta/Economia/current account/current account_weo.csv"
nombre_asignado <- "current_account_weo"
avg <- TRUE
source("variable_weo.R")
vdem_ihme$current_account_weo_5yr_avg <- as.double(vdem_ihme$current_account_weo_5yr_avg)
attr(vdem_ihme$current_account_weo, "label") <- "Balance de cuenta corriente FMI/WEO"


# Balance de cuenta corriente WB
ruta_archivo <- "R/Nueva carpeta/Economia/current account/current account_wb.csv"
nombre_asignado <- "current_account_wb"
avg <- TRUE
source("variable_wb.R")
vdem_ihme$current_account_wb_5yr_avg <- as.double(vdem_ihme$current_account_wb_5yr_avg)
attr(vdem_ihme$current_account_wb, "label") <- "Balance de cuenta corriente WB"

vdem_ihme$current_account_index <- ifelse(
  !is.na(vdem_ihme$current_account_wb_5yr_avg) & !is.na(vdem_ihme$current_account_weo_5yr_avg),
  (vdem_ihme$current_account_wb_5yr_avg + vdem_ihme$current_account_weo_5yr_avg) / 2,
  ifelse(
    !is.na(vdem_ihme$current_account_wb_5yr_avg),
    vdem_ihme$current_account_wb_5yr_avg,
    vdem_ihme$current_account_weo_5yr_avg
  )
)

attr(vdem_ihme$current_account_index, "label") <- "Balance de las sumas de las exportaciones netas de bienes y servicios, el ingreso primario y secundario. Promedio entre las variables del WB y el WEO. (% del PBI)"
vdem_ihme <- vdem_ihme %>% select(-c(current_account_weo_5yr_avg,current_account_wb_5yr_avg))

vdem_ihme <- vdem_ihme %>%
  mutate(
    current_account_index = sign(current_account_index) * log(1 + abs(current_account_index))
  )

# Crecimiento economico
ruta_archivo <- "R/Nueva carpeta/Economia/crecimiento anual/economic growth wb.csv"
nombre_asignado <- "economic_growth_wb"
avg <- TRUE
source("variable_wb.R")
vdem_ihme$economic_growth_wb_5yr_avg <- vdem_ihme$growth_5yr_avg
vdem_ihme$economic_growth_wb_5yr_avg <- as.double(vdem_ihme$economic_growth_wb_5yr_avg)


vdem_ihme <- vdem_ihme %>%
  mutate(
    growth_5yr_avg = sign(growth_5yr_avg) * log(1 + abs(growth_5yr_avg))
  )
vdem_ihme$economic_growth_wb_5yr_avg <- vdem_ihme$growth_5yr_avg
attr(vdem_ihme$economic_growth_wb_5yr_avg, "label") <- "Promedio móvil de 5 años del crecimiento anual del PIB a dolar constante de 2015. Datos del WB (promedio movil del % de crecimiento del PBI)"

#Se invierte la inflacion para que concuerde en sentido con las demas variables de indice
vdem_ihme <- vdem_ihme %>%
  mutate(inflation_index = -inflation_index)

vdem_ihme <- vdem_ihme %>%
  mutate(
    stability_index = rowMeans(
      cbind(
        as.numeric(scale(inflation_index)[,1]),
        as.numeric(scale(current_account_index)[,1]),
        as.numeric(scale(growth_5yr_avg)[,1])
      ),
      na.rm = FALSE
    )
  )

attr(vdem_ihme$stability_index, "label") <- "Índice de estabilidad económica calculado como el promedio estandarizado de inflación transformada (log simétrico), cuenta corriente (% del PIB) y crecimiento económico (promedio móvil de 5 años). Valores mayores indican mayor estabilidad."
vdem_ihme <- vdem_ihme %>% select(-c(cpi_wb, cpi_weo, deflactor_wb, current_account_weo, current_account_wb, economic_growth_wb, v2svindep, v2regint, e_civil_war))


# 1. Agregamos el ISO3 a la base vdem_ihme
vdem_ihme <- vdem_ihme %>%
  mutate(iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))

# 2. Agregamos el nombre del indicador UNESCO para claridad
unesco <- unesco %>%
  mutate(education_unesco = value) %>%  # renombramos
  select(geoUnit, year, education_unesco)

# 3. Hacemos el merge por ISO3 y año, manteniendo nombres originales de V-Dem
vdem_ihme <- vdem_ihme %>%
  left_join(unesco, by = c("iso3c" = "geoUnit", "year" = "year"))


# 1. Escalar las tres variables de educación por separado (z-scores)
vdem_ihme <- vdem_ihme %>%
  mutate(
    edu_unesco_z = as.numeric(scale(education_unesco)),
    edu_hdi_z = as.numeric(scale(education_hdi)),
    edu_pwt_z = as.numeric(scale(education_pwt))
  )

# 2. Combinar las tres maximizando cobertura
vdem_ihme <- vdem_ihme %>%
  rowwise() %>%
  mutate(
    education_index = mean(c_across(c(edu_unesco_z, edu_hdi_z, edu_pwt_z)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-edu_unesco_z, -edu_hdi_z, -edu_pwt_z)  # eliminamos columnas auxiliares


# Se guarda la base
write.csv(vdem_ihme, "vdem_ihme.csv", row.names = FALSE)

