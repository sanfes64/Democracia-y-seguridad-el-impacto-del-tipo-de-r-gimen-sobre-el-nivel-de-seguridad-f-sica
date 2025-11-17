# Código auxiliar para dar formato a las variables de WEO
# Librerias necesarias
library(dplyr)
library(zoo)
library(rlang)  

variable_weo <- read_delim(ruta_archivo, 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Se modifica el formato de la base para que coincida con el de la base general
variable_weo <- variable_weo %>%
  pivot_longer(
    cols = -all_of(colnames(variable_weo)[1]), 
    names_to = "year",   
    values_to = "value"   
  )
variable_weo <- variable_weo %>%
  mutate(across(everything(), ~ ifelse(. == "no data", NA, .)))

# Se filtran los datos
colnames(variable_weo) <- c("country_name", "year", nombre_asignado)
variable_weo <- variable_weo %>%
  filter(year <= 2021, !is.na(country_name))

# Asegurate de usar los nombres correctos de las columnas con país
nombres_wb <- unique(variable_weo$country_name)
nombres_vdem <- unique(vdem_ihme$country_name)

# 1. Países en V-Dem pero no en WEO
faltan_en_wb <- setdiff(nombres_vdem, nombres_wb)

# 2. Países en WEO pero no en V-Dem
faltan_en_vdem <- setdiff(nombres_wb, nombres_vdem)

cat("En V-Dem pero no en WB:\n")
print(faltan_en_wb)

cat("\nEn WEO pero no en V-Dem:\n")
print(faltan_en_vdem)


# Se unifican los nombres de paises tomando como base los utilizados por V-Dem
nombres_a_cambiar <- c(
  # Ya tenías:
  "Cabo Verde" = "Cape Verde",
  "China, People's Republic of" = "China",
  "Congo, Dem. Rep. of the" = "Democratic Republic of the Congo",
  "Congo, Republic of" = "Republic of the Congo",
  "Czech Republic" = "Czechia",
  "Côte d'Ivoire" = "Ivory Coast",
  "Gambia, The" = "The Gambia",
  "Korea, Republic of" = "South Korea",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Lao P.D.R." = "Laos",
  "Myanmar" = "Burma/Myanmar",
  "Russian Federation" = "Russia",
  "Slovak Republic" = "Slovakia",
  "South Sudan, Republic of" = "South Sudan",
  "São Tomé and Príncipe" = "Sao Tome and Principe",
  "Taiwan Province of China" = "Taiwan",
  "Türkiye, Republic of" = "Türkiye",
  "United States" = "United States of America",
  "Brunei Darussalam" = "Brunei Darussalam", 
  "Bahamas, The" = "Bahamas",
  "Hong Kong SAR" = "Hong Kong",
  "Macao SAR" = "Macao",  
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Lucia" = "St. Lucia",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Palestine, State of" = "Palestine",
  "Venezuela, Bolivarian Republic of" = "Venezuela",
  "Iran, Islamic Republic of" = "Iran",
  "Syrian Arab Republic" = "Syria",
  "Egypt, Arab Republic of" = "Egypt",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Tanzania, United Republic of" = "Tanzania",
  "Moldova, Republic of" = "Moldova",
  "Viet Nam" = "Vietnam",
  "Democratic People's Republic of Korea" = "North Korea",
  "Eswatini" = "Eswatini" ,
  "Côte d'Ivoire" = "Ivory Coast",
  "São Tomé and Príncipe" = "Sao Tome and Principe",
  "T\xfcrkiye, Republic of" = "Türkiye",
  "West Bank and Gaza" = "Palestine",
  "St. Kitts and Nevis" = "St. Kitts and Nevis",
  "St. Lucia" = "St. Lucia",
  "St. Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Brunei Darussalam" = "Brunei Darussalam",
  "Hong Kong" = "Hong Kong",
  "Macao" = "Macao"
)



variable_weo <- variable_weo %>%
  mutate(country_name = ifelse(variable_weo$country_name %in% names(nombres_a_cambiar),
                               nombres_a_cambiar[variable_weo$country_name],
                               variable_weo$country_name))
variable_weo$year <- as.numeric(variable_weo$year)
variable_weo$country_name <- as.character(variable_weo$country_name)

variable_weo[[nombre_asignado]] <- ifelse(
  variable_weo[[nombre_asignado]] %in% c("no data", "n/a", "..", "", NA),
  NA,
  as.numeric(gsub(",", ".", variable_weo[[nombre_asignado]]))
)

# Crear promedio móvil de 5 años por país
variable_weo[[nombre_asignado]] <- as.numeric(variable_weo[[nombre_asignado]])

if (avg == TRUE) {variable_weo <- variable_weo %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    "{nombre_asignado}_5yr_avg" := purrr::map_dbl(row_number(), function(i) {
      x <- .data[[nombre_asignado]]
      if (is.na(x[i])) {
        return(NA_real_)
      }
      desde <- max(1, i - 4)
      valores <- x[desde:i]
      mean(valores, na.rm = TRUE)
    })
  ) %>%
  ungroup()}


#variable_weo <- variable_weo %>%
  #select(-all_of(nombre_asignado))

# Se unifican las bases y se cambia el formato de las variables en caso de ser necesario
vdem_ihme <- vdem_ihme %>%
  left_join(variable_weo, by = c("country_name", "year"))
vdem_ihme[[ncol(vdem_ihme)]] <- gsub(",", ".", vdem_ihme[[ncol(vdem_ihme)]])