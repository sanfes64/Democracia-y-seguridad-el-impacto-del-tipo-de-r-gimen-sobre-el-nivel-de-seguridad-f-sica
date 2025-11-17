# Código auxiliar para dar formato a las variables de WB
ruta_archivo <- "inflacion wb.csv"

# Se lee el archivp
variable_wb <- read_delim(ruta_archivo, delim = ";", quote = "")

# Se formatea la base para evitar problemas de compatibilidad
variable_wb <- variable_wb %>%
  mutate_all(~ gsub('\"', "", .))
colnames(variable_wb) <- gsub('\"', "", colnames(variable_wb))

# Se modifica el formato de la base para que coincida con el de la base general
variable_wb <- variable_wb %>%
  pivot_longer(
    cols = matches("^\\d{4} \\[YR\\d{4}\\]"), 
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year))  # extrae y convierte el año a número
  )
variable_wb <- variable_wb %>%
  mutate(across(everything(), ~ ifelse(. == "no data", NA, .)))


# Se filtran los datos
variable_wb <- variable_wb[, colnames(variable_wb) != ""]
variable_wb <- variable_wb %>% select(-c(`Series Name`, `Series Code`, `Country Code`))
variable_wb <- mutate(rename(variable_wb, country_name = `Country Name`))
variable_wb$year <- as.numeric(variable_wb$year)
colnames(variable_wb) <- c("country_name", "year", nombre_asignado)


ifelse(avg == TRUE,
variable_wb <- variable_wb %>%
  filter(year <= 2021, !is.na(country_name)) %>%
  filter(year >= 1975, !is.na(country_name)),
variable_wb <- variable_wb %>%
  filter(year <= 2021, !is.na(country_name)) %>%
  filter(year >= 1980, !is.na(country_name))
)

# Se comprueban las diferencias de nombres entre las bases
variable_wb$year <- as.numeric(variable_wb$year)
nombres_variable_wb <- unique(variable_wb$country_name)  
nombres_vdem_ihme <- unique(vdem_ihme$country_name)  
solo_en_variable_wb <- setdiff(nombres_variable_wb, nombres_vdem_ihme)  

# Se unifican los nombres de paises tomando como base los utilizados por V-Dem
nombres_a_cambiar <- c(
  "Cote d'Ivoire" = "Ivory Coast",
  "Cabo Verde" = "Cape Verde",
  "Cuba" = "Cuba",
  "Gambia, The" = "The Gambia",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Lao PDR" = "Laos",
  "Myanmar" = "Burma/Myanmar",
  "Russian Federation" = "Russia",
  "Sao Tome and Principe" = "Sao Tome and Principe",
  "Slovak Republic" = "Slovakia",
  "Syrian Arab Republic" = "Syria",
  "Turkiye" = "Türkiye",
  "Turkey" = "Türkiye",
  "United States" = "United States of America",
  "Viet Nam" = "Vietnam",
  "Swaziland" = "Eswatini",
  "Czech Republic" = "Czechia",
  "Iran, Islamic Rep." = "Iran",
  "Iran" = "Iran",
  "Korea, Dem. People's Rep." = "North Korea",
  "North Korea" = "North Korea",
  "Korea, Rep." = "South Korea",
  "South Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Moldova" = "Moldova",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Democratic Republic of the Congo" = "Democratic Republic of the Congo",
  "Congo, Rep." = "Republic of the Congo",
  "Republic of the Congo" = "Republic of the Congo",
  "Venezuela, RB" = "Venezuela",
  "Yemen, Rep." = "Yemen",
  "Egypt, Arab Rep." = "Egypt",
  "West Bank and Gaza" = "Palestine",
  "Palestine/Gaza" = "Palestine",
  "Palestine/West Bank" = "Palestine",
  "Iran (Islamic Republic of)" = "Iran",
  "Korea" = "South Korea",
  "Congo" = "Republic of the Congo"
)


variable_wb <- variable_wb %>%
  mutate(country_name = recode(country_name, !!!nombres_a_cambiar))
variable_wb[[nombre_asignado]] <- suppressWarnings(as.numeric(variable_wb[[nombre_asignado]]))


# Crear promedio móvil de 5 años por país
if (avg == TRUE) {
  variable_wb <- variable_wb %>%
    arrange(country_name, year) %>%
    group_by(country_name) %>%
    mutate(
      !!paste0(nombre_asignado, "_5yr_avg") := zoo::rollapplyr(
        .data[[nombre_asignado]],
        width = 5,
        FUN = function(x) {
          if (length(x) < 5 || is.na(x[5])) {
            NA
          } else {
            mean(x, na.rm = TRUE)
          }
        },
        partial = TRUE,
        fill = NA
      )
    ) %>%
    ungroup()
}
nrow(
  variable_wb %>%
    filter(!is.na(.data[[nombre_asignado]]), is.na(.data[[paste0(nombre_asignado, "_5yr_avg")]]))
)

#variable_wb <- variable_wb %>%
  #select(-all_of(nombre_asignado))

# Filtrar luego de calcular el promedio móvil
variable_wb <- variable_wb %>%
  filter(year >= 1980, !is.na(country_name))

variable_wb$year <- as.numeric(variable_wb$year)

# Se unifican las bases
vdem_ihme <- vdem_ihme %>%
  left_join(variable_wb, by = c("country_name", "year"))

vdem_ihme[[ncol(vdem_ihme)]] <- gsub(",", ".", vdem_ihme[[ncol(vdem_ihme)]])