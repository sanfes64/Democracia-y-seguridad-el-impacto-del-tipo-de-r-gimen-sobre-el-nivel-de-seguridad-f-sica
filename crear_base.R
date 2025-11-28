# Creación de la base de datos. 
# En caso de utilizar los índices creados por MCMC se debe remplazar la importación de V-Dem por el output de polyarchy_rep
# Librerias necesarias
library(dplyr)
library(summarytools)
library(tidyr)
library(readr)

# Se importa la base de V-Dem y IHME.
VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")
IHME <- read_csv("IHME-GBD_2021_DATA-59101582-1.csv")
# Filtrar la base de datos
VDEM <- VDEM[VDEM$year >= 1980 & VDEM$year <= 2021, ]
VDEM <- VDEM %>% select(c(country_name,
                          country_id,
                          year,
                          v2x_polyarchy,
                           v2svindep,
                           v2regint
))


# Se transforma los NA de v2regint y v2svindep para evitar problemas al filtrar
VDEM <- VDEM %>%
  mutate(v2regint = ifelse(is.na(v2regint), 1, v2regint),
         v2svindep = ifelse(is.na(v2svindep), 1, v2svindep)) 

# Se filtra la base utilizando v2regint y v2svindep
VDEM <- VDEM %>%
  filter(!(v2regint == 0 | v2svindep == 0))

# Se revisa las diferencias entre los nombres de las unidades de ambas bases. 
# Estos son tomados y analizados para encontrar casos donde un mismo pais se presenta con nombres distintos
nombres_IHME <- unique(IHME$location_name)  
nombres_VDEM <- unique(VDEM$country_name)  
solo_en_IHME <- setdiff(nombres_IHME, nombres_VDEM)  
solo_en_VDEM <- setdiff(nombres_VDEM, nombres_IHME)  

# Se reportan los nombres
cat("Casos que solo están en la IHME:\n")
print(solo_en_IHME)
cat("\nCasos que solo están en la VDEM:\n")
print(solo_en_VDEM)

# Se unifican los nombres de paises tomando como base los utilizados por V-Dem
nombres_a_cambiar <- list("Venezuela (Bolivarian Republic of)" = "Venezuela", 
                          "Republic of Korea" =	"South Korea", 
                          "Republic of Moldova" =	"Moldova",
                          "Iran (Islamic Republic of)" =	"Iran",
                          "Lao People's Democratic Republic" =	"Laos",
                          "Viet Nam" =	"Vietnam",
                          "Syrian Arab Republic" =	"Syria",
                          "Russian Federation" =	"Russia",
                          "Democratic People's Republic of Korea" =	"North Korea",
                          "Myanmar" =	"Burma/Myanmar",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "Cabo Verde" =	"Cape Verde",
                          "Côte d'Ivoire" =	"Ivory Coast",
                          "Congo" =	"Republic of the Congo",
                          "Gambia" =	"The Gambia",
                          "United Republic of Tanzania" =	"Tanzania"
)
                          
IHME$location_name <- ifelse(IHME$location_name %in% names(nombres_a_cambiar), 
                     nombres_a_cambiar[IHME$location_name], 
                     IHME$location_name)

#se vuelve a revisar los nombres unicos por los cambios en las bases
nombres_IHME <- unique(IHME$location_name)  
nombres_VDEM <- unique(VDEM$country_name) 
solo_en_IHME <- setdiff(nombres_IHME, nombres_VDEM) 
solo_en_VDEM <- setdiff(nombres_VDEM, nombres_IHME)  

# Se filtran ambas bases conservando solo las unidades comunes
unidades_comunes <- intersect(nombres_IHME, nombres_VDEM) 
VDEM <- VDEM %>% filter(country_name %in% unidades_comunes)
IHME <- IHME %>% filter(location_name %in% unidades_comunes)


# Se rota la base de IHME para que coincidan con la de V-Dem
IHME <- IHME %>%
  group_by(location_name, year) %>%  
  summarize(
    deaths = sum(val[measure_id == 1], na.rm = TRUE), 
    incidence = sum(val[measure_id == 6], na.rm = TRUE)  
  ) %>%
  ungroup() 

# Se cambia el nombre de la variable location_name de IHME para que coincida con la de V-Dem
IHME <- IHME %>%
  rename(country_name = location_name)
IHME <- IHME %>%
  mutate(country_name = as.character(country_name))

# Se realiza la unión
vdem_ihme <- VDEM %>%
  left_join(IHME, by = c("country_name", "year"))
vdem_ihme$year <-as.numeric(vdem_ihme$year) 

# Se agregar los labels
attr(vdem_ihme$v2x_polyarchy, "label") <- "Democracia electoral V-Dem"
attr(vdem_ihme$v2svindep, "label") <- "Independencia estatal: 1 si, 0 no"
attr(vdem_ihme$v2regint, "label") <- "Regimen identificable: 1 si, 0 no"
attr(vdem_ihme$e_civil_war, "label") <- "Guerra civil: 1 si, 0 no"
attr(vdem_ihme$deaths, "label") <- "% cada 100.000 habitantes"
attr(vdem_ihme$incidence, "label") <- "% cada 100.000 habitantes"

# Se guarda la base

write.csv(vdem_ihme, "vdem_ihme.csv", row.names = FALSE)
