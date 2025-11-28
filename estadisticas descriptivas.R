# ============================================================
# Estadísticas descriptivas univariadas y bivariadas.
#
# Este script genera las exportaciones y tablas de LaTex de las estadísticas descriptivas univariadas y bivariadas.
#
# Se incluye un análisis de posibles outliers.
#
# Algunas de las tablas fueron modificadas posteriormente en TexStudio para mejorar el formato y la legibilidad, pero no se modificó el contenido sustantivo.
# ============================================================

# Librerias necesarias
library(dplyr)
library(haven)
library(stargazer)
library(knitr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(patchwork)
library(extrafont)  
library(Hmisc)
library(xtable)
library(psych)
library(scales)

# Se importa la base
VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")
vdem_ihme <- read_dta("vdem_ihme.dta")
qog_ethnic <- read_excel("qogdata_15_06_2025.xlsx")

vdem_ihme <- vdem_ihme %>%
  left_join(
    VDEM %>%
      select(country_name, year,v2xeg_eqdr, v2clslavem, v2clslavef, e_regionpol),
    by = c("country_name", "year")
  )

# Se analizan las diferencias de nombres entre paises
vdem_names <- unique(vdem_ihme$country_name)
qog_names <- unique(qog_ethnic$cname_qog) 


setdiff(qog_names, vdem_names)
setdiff(vdem_names, qog_names)

# Se igualan los nombres
qog_ethnic <- qog_ethnic %>%
  mutate(cname_qog = case_when(
    cname_qog == "Myanmar"                        ~ "Burma/Myanmar",
    cname_qog == "Congo"                          ~ "Republic of the Congo",
    cname_qog == "Congo, Democratic Republic"     ~ "Democratic Republic of the Congo",
    cname_qog == "Cyprus (1975-)"                 ~ "Cyprus",
    cname_qog == "Czech Republic"                 ~ "Czechia",
    cname_qog == "Ethiopia (-1992)"               ~ "Ethiopia",
    cname_qog == "Ethiopia (1993-)"               ~ "Ethiopia",
    cname_qog == "France (1963-)"                 ~ "France",
    cname_qog == "Gambia"                         ~ "The Gambia",
    cname_qog == "Germany, West"                  ~ "Germany",
    cname_qog == "Cote d'Ivoire"                  ~ "Ivory Coast",
    cname_qog == "Korea, North"                   ~ "North Korea",
    cname_qog == "Korea, South"                   ~ "South Korea",
    cname_qog == "Malaysia (1966-)"               ~ "Malaysia",
    cname_qog == "Malaysia (-1965)"               ~ "Malaysia",
    cname_qog == "Pakistan (1971-)"               ~ "Pakistan",
    cname_qog == "Pakistan (-1970)"               ~ "Pakistan",
    cname_qog == "Sudan (-2011)"                  ~ "Sudan",
    cname_qog == "Eswatini (former Swaziland)"    ~ "Eswatini",
    cname_qog == "Turkey"                         ~ "Türkiye",
    cname_qog == "United States"                  ~ "United States of America",
    cname_qog == "Yemen, North"                   ~ "Yemen",
    cname_qog == "France (-1962)"                 ~ "France",
    cname_qog == "Cyprus (-1974)"                 ~ "Cyprus",
    TRUE ~ cname_qog  # Keep others unchanged
  ))
# Se agrega la variable de fragmentación étnica a la base general
vdem_ihme <- vdem_ihme %>%
  left_join(
    qog_ethnic %>%
      select(country_name = cname_qog, year, fe_etfra ),
    by = c("country_name", "year")
  )

# Se agregan las demás variables de control
vdem_ihme <- vdem_ihme %>%
  rename(
    inequality_index         = v2xeg_eqdr,
    forced_labor_male        = v2clslavem,
    forced_labor_female      = v2clslavef,
    country_region           = e_regionpol,
    ethnic_fract = fe_etfra
  )
vdem_ihme <- vdem_ihme %>%
  select(
    country_name,
    country_id,
    country_region,
    year,
    viol_deaths,
    new_viol_deaths,
    forced_labor_male,
    forced_labor_female,
    polyarchy,
    polyarchy_codelow,
    polyarchy_codehigh,
    stability_index,
    urban_pop_wb,
    inequality_index,
    education_index,
    ethnic_fract
  )

attr(vdem_ihme$country_region, "label") <- "Región política geográfica a la que pertenece la unidad"
attr(vdem_ihme$forced_labor_male, "label") <- "Exposición de los hombres al trabajo forzoso"
attr(vdem_ihme$forced_labor_female, "label") <- "Exposición de las mujeres al trabajo forzoso"
attr(vdem_ihme$inequality_index, "label") <- "Nivel de desigualdad de acceso a recursos"
attr(vdem_ihme$ethnic_fract, "label") <- "Nivel de diversidad étnica. 0 = nula y 1 = gran diversidad"

# Se guardan los resultados (opcional)
saveRDS(vdem_ihme, file = file.path(path, "vdem_ihme.rds"))
write_dta(vdem_ihme, path = file.path(path, "vdem_ihme.dta"))

# Se transforman las variables y se cera la variable de control "Desarrollo"
vdem_ihme <- vdem_ihme %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    forced_labor_male = -1 * forced_labor_male,
    forced_labor_female = -1*forced_labor_female,
    log_viol = log(viol_deaths)
  ) %>%
  ungroup()
variables <- c("urban_pop_wb", "education_index", "inequality_index")
pc1scores <- principal(vdem_ihme[variables], nfactors=1, rotate="varimax", scores=TRUE)
vdem_ihme <- cbind(vdem_ihme, pc1scores$scores)
vdem_ihme$PC1 <- as.numeric(pc1scores$scores[, 1])  # ensure it's a numeric column


# Variables seleccionadas
vars <- vdem_ihme %>%
  select(viol_deaths, log_viol, forced_labor_male, forced_labor_female,
         polyarchy, stability_index, urban_pop_wb,
         inequality_index, education_index,PC1, ethnic_fract)

# Estadísticas descriptivas univariadas
tabla_desc <- data.frame(
  Variable = names(vars),
  Media    = sapply(vars, function(x) mean(x, na.rm = TRUE)),
  Mediana  = sapply(vars, function(x) median(x, na.rm = TRUE)),
  `Desv. Est.` = sapply(vars, function(x) sd(x, na.rm = TRUE)),
  Mínimo   = sapply(vars, function(x) min(x, na.rm = TRUE)),
  Máximo   = sapply(vars, function(x) max(x, na.rm = TRUE)),
  N        = sapply(vars, function(x) sum(!is.na(x)))
)

# Redondear y aplicar formato con coma decimal
tabla_desc_coma <- tabla_desc %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3), decimal.mark = ",", big.mark = ".", trim = TRUE)))

# Exportar tabla a LaTex
library(knitr)
library(kableExtra)

kable(tabla_desc_coma, format = "latex", booktabs = TRUE,
      caption = "Estadísticas descriptivas seleccionadas",
      align = "lcccccc", label = "tabla_desc") %>%
  kable_styling(latex_options = "hold_position")


# Boxplots
variables <- c(
  "viol_deaths", "log_viol", "forced_labor_male", "forced_labor_female",
  "polyarchy", "stability_index", "urban_pop_wb", "inequality_index",
  "education_index", "PC1", "ethnic_fract"
)

titulos <- c(
  "Muertes por violencia física",
  "Muertes por violencia física (log)",
  "Trabajo forzoso (hombres)",
  "Trabajo forzoso (mujeres)",
  "Democracia",
  "Estabilidad económica",
  "Población urbana (%)",
  "Igualdad",
  "Educación",
  "Desarrollo",
  "Fragmentación étnica"
)

names(titulos) <- variables

# Se estandarizan las variables para comprar
vdem_std <- vdem_ihme %>%
  select(all_of(variables)) %>%
  mutate(across(everything(), ~ (.-mean(., na.rm = TRUE))/sd(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = rev(variables), labels = rev(titulos)))

# Se da formato a los boxplots
windowsFonts(Times = windowsFont("Times New Roman"))

p <- ggplot(vdem_std, aes(y = variable, x = value)) +
  geom_boxplot(fill = "gray85", color = "black", outlier.size = 0.8) +
  labs(x = "Desviaciones estándar", y = NULL) +
  theme_minimal(base_size = 16, base_family = "Times") +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Se exportan en formato png
ggsave("boxplots_estandarizados.png", p, width = 12, height = 8, dpi = 300)

# Tabla de paises
nombres_regiones <- c(
  "Europa del Este y Asia Central",
  "América Latina y el Caribe",
  "Norte de África y Medio Oriente",
  "Región subsahariana",
  "Europa Occidental y Norte América",
  "Asia y el Pacífico"
)

# Se asignan los nombres
vdem_ihme <- vdem_ihme %>%
  mutate(region_nombre = factor(country_region, levels = 1:6, labels = nombres_regiones))

# Se agrupan paises
tabla_paises <- vdem_ihme %>%
  select(region_nombre, country_name) %>%
  distinct() %>%
  group_by(region_nombre) %>%
  summarise(Paises = paste(sort(country_name), collapse = ", ")) %>%
  ungroup()

# Se genera tabla LaTeX 
kable(tabla_paises,
      format = "latex",
      booktabs = TRUE,
      escape = TRUE,
      longtable = TRUE,
      col.names = c("Región", "Países"),
      caption = "Países incluidos por región") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"),
                font_size = 9, position = "center") %>%
  column_spec(2, width = "12cm", latex_valign = "top") %>%
  row_spec(0, bold = TRUE)

# Se agregan los nombres a la tabla
tabla_region <- vdem_ihme %>%
  select(country_name, country_region) %>%
  distinct() %>%
  count(country_region, name = "n_paises") %>%
  arrange(country_region) %>%
  mutate(region_nombre = nombres_regiones[country_region]) %>%
  select(region_nombre, n_paises)

tabla_region

# Estadísticas descriptivas bivariadas
# Matriz de correlación
vars <- c(
  "viol_deaths",
  "log_viol",
  "forced_labor_male",    
  "forced_labor_female",  
  "polyarchy",            
  "stability_index",      
  "urban_pop_wb",         
  "inequality_index",     
  "education_index",
  "ethnic_fract"          
)

# Se filtra la base
df_cor <- vdem_ihme %>%
  select(all_of(vars)) %>%
  na.omit()

# Se calcula la matríz de correlaciones
cor_mat <- round(cor(df_cor), 2)

# Se modifican los nombres para mejorar la visualización
nombres_legibles <- c(
  "Muertes por violencia física",
  "Muertes por violencia física (log)",
  "Trabajo forzoso masculino",
  "Trabajo forzoso femenino",
  "Democracia",
  "Estabilidad económica",
  "Población urbana",
  "Igualdad",
  "Educación",
  "Fragmentación étnica"
)

colnames(cor_mat) <- nombres_legibles
rownames(cor_mat) <- nombres_legibles

# Se exporta a LaTex
print(
  xtable(
    cor_mat,
    caption = "Matriz de correlaciones de Pearson entre variables continuas",
    label = "tab:correlaciones"
  ),
  include.rownames = TRUE,
  sanitize.colnames.function = identity,
  caption.placement = "top",
  booktabs = TRUE,
  size = "scriptsize"
)


# Multicolinealidad
#VIF
library(car)

# Variables explicativas
vars_expl <- c("polyarchy", "stability_index", "urban_pop_wb", 
               "inequality_index", "education_index", "ethnic_fract")

# Función para calcular VIF 
get_vif <- function(outcome) {
  formula <- as.formula(paste(outcome, "~", paste(vars_expl, collapse = " + ")))
  modelo <- lm(formula, data = vdem_ihme)
  vif(modelo)
}

# Ser estima el VIF para cada VD
vif_viol <- get_vif("viol_deaths")
vif_forz_m <- get_vif("forced_labor_male")
vif_forz_f <- get_vif("forced_labor_female")

# Se crea una tabla
vif_table <- data.frame(
  Variable = names(vif_viol),
  `Muertes viol.` = round(vif_viol, 2),
  `Forzoso H` = round(vif_forz_m, 2),
  `Forzoso M` = round(vif_forz_f, 2)
)

print(vif_table)

# Descomposición espectral
vars_explicativas <- c("polyarchy", "stability_index", "urban_pop_wb",
                       "inequality_index", "education_index", "ethnic_fract")

# Se filtran los datos
X <- na.omit(vdem_ihme[, vars_explicativas])

# Se calcula la matríz de correlación
cor_matrix <- cor(X)

# Descomposición en autovalores
eig <- eigen(cor_matrix)
eigenvalues <- eig$values

# Se estima el índice de condición
condition_index <- sqrt(max(eigenvalues) / eigenvalues)

# Se crea la tabla
multicol_table <- data.frame(
  Dimension = 1:length(eigenvalues),
  Eigenvalue = round(eigenvalues, 4),
  Condition_Index = round(condition_index, 2)
)

print(multicol_table)

# Outliers
# Se eliminan duplicados
vdem_ihme_clean <- vdem_ihme %>%
  distinct(country_name, year, .keep_all = TRUE)

# Lista de variables
vars_outliers <- c(
  "viol_deaths","log_viol", "forced_labor_male", "forced_labor_female",
  "polyarchy", "stability_index", "urban_pop_wb",
  "inequality_index", "education_index", "PC1", "ethnic_fract"
)

# Función para contar outliers univariados
count_outliers <- function(x) {
  x <- na.omit(x)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper)
}

# Se calculan los outliers totales por variable
outliers_total <- lapply(vars_outliers, function(var) {
  x <- vdem_ihme_clean[[var]]
  data.frame(
    variable = var,
    n_outliers_total = count_outliers(x),
    n_total = sum(!is.na(x))
  )
}) %>%
  bind_rows()

# Se calculan los outliers por región por variable
outliers_by_region <- vdem_ihme_clean %>%
  group_by(country_region) %>%
  summarise(across(all_of(vars_outliers), ~ {
    x <- na.omit(.x)
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    sum(x < lower | x > upper)
  }, .names = "out_{.col}")) %>%
  ungroup()

# Se suman los outliers por región por variable
outliers_region_sum <- outliers_by_region %>%
  summarise(across(starts_with("out_"), sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_outliers_by_region"
  ) %>%
  mutate(variable = gsub("out_", "", variable))

# Se une total + por región
outliers_summary <- outliers_total %>%
  left_join(outliers_region_sum, by = "variable") %>%
  mutate(prop_outliers_total = round(n_outliers_total / n_total, 3),
         prop_outliers_region = round(n_outliers_by_region / n_total, 3))

outliers_summary

# Analisis opcional
top_outliers_list <- lapply(vars_outliers, function(var) {
  df <- vdem_ihme_clean %>%
    select(country_name, year, country_region, !!sym(var)) %>%
    filter(!is.na(!!sym(var))) %>%
    mutate(
      value = !!sym(var),
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outlier = (value < lower | value > upper),
      distance = pmax(abs(value - lower), abs(value - upper))
    ) %>%
    filter(outlier) %>%
    arrange(desc(distance)) %>%
    select(country_name, year, country_region, variable = value, distance) %>%
    mutate(var_name = var) %>%
    slice_head(n = 5)
  
  return(df)
})

top_outliers <- bind_rows(top_outliers_list)

top_outliers

var_labels <- c(
  "Muertes por violencia","Muertes por violencia (log)", "Trabajo forzoso masulino", "Trabajo forzoso femenino",
  "Democracia", "Estabilidad económica", "Población urbana",
  "Igualdad", "Educación", "Desarrollo", "Fragmentación étnica"
)

table_out <- outliers_summary %>%
  mutate(
    `Total de casos` = n_total,
    `Outliers` = n_outliers_total,
    `Outliers (por región)` = n_outliers_by_region,
    `Prop.` = round(prop_outliers_total, 3),
    `Prop. región` = round(prop_outliers_region, 3)
  ) %>%
  select(Variable = variable, `Total de casos`, Outliers, `Outliers (por región)`, `Prop.`, `Prop. región`)

options(OutDec = ".")

stargazer(table_out, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Outliers totales y filtrando por región",
          label = "tab:outliers",
          font.size = "scriptsize",
          header = FALSE)

