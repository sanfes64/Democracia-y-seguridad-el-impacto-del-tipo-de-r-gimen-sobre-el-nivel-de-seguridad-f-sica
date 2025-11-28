# ============================================================
# Análisis de los supuestos del modelo de Diferencias en Diferencias.
#
# Este script evalúa los cuatro supuestos clave del modelo DiD:
#   1) Modelo estático
#   2) Constante de Lipschitz
#   3) Existencia de cuasi-stayers
#   4) Tendencias paralelas
#
# Cada sección está pensada para correrse de manera independiente.
# ============================================================

# Librerias necesarias
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)
library(readr)
library(haven)
library(stargazer)
library(modelsummary)
vdem_ihme <- read_dta("vdem_ihme.dta")
  
# Supuesto de modelo estático
# Se comienza analizando la autocorrelación con test clásicos y luego se analiza utilizando variables lag y lead
model_static_controls <- lm(
  viol_deaths ~ polyarchy + stability_index + inequality_index +
    urban_pop_wb + education_index + ethnic_fract  + polyarchy:country_region + factor(year),
  data = vdem_ihme
)

# Test de Durbin-Watson.
dwtest(model_static_controls, alternative = "two.sided")

# Test de Ljung-Box Q 
residuals_static <- resid(model_static_controls)

Box.test(residuals_static, lag = 1, type = "Ljung-Box") 
Box.test(residuals_static, lag = 2, type = "Ljung-Box")
Box.test(residuals_static, lag = 3, type = "Ljung-Box")
Box.test(residuals_static, lag = 4, type = "Ljung-Box")
Box.test(residuals_static, lag = 5, type = "Ljung-Box")
Box.test(residuals_static, lag = 6, type = "Ljung-Box")
Box.test(residuals_static, lag = 7, type = "Ljung-Box")
Box.test(residuals_static, lag = 8, type = "Ljung-Box")
Box.test(residuals_static, lag = 9, type = "Ljung-Box")
Box.test(residuals_static, lag = 10, type = "Ljung-Box")  


vdem_ihme <- vdem_ihme %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(
    poly_lag1 = lag(polyarchy, 1),
    poly_lag2 = lag(polyarchy, 2),
    poly_lag3 = lag(polyarchy, 3)
  ) %>%
  ungroup()

# Se crean las variables lag
vdem_ihme <- vdem_ihme %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(
    poly_lag1 = lag(polyarchy, 1),
    poly_lag2 = lag(polyarchy, 2),
    poly_lag3 = lag(polyarchy, 3)
  ) %>%
  ungroup()

vdem_ihme <- vdem_ihme %>%
  mutate(
    polyarchy = as.numeric(polyarchy),
    poly_lag1 = as.numeric(poly_lag1),
    poly_lag2 = as.numeric(poly_lag2),
    poly_lag3 = as.numeric(poly_lag3),
    country_region = as.factor(country_region)
  )

# Se transforman las variables para el modelo.
vdem_ihme <- vdem_ihme %>%
  mutate(
    forced_labor_male_scaled = (forced_labor_male - min(forced_labor_male, na.rm = TRUE)) /
      (max(forced_labor_male, na.rm = TRUE) - min(forced_labor_male, na.rm = TRUE)),
    forced_labor_female_scaled = (forced_labor_female - min(forced_labor_female, na.rm = TRUE)) /
      (max(forced_labor_female, na.rm = TRUE) - min(forced_labor_female, na.rm = TRUE)),
    log_viol_deaths = log(viol_deaths + 1e-6),
    log_forced_labor_male = log(forced_labor_male_scaled + 1e-6),
    log_forced_labor_female = log(forced_labor_female_scaled + 1e-6)
  )

# Estimaciones
model_viol <- lm(viol_deaths ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                   stability_index + inequality_index +
                   urban_pop_wb + education_index + ethnic_fract +
                   country_region + polyarchy:country_region + factor(year),
                 data = vdem_ihme, na.action = na.omit)

model_forced_male <- lm(forced_labor_male ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                          stability_index + inequality_index +
                          urban_pop_wb + education_index + ethnic_fract +
                          country_region + polyarchy:country_region + factor(year),
                        data = vdem_ihme, na.action = na.omit)

model_forced_female <- lm(forced_labor_female ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                            stability_index + inequality_index +
                            urban_pop_wb + education_index + ethnic_fract +
                            country_region + polyarchy:country_region + factor(year),
                          data = vdem_ihme, na.action = na.omit)

model_log_viol <- lm(log_viol_deaths ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                       stability_index + inequality_index +
                       urban_pop_wb + education_index + ethnic_fract +
                       country_region + polyarchy:country_region + factor(year),
                     data = vdem_ihme, na.action = na.omit)

model_log_forced_male <- lm(log_forced_labor_male ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                              stability_index + inequality_index +
                              urban_pop_wb + education_index + ethnic_fract +
                              country_region + polyarchy:country_region + factor(year),
                            data = vdem_ihme, na.action = na.omit)

model_log_forced_female <- lm(log_forced_labor_female ~ polyarchy + poly_lag1 + poly_lag2 + poly_lag3 +
                                stability_index + inequality_index +
                                urban_pop_wb + education_index + ethnic_fract +
                                country_region + polyarchy:country_region + factor(year),
                              data = vdem_ihme, na.action = na.omit)


keep_coeffs <- function(model, vars) {
  coefs <- names(coef(model))
  to_remove <- setdiff(coefs, vars)
  model$coefficients[to_remove] <- NA
  return(model)
}

# Se modifican los modelos para una mejor visualización.
vars_to_keep <- c("polyarchy", "poly_lag1", "poly_lag2", "poly_lag3")

model_viol_k              <- keep_coeffs(model_viol, vars_to_keep)
model_log_viol_k          <- keep_coeffs(model_log_viol, vars_to_keep)
model_forced_male_k       <- keep_coeffs(model_forced_male, vars_to_keep)
model_log_forced_male_k   <- keep_coeffs(model_log_forced_male, vars_to_keep)
model_forced_female_k     <- keep_coeffs(model_forced_female, vars_to_keep)
model_log_forced_female_k <- keep_coeffs(model_log_forced_female, vars_to_keep)

coef_map <- c(
  "polyarchy"   = "Democracia",
  "poly_lag1"   = "Democracia (t-1)",
  "poly_lag2"   = "Democracia (t-2)",
  "poly_lag3"   = "Democracia (t-3)"
)

# Se crea la tabla de exportación
modelsummary(
  list(
    "Muertes"                  = model_viol,
    "Log(Muertes)"             = model_log_viol,
    "Trabajo forzoso H"        = model_forced_male,
    "Log(Trabajo forzoso H)"   = model_log_forced_male,
    "Trabajo forzoso M"        = model_forced_female,
    "Log(Trabajo forzoso M)"   = model_log_forced_female
  ),
  coef_map = coef_map,
  coef_omit = "^(?!polyarchy|poly_lag1|poly_lag2|poly_lag3).*",  # omit all others
  stars = TRUE,
  gof_omit = "IC|Log|F|RMSE",  # keep N and R2 only
  output = "latex",
  title = "Efecto retardo del nivel de democracia sobre las variables dependientes",
  notes = "Errores estándar entre paréntesis. * p < 0.1, ** p < 0.05, *** p < 0.01"
)

# Se crean las variables lead
vdem_ihme <- vdem_ihme %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(
    poly_lead1 = lead(polyarchy, 1),
    poly_lead2 = lead(polyarchy, 2),
    poly_lead3 = lead(polyarchy, 3)
  ) %>%
  ungroup()

vdem_ihme <- vdem_ihme %>%
  mutate(
    polyarchy = as.numeric(polyarchy),
    poly_lead1 = as.numeric(poly_lead1),
    poly_lead2 = as.numeric(poly_lead2),
    poly_lead3 = as.numeric(poly_lead3),
    country_region = as.factor(country_region)
  )

# Se transforman las variables para el modelo.
vdem_ihme <- vdem_ihme %>%
  mutate(
    forced_labor_male_scaled = (forced_labor_male - min(forced_labor_male, na.rm = TRUE)) /
      (max(forced_labor_male, na.rm = TRUE) - min(forced_labor_male, na.rm = TRUE)),
    forced_labor_female_scaled = (forced_labor_female - min(forced_labor_female, na.rm = TRUE)) /
      (max(forced_labor_female, na.rm = TRUE) - min(forced_labor_female, na.rm = TRUE)),
    log_viol_deaths = log(viol_deaths + 1e-6),
    log_forced_labor_male = log(forced_labor_male_scaled + 1e-6),
    log_forced_labor_female = log(forced_labor_female_scaled + 1e-6)
  )

# Estimaciones.
model_viol <- lm(viol_deaths ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                   stability_index + inequality_index +
                   urban_pop_wb + education_index + ethnic_fract +
                   country_region + polyarchy:country_region + factor(year),
                 data = vdem_ihme, na.action = na.omit)

model_forced_male <- lm(forced_labor_male ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                          stability_index + inequality_index +
                          urban_pop_wb + education_index + ethnic_fract +
                          country_region + polyarchy:country_region + factor(year),
                        data = vdem_ihme, na.action = na.omit)

model_forced_female <- lm(forced_labor_female ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                            stability_index + inequality_index +
                            urban_pop_wb + education_index + ethnic_fract +
                            country_region + polyarchy:country_region + factor(year),
                          data = vdem_ihme, na.action = na.omit)

model_log_viol <- lm(log_viol_deaths ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                       stability_index + inequality_index +
                       urban_pop_wb + education_index + ethnic_fract +
                       country_region + polyarchy:country_region + factor(year),
                     data = vdem_ihme, na.action = na.omit)

model_log_forced_male <- lm(log_forced_labor_male ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                              stability_index + inequality_index +
                              urban_pop_wb + education_index + ethnic_fract +
                              country_region + polyarchy:country_region + factor(year),
                            data = vdem_ihme, na.action = na.omit)

model_log_forced_female <- lm(log_forced_labor_female ~ polyarchy + poly_lead1 + poly_lead2 + poly_lead3 +
                                stability_index + inequality_index +
                                urban_pop_wb + education_index + ethnic_fract +
                                country_region + polyarchy:country_region + factor(year),
                              data = vdem_ihme, na.action = na.omit)

coef_map <- c(
  "polyarchy"   = "Democracia",
  "poly_lead1"  = "Democracia (t+1)",
  "poly_lead2"  = "Democracia (t+2)",
  "poly_lead3"  = "Democracia (t+3)"
)

# Se crea la tabla de exportación
modelsummary(
  list(
    "Muertes"                  = model_viol,
    "Log(Muertes)"             = model_log_viol,
    "Trabajo forzoso H"        = model_forced_male,
    "Log(Trabajo forzoso H)"   = model_log_forced_male,
    "Trabajo forzoso M"        = model_forced_female,
    "Log(Trabajo forzoso M)"   = model_log_forced_female
  ),
  coef_map = coef_map,
  coef_omit = "^(?!polyarchy|poly_lead1|poly_lead2|poly_lead3).*",
  stars = TRUE,
  gof_omit = "IC|Log|F|RMSE",
  output = "latex",
  title = "Efecto anticipado del nivel de democracia sobre las variables dependientes",
  notes = "Errores estándar entre paréntesis. * p < 0.1, ** p < 0.05, *** p < 0.01"
)



# Supuesto de la constante de Lipschitz 
# Librerias necesarias
library(dplyr)
library(ggplot2)

# Se definen la variable dependiente e independiente.
treatment_var <- "polyarchy"
outcome_var   <- "log_viol_deaths"

# Se utiliza una submuestra para estimar y evitar problemas con la sesión de R.
set.seed(123)
data_sub <- vdem_ihme %>%
  select(country_id, year, !!sym(treatment_var), !!sym(outcome_var)) %>%
  na.omit() %>%
  sample_n(500) 

# Se generan los pares de diferencias para estimar la variación.
pairs <- expand.grid(i = 1:nrow(data_sub), j = 1:nrow(data_sub)) %>%
  filter(i < j) %>%
  mutate(
    delta_d = abs(data_sub[[treatment_var]][i] - data_sub[[treatment_var]][j]),
    delta_y = abs(data_sub[[outcome_var]][i] - data_sub[[outcome_var]][j]),
    slope   = delta_y / delta_d
  ) %>%
  filter(delta_d > 0 & is.finite(slope))  # remove identical treatment or NaNs

ggplot(pairs, aes(x = delta_d, y = slope)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
  labs(
    title = "Empirical Check for Lipschitz Continuity",
    x = "Cambio en el tratamiento (|ΔD|)",
    y = "Cambio en el resultado / Cambio en el tratamiento (|ΔY| / |ΔD|)"
  ) +
  theme_minimal()
# Se estima el máximo local de la pendiente.
# Se eliminan los outliers para no sesgar el resultado.
# Estimate max local slope (trimmed to avoid outliers)
quantile(pairs$slope, probs = c(0.95, 0.99), na.rm = TRUE)
Y_bar <- quantile(pairs$slope, 0.99, na.rm = TRUE)
cat("Estimated Lipschitz constant (99th percentile):", round(Y_bar, 3), "\n")

ggplot(pairs, aes(x = abs(d_diff), y = abs(slope))) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = L_hat, linetype = "dashed", color = "red") +
  labs(x = "|ΔD|", y = "|ΔY / ΔD|", title = "Testeo de la Condición de Lipschitz")

# Tendencias paralelas
# Librerias necesarias
library(fixest)
library(dplyr)
library(modelsummary)

# Se generan valores lag.
vdem_ihme <- vdem_ihme %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(
    polyarchy_t0 = lag(polyarchy, 2),
    polyarchy_t1 = lag(polyarchy, 1),
    viol_deaths_t0 = lag(viol_deaths, 2),
    viol_deaths_t1 = lag(viol_deaths, 1)
  ) %>%
  ungroup()

# Se crea el test de placebo.
vdem_ihme <- vdem_ihme %>%
  mutate(
    delta_d_placebo = polyarchy_t1 - polyarchy_t0,
    delta_y_placebo = log(viol_deaths_t1 + 1) - log(viol_deaths_t0 + 1),
    polyarchy_lag = polyarchy_t0,
    quasi_stayer_lag = abs(delta_d_placebo) < 0.01
  )

# Se filtra por cuasi-stayers
placebo_data <- vdem_ihme %>%
  filter(quasi_stayer_lag == TRUE)

# Se estima agregando controles.
modelo_placebo <- feols(
  delta_y_placebo ~ polyarchy_lag + delta_d_placebo + I(polyarchy_lag^2) + I(delta_d_placebo^2) +
    stability_index + urban_pop_wb + inequality_index + education_index + ethnic_fract |
    country_region,
  cluster = ~country_region,
  data = placebo_data
)

summary(modelo_placebo)

# Se crea la tabla de exportación a LaTex
coef_map <- c(
  "polyarchy_lag" = "Lag Democracia",
  "delta_d_placebo" = "$\\Delta$ Democracia (placebo)",
  "I(polyarchy_lag^2)" = "(Lag Democracia)$^2$",
  "I(delta_d_placebo^2)" = "($\\Delta$ Democracia)$^2$",
  "stability_index" = "Estabilidad económica",
  "urban_pop_wb" = "% población urbana",
  "inequality_index" = "Desigualdad",
  "education_index" = "Educación",
  "ethnic_fract" = "Fragmentación étnica"
)

modelsummary(
  modelo_placebo,
  coef_map = coef_map,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  vcov = ~country_region,  # clustered SEs
  output = "latex"
)


# Test de cuasi-stayers
# Librerias necesarias
library(dplyr)
library(tibble)
library(modelsummary)

# Se crean las variables necesarias
vdem_ihme <- vdem_ihme %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    delta_polyarchy = polyarchy - lag(polyarchy),
    polyarchy_lag = lag(polyarchy)
  ) %>%
  ungroup()

# Se verifica que no haya variables stayers
stayers <- vdem_ihme %>%
  group_by(country_name) %>%
  summarise(no_change = all(delta_polyarchy == 0 | is.na(delta_polyarchy))) %>%
  filter(no_change)

cat("Países que nunca cambian su nivel de democracia (stayers):\n")
print(stayers$country_name)

# Se genera la tabla de cuasi-stayers
vdem_ihme <- vdem_ihme %>%
  filter(!is.na(polyarchy_lag), !is.na(delta_polyarchy)) %>%
  mutate(decile = ntile(polyarchy_lag, 10))

tabla_qstayers <- vdem_ihme %>%
  group_by(decile) %>%
  summarise(
    Observaciones = n(),
    `Quasi-stayers` = sum(abs(delta_polyarchy) > 0 & abs(delta_polyarchy) < 0.01),
    `% Quasi-stayers` = round(100 * `Quasi-stayers` / Observaciones, 1)
  )

cat("\nDistribución de cuasi-stayers por decil de democracia (polyarchy_lag):\n")
print(tabla_qstayers)

# Se exporta la tabla
modelsummary(
  list("Distribución de cuasi-stayers por nivel de democracia" = tabla_qstayers),
  output = "quasi_stayers_tabla.tex",
  title = "Distribución de cuasi-stayers por decil de democracia (lag)",
  align = "c",
  booktabs = TRUE,
  notes = "Se considera quasi-stayer si el cambio en democracia absoluta es menor a 0.01.",
  escape = FALSE
)


