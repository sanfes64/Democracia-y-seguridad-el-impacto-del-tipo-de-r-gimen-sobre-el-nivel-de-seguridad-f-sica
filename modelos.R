# ============================================================
# Estimación de modelos y generación de resultados.
#
# Este script corre los modelos:
#   - OLS
#   - Diferencias en Diferencias 
#
# Algunas de las tablas fueron modificadas posteriormente para mejorar el formato y la legibilidad, pero no se modificó el contenido sustantivo.
# ============================================================

# Librerias necesarias
library(fixest)
library(dplyr)
library(modelsummary)
library(psych)
library(tibble)
library(kableExtra)
library(fixest)

vdem_ihme <- vdem_ihme %>%
  mutate(log_viol = log(viol_deaths))
# En caso de no haber generado y guardado la variable de desarrollo en pasos anteriores se corre el siguiente PCA.
variables <- c("urban_pop_wb", "education_index", "inequality_index")
pc1scores <- principal(vdem_ihme[variables], nfactors=1, rotate="varimax", scores=TRUE)
vdem_ihme <- cbind(vdem_ihme, pc1scores$scores)
vdem_ihme$PC1 <- as.numeric(pc1scores$scores[, 1])  # ensure it's a numeric column

# VD: Logarítmo de la tasa de muertes por violencia interpersonal cada 100.000 habitantes
# Modelos de OLS
modelo1 <- feols(log_viol ~ polyarchy + stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract|
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)
summary(modelo1)


modelo2 <- feols(log_viol ~ polyarchy + stability_index + urban_pop_wb + inequality_index +
               education_index + ethnic_fract |
               country_region + year,
               panel.id = ~country_id + year,
               vcov = NW(lag = 2),
             data = vdem_ihme)
summary(modelo2)

modelo3 <- feols(log_viol ~ polyarchy + stability_index + urban_pop_wb + inequality_index +
                education_index + ethnic_fract + as.factor(country_region)|
                year,
                panel.id = ~country_id + year,
                vcov = NW(lag = 2),
              data = vdem_ihme,
              )
summary(modelo3)

modelo4 <- feols(log_viol ~ polyarchy + stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract + as.factor(country_region),
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme,
)
summary(modelo4) 

modelo5 <- feols(log_viol ~ polyarchy + stability_index + PC1 + ethnic_fract|
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)
summary(modelo5)


modelo6 <- feols(log_viol ~ polyarchy + stability_index + PC1 + ethnic_fract |
                   country_region + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)
summary(modelo6)

modelo7 <- feols(
  log_viol ~ polyarchy + stability_index + PC1 + ethnic_fract + as.factor(country_region) | year,
  panel.id = ~country_id + year,
  vcov = NW(lag = 2),
  data = vdem_ihme
)

summary(modelo7)

modelo8 <- feols(
  log_viol ~ polyarchy + stability_index + PC1 + ethnic_fract + as.factor(country_region),
  panel.id = ~country_id + year,
  vcov = NW(lag = 2),
  data = vdem_ihme
)

summary(modelo8)

# Se exporta en LaTex
models <- list(
  "Modelo 1" = modelo1,
  "Modelo 2" = modelo2,
  "Modelo 3" = modelo3,
  "Modelo 4" = modelo4,
  "Modelo 5" = modelo5,
  "Modelo 6" = modelo6,
  "Modelo 7" = modelo7,
  "Modelo 8" = modelo8
)

modelsummary(
  models,
  output = "latex",
  title = "Efecto del nivel de democracia sobre log(muertes por violencia)",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = 'RMSE|Log.Lik|F|Within|R2 Pseudo',  # hide unnecessary GOF
  notes = "Errores estándar robustos de Newey-West con 2 rezagos entre paréntesis. + $p<0.1$, * $p<0.05$, ** $p<0.01$, *** $p<0.001$"
)

# Estimación por Dif-in-Dif
results_theta <- tibble(
  specification = character(),
  theta = numeric(),
  se = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  N = integer()
)

# Se estiman distintas formas paramétricas
estimate_theta_once <- function(data) {
  data <- data %>%
    arrange(country_name, year) %>%
    group_by(country_name) %>%
    mutate(
      log_viol = log(viol_deaths),
      delta_y = log_viol - lag(log_viol),
      delta_d = polyarchy - lag(polyarchy),
      d1 = lag(polyarchy),
      d1_sq = d1^2,
      delta_d_sq = delta_d^2,
      d1_delta = d1 * delta_d,
      sgn = sign(delta_d),
      abs_delta_d = abs(delta_d),
      year_lag = lag(year)
    ) %>%
    ungroup() %>%
    mutate(
      region_f = as.factor(country_region),
      year_f = as.factor(year_lag),
      country_f = as.factor(country_name)
    ) %>%
    filter(
      !is.na(delta_y), !is.na(d1), !is.na(delta_d),
      !is.na(stability_index), !is.na(urban_pop_wb),
      !is.na(inequality_index), !is.na(education_index),
      !is.na(ethnic_fract)
    )
  
  if (nrow(data) < 30) return(NA)
  
  # Se incorporan las variables de control
  formula_flex <- delta_y ~ d1 + d1_sq + delta_d + delta_d_sq + d1_delta +
    stability_index + urban_pop_wb + inequality_index +
    education_index + ethnic_fract|year
  
  model <- lm(formula_flex, data = data)
  
  # Se genera el contrafáctico
  newdata_d0 <- data %>%
    mutate(delta_d = 0, delta_d_sq = 0, d1_delta = 0)
  
  data$g_d1_0 <- predict(model, newdata = newdata_d0)
  
  # Estimador WAOSS
  numerator <- sum(data$sgn * (data$delta_y - data$g_d1_0), na.rm = TRUE)
  denominator <- sum(data$abs_delta_d, na.rm = TRUE)
  theta_hat <- numerator / denominator
  
  return(list(theta = theta_hat, N = nrow(data)))
}

# Se realiza un proceso de bootstrap para estimar el intervalo de confianza del 95%
bootstrap_theta <- function(data, B = 200) {
  set.seed(123)
  country_ids <- unique(data$country_name)
  theta_vals <- numeric(B)
  
  for (b in 1:B) {
    sampled_ids <- sample(country_ids, replace = TRUE)
    boot_data <- data %>%
      filter(country_name %in% sampled_ids) %>%
      group_by(country_name) %>%
      mutate(boot_id = paste0(country_name, "_", cur_group_id())) %>%
      ungroup()
    
    est <- estimate_theta_once(boot_data)
    theta_vals[b] <- est$theta
  }
  
  theta_vals <- na.omit(theta_vals)
  theta_se <- sd(theta_vals)
  theta_ci <- quantile(theta_vals, probs = c(0.025, 0.975))
  main_est <- estimate_theta_once(data)
  
  return(list(
    theta = main_est$theta,
    se = theta_se,
    ci = theta_ci,
    dist = theta_vals,
    N = main_est$N
  ))
}

res_boot <- bootstrap_theta(vdem_ihme, B = 200)

# Resultados
cat("θ (log outcome):", round(res_boot$theta, 4), "\n")
cat("SE (bootstrap):", round(res_boot$se, 4), "\n")
cat("95% CI (bootstrap):", round(res_boot$ci[1], 4), "to", round(res_boot$ci[2], 4), "\n")
cat("N used:", res_boot$N, "\n")

results_theta <- results_theta %>%
  add_row(
    specification = "Modelo 4",
    theta = round(res_boot$theta, 4),
    se = round(res_boot$se, 4),
    ci_lower = round(res_boot$ci[1], 4),
    ci_upper = round(res_boot$ci[2], 4),
    N = res_boot$N
  )



# Se exporta en formato LaTex
results_theta <- results_theta %>%
  mutate(
    z = abs(theta / se),
    signif_star = case_when(
      z >= qnorm(0.9995) ~ "***",  # p < 0.001
      z >= qnorm(0.995)  ~ "**",   # p < 0.01
      z >= qnorm(0.975)  ~ "*",    # p < 0.05
      z >= qnorm(0.95)   ~ "+",    # p < 0.10
      TRUE               ~ ""
    ),
    result_fmt = paste0(
      formatC(theta, format = "f", digits = 3), " ± ",
      formatC(se, format = "f", digits = 3), signif_star
    )
  )

final_table <- results_theta %>%
  select(specification, result_fmt)

kable(final_table, format = "latex", booktabs = TRUE,
      col.names = c("Modelo", "$\\hat{\\theta} \\pm SE$"),
      caption = "Estimaciones del efecto de la democracia sobre la violencia física",
      escape = FALSE) %>%  
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general = "+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001",
    general_title = ""
  )


vdem_ihme <- vdem_ihme %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    polyarchy_lag1 = lag(polyarchy, 1),
    polyarchy_lag2 = lag(polyarchy, 2),
    polyarchy_lag3 = lag(polyarchy, 3),
    polyarchy_lead1 = lead(polyarchy, 1),
    polyarchy_lead2 = lead(polyarchy, 2),
    polyarchy_lead3 = lead(polyarchy, 3),
   forced_labor_male = -1 * forced_labor_male,
   forced_labor_female = -1*forced_labor_female
    
  ) %>%
  ungroup()


# Estimación por OLS
# VD: Trabajo forzoso en hombres
modelo1 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract |
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo2 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract |
                   country_region + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo3 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract + as.factor(country_region) |
                   year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo4 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract + as.factor(country_region),
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo5 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract |
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo6 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract |
                   country_region + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo7 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract + as.factor(country_region) |
                   year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo8 <- feols(forced_labor_male ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract + as.factor(country_region),
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

# Se exporta en LaTex
models <- list(
  "Modelo 1" = modelo1,
  "Modelo 2" = modelo2,
  "Modelo 3" = modelo3,
  "Modelo 4" = modelo4,
  "Modelo 5" = modelo5,
  "Modelo 6" = modelo6,
  "Modelo 7" = modelo7,
  "Modelo 8" = modelo8
)

modelsummary(
  models,
  output = "latex",
  title = "Efecto del nivel de democracia sobre log(muertes por violencia)",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = 'RMSE|Log.Lik|F|Within|R2 Pseudo',  # hide unnecessary GOF
  notes = "Errores estándar robustos de Newey-West con 2 rezagos entre paréntesis. + $p<0.1$, * $p<0.05$, ** $p<0.01$, *** $p<0.001$"
)

# VD: Trabajo forzoso en mujeres
modelo1 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract |
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo2 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract |
                   country_region + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo3 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract + as.factor(country_region) |
                   year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo4 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + urban_pop_wb + inequality_index +
                   education_index + ethnic_fract + as.factor(country_region),
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo5 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract |
                   country_name + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo6 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract |
                   country_region + year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo7 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract + as.factor(country_region) |
                   year,
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

modelo8 <- feols(forced_labor_female ~ polyarchy + polyarchy_lag1 + polyarchy_lag2 + polyarchy_lag3 +
                   polyarchy_lead1 + polyarchy_lead2 + polyarchy_lead3 +
                   stability_index + PC1 + ethnic_fract + as.factor(country_region),
                 panel.id = ~country_id + year,
                 vcov = NW(lag = 2),
                 data = vdem_ihme)

# Se exporta en LaTex
models <- list(
  "Modelo 1" = modelo1,
  "Modelo 2" = modelo2,
  "Modelo 3" = modelo3,
  "Modelo 4" = modelo4,
  "Modelo 5" = modelo5,
  "Modelo 6" = modelo6,
  "Modelo 7" = modelo7,
  "Modelo 8" = modelo8
)

modelsummary(
  models,
  output = "latex",
  title = "Efecto del nivel de democracia sobre log(muertes por violencia)",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = 'RMSE|Log.Lik|F|Within|R2 Pseudo',  # hide unnecessary GOF
  notes = "Errores estándar robustos de Newey-West con 2 rezagos entre paréntesis. + $p<0.1$, * $p<0.05$, ** $p<0.01$, *** $p<0.001$"
)


