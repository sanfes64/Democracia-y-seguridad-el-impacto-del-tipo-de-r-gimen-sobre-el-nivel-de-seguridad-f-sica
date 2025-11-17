# Ajuste de los resultados de MCMC.
# En este caso se utiliza el índice de elecciones limpias, pero es intercambiable por cualquiera de los 3 índices replicados y adaptados
# Librerias necesarias
library(dplyr)
library(zoo)
library(readr)
library(ggplot2)
library(tidyr)

# Se importan las bases necesarias
VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")
vdem_clean <- read_csv("vdem_clean.csv")
#Se cambian los nombres de las variables
vdem_clean <- vdem_clean %>% rename(
    country_name = country_name,
    year = year,
    cleanelec_replica = latent_index,
    cleanelec_replica_lower = lower_95,
    cleanelec_replica_upper = upper_95
  )


# Se une la réplica con el índice original
rescale_df <- VDEM %>%
  select(country_name, year, 
         v2xel_frefair, 
         v2xel_frefair_codelow, 
         v2xel_frefair_codehigh) %>%
  inner_join(vdem_clean, by = c("country_name", "year")) %>%
  mutate(
    cleanelec_replica        = ifelse(is.na(v2xel_frefair), NA, cleanelec_replica),
    cleanelec_replica_lower  = ifelse(is.na(v2xel_frefair_codelow), NA, cleanelec_replica_lower),
    cleanelec_replica_upper  = ifelse(is.na(v2xel_frefair_codehigh), NA, cleanelec_replica_upper)
  )

# Se imputa el valor mínimo para los casos donde no hay régimen electoral
min_val         <- min(rescale_df$cleanelec_replica, na.rm = TRUE)
min_val_lower   <- min(rescale_df$cleanelec_replica_lower, na.rm = TRUE)
min_val_upper   <- min(rescale_df$cleanelec_replica_upper, na.rm = TRUE)

rescale_df <- rescale_df %>%
  left_join(VDEM %>% select(country_name, year, v2x_elecreg, v2regidnr, v2regendtype), 
            by = c("country_name", "year")) %>%
  mutate(
    cleanelec_replica        = ifelse(v2x_elecreg == 0, min_val, cleanelec_replica),
    cleanelec_replica_lower  = ifelse(v2x_elecreg == 0, min_val_lower, cleanelec_replica_lower),
    cleanelec_replica_upper  = ifelse(v2x_elecreg == 0, min_val_upper, cleanelec_replica_upper)
  )

# se calcula el error con respecto al índice original y sus intervalos
rescale_df <- rescale_df %>%
  mutate(
    error = v2xel_frefair - cleanelec_replica,
    error_lower  = v2xel_frefair_codelow - cleanelec_replica_lower,
    error_upper  = v2xel_frefair_codehigh - cleanelec_replica_upper
  )


#loadfonts(device = "win")

# Gráfico de dispersión
ggplot(rescale_df, aes(x = v2xel_frefair, y = cleanelec_replica)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "", x = "Original", y = "Réplica") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )


# Histograma del error
ggplot(rescale_df, aes(x = error)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, fill = "lightgray", color = "white") +
  geom_density(color = "steelblue", linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "", x = "Error (V-Dem − Réplica)", y = "Densidad") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )


#Se crea la correción
error_model <- lm(
  error ~ poly(cleanelec_replica, 3),
  data = rescale_df %>% filter(!is.na(error), !is.na(cleanelec_replica))
)

correction_function <- function(x) {
  ifelse(is.na(x), NA, predict(error_model, newdata = data.frame(cleanelec_replica = x)))
}

# Se aplica la correción
rescale_df <- rescale_df %>%
  mutate(
    correction_center = correction_function(cleanelec_replica),
    correction_lower  = correction_function(cleanelec_replica_lower),
    correction_upper  = correction_function(cleanelec_replica_upper),
    
    cleanelec_replica_corrected        = pmin(pmax(cleanelec_replica        + correction_center, 0), 1),
    cleanelec_replica_lower_corrected  = pmin(pmax(cleanelec_replica_lower  + correction_lower,  0), 1),
    cleanelec_replica_upper_corrected  = pmin(pmax(cleanelec_replica_upper  + correction_upper,  0), 1)
  )

# se crean las penalizaciones para los años de transición
penalty_weights <- c(
  "0" = 1.0, "1" = 0.5, "2" = 1.0, "3" = 0.5, "4" = 0.25,
  "5" = 0.5, "6" = 0.25, "7" = 0.5, "8" = 0.25, "9" = 1.0,
  "10" = 0.25, "11" = 0.5, "12" = 0.25, "13" = 1.0
)

# Se aplica la penalización
rescale_df <- rescale_df %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    lag_elecreg = lag(v2x_elecreg),
    first_electoral_year = suppressWarnings(min(year[v2x_elecreg == 1], na.rm = TRUE)),
    
    # Penalize if: it's electoral (1), preceded by non-electoral (0), but not the first electoral year ever
    penalize_elecreg_0_to_1 = v2x_elecreg == 1 &
      !is.na(lag_elecreg) & lag_elecreg == 0 &
      year != first_electoral_year
  ) %>%
  ungroup()



rescale_df <- rescale_df %>%
  mutate(
    regendtype_str = as.character(v2regendtype),
    penalty_weight = ifelse(penalize_elecreg_0_to_1, penalty_weights[regendtype_str], 0),
    
    penalty_amount_center = cleanelec_replica_corrected^2 * as.numeric(penalty_weight),
    penalty_amount_lower  = cleanelec_replica_lower_corrected^2 * as.numeric(penalty_weight),
    penalty_amount_upper  = cleanelec_replica_upper_corrected^2 * as.numeric(penalty_weight),
    
    cleanelec_replica_corrected_final        = pmax(cleanelec_replica_corrected        - penalty_amount_center, 0),
    cleanelec_replica_lower_corrected_final  = pmax(cleanelec_replica_lower_corrected  - penalty_amount_lower,  0),
    cleanelec_replica_upper_corrected_final  = pmax(cleanelec_replica_upper_corrected  - penalty_amount_upper,  0)
  )

# Se calcula el error
rescale_df <- rescale_df %>%
  mutate(
    error_final        = abs(v2xel_frefair        - cleanelec_replica_corrected_final),
    error_final_lower  = abs(v2xel_frefair_codelow  - cleanelec_replica_lower_corrected_final),
    error_final_upper  = abs(v2xel_frefair_codehigh - cleanelec_replica_upper_corrected_final)
  )

# Se comparan los resultados
cor_before <- cor(rescale_df$v2xel_frefair, rescale_df$cleanelec_replica, use = "complete.obs")
cor_after  <- cor(rescale_df$v2xel_frefair, rescale_df$cleanelec_replica_corrected_final, use = "complete.obs")
mae_before <- mean(abs(rescale_df$cleanelec_replica - rescale_df$v2xel_frefair), na.rm = TRUE)
mae_after  <- mean(abs(rescale_df$cleanelec_replica_corrected_final - rescale_df$v2xel_frefair), na.rm = TRUE)
spearman_before <- cor(rescale_df$v2xel_frefair, rescale_df$cleanelec_replica, use = "complete.obs", method = "spearman")
spearman_after  <- cor(rescale_df$v2xel_frefair, rescale_df$cleanelec_replica_corrected_final, use = "complete.obs", method = "spearman")

cat("Antes de la correción - Correlación:", cor_before, "| Error absoluto:", mae_before, "\n")
cat("Después de la correción - Correlación", cor_after, "| Error absoluto:", mae_after, "\n")


# Se calculan los 30 casos con mayor diferencia absoluta
# Ordenar por mayor error absoluto y seleccionar top 30
top_differences_clean <- rescale_df %>%
  mutate(abs_error = abs(cleanelec_replica_corrected_final - v2xel_frefair)) %>%
  arrange(desc(abs_error)) %>%
  select(country_name, year, 
         Original = v2xel_frefair, 
         Réplica = cleanelec_replica_corrected_final, 
         Diferencia = abs_error) %>%
  head(30)

print(top_differences_clean, n =30)


# Se grafíca el juste de grado 3
ggplot(rescale_df, aes(x = cleanelec_replica, y = error)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), color = "blue", se = FALSE) +
  labs(title = "Curva de ajuste de grado 3",
       x = "Replica", y = "V-Dem - Replica") +
  theme_minimal()


rescale_df <- rescale_df %>%
  mutate(
    abs_error_before = abs(v2xel_frefair - cleanelec_replica),
    abs_error_after = abs(v2xel_frefair - cleanelec_replica_corrected)
  )

# Gráfico de dispersión
ggplot(rescale_df, aes(x = v2xel_frefair, y = cleanelec_replica_corrected)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
    labs(
      x = "Original)",
      y = "Réplica corregida"
    ) +
      theme_minimal(base_family = "Times New Roman") +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(angle = 0, vjust = 0.5)
      )

# Gráfico de densidad
rescale_df <- rescale_df %>%
  mutate(error_after = v2xel_frefair - cleanelec_replica_corrected)

ggplot(rescale_df, aes(x = error_after)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.02, fill = "lightgray", color = "white") +
  geom_density(color = "steelblue", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Error (V-Dem − Réplica corregida)",
    y = "Densidad"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

# Se exportan los resultados
vdem_original_bounds <- VDEM %>%
  select(country_name, year,
         v2xel_frefair,
         v2xel_frefair_codelow,
         v2xel_frefair_codehigh)

export_df <- vdem_original_bounds %>%
  left_join(
    rescale_df %>%
      select(country_name, year,
             cleanelec_replica_corrected_final,
             cleanelec_replica_lower_corrected_final,
             cleanelec_replica_upper_corrected_final),
    by = c("country_name", "year")
  )

export_df <- export_df %>% rename(
  cleanelec_replica = cleanelec_replica_corrected_final,
  cleanelec_replica_codelow = cleanelec_replica_lower_corrected_final,
  cleanelec_replica_codehigh = cleanelec_replica_upper_corrected_final
)
write.csv(export_df, "vdem_clean_final.csv", row.names = FALSE)


# Se carga el nuevo índice
clean <- read_csv("clean.csv")

#Se cambian los nombres de las variables
clean <- clean %>%
  rename(
    cleanelec = latent_index,
    cleanelec_lower = lower_95,
    cleanelec_upper = upper_95
  )

# Se une el nuevo con el índice original
rescale_new <- VDEM %>%
  select(country_name, year, 
         v2xel_frefair, 
         v2xel_frefair_codelow, 
         v2xel_frefair_codehigh) %>%
  inner_join(clean, by = c("country_name", "year")) %>%
  mutate(
    cleanelec        = ifelse(is.na(v2xel_frefair), NA, cleanelec),
    cleanelec_lower  = ifelse(is.na(v2xel_frefair_codelow), NA, cleanelec_lower),
    cleanelec_upper  = ifelse(is.na(v2xel_frefair_codelow), NA, cleanelec_upper)
  )

# Se imputa el valor mínimo para los casos donde no hay régimen electoral
min_val         <- min(rescale_new$cleanelec, na.rm = TRUE)
min_val_lower   <- min(rescale_new$cleanelec_lower, na.rm = TRUE)
min_val_upper   <- min(rescale_new$cleanelec_upper, na.rm = TRUE)

rescale_new <- rescale_new %>%
  left_join(VDEM %>% select(country_name, year, v2x_elecreg, v2regidnr, v2regendtype), 
            by = c("country_name", "year")) %>%
  mutate(
    cleanelec        = ifelse(v2x_elecreg == 0, min_val, cleanelec),
    cleanelec_lower  = ifelse(v2x_elecreg == 0, min_val_lower, cleanelec_lower),
    cleanelec_upper  = ifelse(v2x_elecreg == 0, min_val_upper, cleanelec_upper)
  )


# se calcula el error con respecto al índice original y sus intervalos
rescale_new <- rescale_new %>%
  mutate(
    error = v2xel_frefair - cleanelec,
    error_lower  = v2xel_frefair_codelow - cleanelec_lower,
    error_upper  = v2xel_frefair_codehigh - cleanelec_upper
  )

#loadfonts(device = "win")

# Gráfico de dispersión
ggplot(rescale_new, aes(x = v2xel_frefair, y = cleanelec)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "", x = "Original", y = "Nuevo") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )


# Histograma del error
ggplot(rescale_new, aes(x = error)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, fill = "lightgray", color = "white") +
  geom_density(color = "steelblue", linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "", x = "Error (V-Dem − Nuevo)", y = "Densidad") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )


#Se crea la correción
error_model <- lm(
  error ~ poly(cleanelec, 3),
  data = rescale_new %>% filter(!is.na(error), !is.na(cleanelec))
)

correction_function <- function(x) {
  ifelse(is.na(x), NA, predict(error_model, newdata = data.frame(cleanelec = x)))
}

# Se aplica la correción
rescale_new <- rescale_new %>%
  mutate(
    correction_center = correction_function(cleanelec),
    correction_lower  = correction_function(cleanelec_lower),
    correction_upper  = correction_function(cleanelec_upper),
    
    cleanelec_corrected        = pmin(pmax(cleanelec        + correction_center, 0), 1),
    cleanelec_lower_corrected  = pmin(pmax(cleanelec_lower  + correction_lower,  0), 1),
    cleanelec_upper_corrected  = pmin(pmax(cleanelec_upper  + correction_upper,  0), 1)
  )

# se crean las penalizaciones para los años de transición
penalty_weights <- c(
  "0" = 1.0, "1" = 0.5, "2" = 1.0, "3" = 0.5, "4" = 0.25,
  "5" = 0.5, "6" = 0.25, "7" = 0.5, "8" = 0.25, "9" = 1.0,
  "10" = 0.25, "11" = 0.5, "12" = 0.25, "13" = 1.0
)

# Se aplica la penalización
rescale_new <- rescale_new %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    lag_elecreg = lag(v2x_elecreg),
    first_electoral_year = suppressWarnings(min(year[v2x_elecreg == 1], na.rm = TRUE)),
    
    # Penalize if: it's electoral (1), preceded by non-electoral (0), but not the first electoral year ever
    penalize_elecreg_0_to_1 = v2x_elecreg == 1 &
      !is.na(lag_elecreg) & lag_elecreg == 0 &
      year != first_electoral_year
  ) %>%
  ungroup()



rescale_new <- rescale_new %>%
  mutate(
    regendtype_str = as.character(v2regendtype),
    penalty_weight = ifelse(penalize_elecreg_0_to_1, penalty_weights[regendtype_str], 0),
    
    penalty_amount_center = cleanelec_corrected^2 * as.numeric(penalty_weight),
    penalty_amount_lower  = cleanelec_lower_corrected^2 * as.numeric(penalty_weight),
    penalty_amount_upper  = cleanelec_upper_corrected^2 * as.numeric(penalty_weight),
    
    cleanelec_corrected_final        = pmax(cleanelec_corrected        - penalty_amount_center, 0),
    cleanelec_lower_corrected_final  = pmax(cleanelec_lower_corrected  - penalty_amount_lower,  0),
    cleanelec_upper_corrected_final  = pmax(cleanelec_upper_corrected  - penalty_amount_upper,  0)
  )

# Se calcula el error
rescale_new <- rescale_new %>%
  mutate(
    error_final        = abs(v2xel_frefair        - cleanelec_corrected_final),
    error_final_lower  = abs(v2xel_frefair_codelow  - cleanelec_lower_corrected_final),
    error_final_upper  = abs(v2xel_frefair_codehigh - cleanelec_upper_corrected_final)
  )

# Se comparan los resultados
cor_before <- cor(rescale_new$v2xel_frefair, rescale_new$cleanelec, use = "complete.obs")
cor_after  <- cor(rescale_new$v2xel_frefair, rescale_new$cleanelec_corrected_final, use = "complete.obs")
mae_before <- mean(abs(rescale_new$cleanelec - rescale_new$v2xel_frefair), na.rm = TRUE)
mae_after  <- mean(abs(rescale_new$cleanelec_corrected_final - rescale_new$v2xel_frefair), na.rm = TRUE)
spearman_before <- cor(rescale_new$v2xel_frefair, rescale_new$cleanelec, use = "complete.obs", method = "spearman")
spearman_after  <- cor(rescale_new$v2xel_frefair, rescale_new$cleanelec_corrected_final, use = "complete.obs", method = "spearman")

cat("Antes de la correción - Correlación:", cor_before, "| Error absoluto:", mae_before, "\n")
cat("Después de la correción - Correlación", cor_after, "| Error absoluto:", mae_after, "\n")


# Se calculan los 30 casos con mayor diferencia absoluta
# Ordenar por mayor error absoluto y seleccionar top 30
top_differences_clean <- rescale_new %>%
  mutate(abs_error = abs(cleanelec_corrected_final - v2xel_frefair)) %>%
  arrange(desc(abs_error)) %>%
  select(country_name, year, 
         Original = v2xel_frefair, 
         Réplica = cleanelec_corrected_final, 
         Diferencia = abs_error) %>%
  head(30)

print(top_differences_clean, n =30)


# Se grafíca el juste de grado 3
ggplot(rescale_new, aes(x = cleanelec, y = error)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), color = "blue", se = FALSE) +
  labs(title = "Curva de ajuste de grado 3",
       x = "Replica", y = "V-Dem - Nuevo") +
  theme_minimal()


rescale_new <- rescale_new %>%
  mutate(
    abs_error_before = abs(v2xel_frefair - cleanelec),
    abs_error_after = abs(v2xel_frefair - cleanelec_corrected)
  )

# Gráfico de dispersión
ggplot(rescale_new, aes(x = v2xel_frefair, y = cleanelec_corrected)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    x = "Original",
    y = "Nuevo corregido"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

# Gráfico de densidad
rescale_new <- rescale_new %>%
  mutate(error_after = v2xel_frefair - cleanelec_corrected)

ggplot(rescale_new, aes(x = error_after)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.02, fill = "lightgray", color = "white") +
  geom_density(color = "steelblue", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Error (V-Dem − Nuevo corregido)",
    y = "Densidad"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

# Se itenta ver el impacto de la variable eliminada sobre la diferencia
rescale_new <- rescale_new %>%
  left_join(
    VDEM %>% select(country_name, year, v2elintim, v2elpeace),
    by = c("country_name", "year")
  )

rescale_new <- rescale_new %>%
  mutate(diff_index = rescale_new$cleanelec_corrected_final - rescale_df$cleanelec_replica_corrected_final)

model <- lm(diff_index ~ v2elintim + v2elpeace, data = rescale_new)
summary(model)

# Se exportan los resultados
vdem_clean_final <- read_csv("vdem_clean_final.csv")

vdem_clean_final <- vdem_clean_final %>%
  left_join(
    rescale_new %>%
      select(country_name,
             year,
             cleanelec_corrected_final,
             cleanelec_lower_corrected_final,
             cleanelec_upper_corrected_final),
    by = c("country_name", "year")
  )

vdem_clean_final <- vdem_clean_final %>% rename(
  cleanelec = cleanelec_corrected_final,
  cleanelec_codelow = cleanelec_lower_corrected_final,
  cleanelec_codehigh = cleanelec_upper_corrected_final
)

write.csv(vdem_clean_final, "vdem_clean_final.csv", row.names = FALSE)
