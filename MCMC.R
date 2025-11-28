# ============================================================
# Replicación de índices mediante MCMC (opcional).
#
# Este script implementa un modelo de cadenas de Markov para replicar
# los índices de V-Dem y permitir la eliminación de indicadores considerados
# problemáticos para el análisis.
#
# Este paso es intensivo en memoria y se recomienda limpieza de caché y 
# reiniciar la sesion de R luego de correr el código.
# ============================================================

# Librerias necesarias
library(dplyr)
library(summarytools)
library(tidyr)
library(readr)
library(DescTools)
library(rjags)
library(coda)
library(ggplot2)

VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")

# La variable v2x_elecreg se utiliza únicamente en los índices  Clean elections y Freedom of association thick
#VDEM <- VDEM %>%
  #arrange(country_name, year) %>%
  #group_by(country_name) %>%
  #mutate(
    #regime_change = v2x_elecreg != lag(v2x_elecreg, default = first(v2x_elecreg)),
    #regime_id = cumsum(regime_change)
  #) %>%
  #ungroup()

#VDEM <- VDEM %>%
  #group_by(country_name, regime_id) %>%
  #mutate(v2elmulpar_filled = if (all(is.na(v2elmulpar))) NA_real_ else median(v2elmulpar, na.rm = TRUE)) %>%
  #ungroup()


#VDEM <- VDEM %>%
  #mutate(
    #v2elmulpar_filled = case_when(
      #!is.na(v2elmulpar_filled) ~ v2elmulpar_filled,
      #v2x_elecreg == 0 ~ min(v2elmulpar_filled, na.rm = TRUE),
      #TRUE ~ NA_real_
    #)
  #)

# Se cambian las variabes en función del índice que se busca componer
y_data <- c("v2clacfree",
            "v2cldiscm",
            "v2cldiscw",
            "v2mebias",
            "v2mecenefm",
            "v2mecrit",
            "v2merange",
            "v2meslfcen")
            

# Se filtra la base de datos
df_filtered <- VDEM %>%
  dplyr::select(country_name, year, all_of(y_data))


# Se prepara la matríz para jags
y <- as.matrix(df_filtered %>% select(all_of(y_data)))
n <- nrow(y)
p <- ncol(y)

data_jags <- list(y = y, n = n, p = p)

# Se crea el código de jags
model_string <- "
model {
    for (i in 1:n) {
        for (j in 1:p) {
            mu[i, j] <- gamma[j, 1] + gamma[j, 2] * xi[i]
            y[i, j] ~ dnorm(mu[i, j], tau[j])
        }
    }

    for(i in 1:n) {
        xi[i] ~ dnorm(0,1)
    }

    for (j in 1:p) {
        gamma[j, 1] ~ dnorm(0, 0.01)
        gamma[j, 2] ~ dnorm(0, 0.01) T(0, )
        omega[j] ~ dunif(0, 10)
        tau[j] <- pow(omega[j], -2)
    }
}
"
writeLines(model_string, "factor_model.bug")

# Se generan las MCMC
run_chain <- function(seed, n.iter = 2000) {
  set.seed(seed)
  model <- jags.model("factor_model.bug", data = data_jags, n.chains = 1, n.adapt = 1000)
  update(model, 1000)
  coda.samples(model, variable.names = c("xi"), n.iter = n.iter, thin = 2)
}

# Se corren 3 cadenas
chain1 <- run_chain(111)
chain2 <- run_chain(222)
chain3 <- run_chain(333)

# Se combinan los resultados y se exporta.
# Combine and save chains
samples_combined <- mcmc.list(chain1[[1]], chain2[[1]], chain3[[1]])
saveRDS(samples_combined, "samples_combined_free.rds")  # Save for future use

# Se limpia la memoria para evitar posibles crasheos de la sesión de R.
rm(chain1, chain2, chain3)
gc()

# Se analiza la convergencia del proceso.
gelman.diag(samples_combined[,1:1000])
gelman.plot(samples_combined[,1:5])
plot(samples_combined)

# En caso de haber limpiado la sesión para evitar problmas se carga nuevamente.
samples_combined <- readRDS("samples_combined_free.rds")

# Se define el tamaño de submuestra.
n_params <- ncol(as.matrix(samples_combined))

batch_size <- 1000

# Se corre el diagnostico de Gelman-Rubin en submuestras.
gelman_results <- list()

for (start in seq(1, n_params, by = batch_size)) {
  end <- min(start + batch_size - 1, n_params)
  cat("Checking parameters", start, "to", end, "...\n")
  subset <- samples_combined[, start:end]
  gelman_results[[paste0(start, "-", end)]] <- gelman.diag(subset)
}
all_psrf <- do.call(rbind, lapply(gelman_results, function(x) x$psrf))
summary(all_psrf[, "Point est."])


# Se analizan los resultados ex post
xi_samples <- as.matrix(samples_combined)
xi_mean <- apply(xi_samples, 2, mean)
xi_hpd  <- apply(xi_samples, 2, quantile, probs = c(0.025, 0.975))


all_psrf <- do.call(rbind, lapply(gelman_results, function(x) x$psrf))

# Se presentan los resultados del diagnostico de Gelman-Rubin
ggplot(data.frame(rhat = all_psrf[, "Point est."]), aes(x = rhat)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "white") +
  geom_density(color = "steelblue", size = 1) +
  #geom_vline(xintercept = 1.1, color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Gelman-Rubin R̂ Values",
    x = expression(hat(R)),
    y = "Density"
  ) +
  theme_minimal()

summary(all_psrf[, "Point est."])
table(all_psrf[, "Point est."] > 1.1)

ess <- effectiveSize(samples_combined)
summary(ess)

# Se importa la base de V-Dem para poder combinar los resultados con las unidades país-año.
VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")

df_filtered <- VDEM %>%
  select(country_name, year, all_of(y_data))  

# Add to data frame
df_results <- df_filtered %>%
  select(country_name, year) %>%
  mutate(
    latent_index = xi_mean,
    lower_95 = xi_hpd[1, ],
    upper_95 = xi_hpd[2, ]
  )

# Se exportan los resultados
write.csv(df_results, "assoc.csv", row.names = FALSE)

