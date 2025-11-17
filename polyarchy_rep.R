# Creación del índice de democracia electoral modificado para evitar problemas de circularidad
# Librerias necesarias
library(dplyr)
library(ggplot2)
library(readr)

# Se importan las subíndices modificados
VDEM <- readRDS("V-Dem-CY-Full+Others-v15.rds")
vdem_free <- read_csv("vdem_free_final.csv")
vdem_clean <- read_csv("vdem_clean_final.csv")
vdem_frassoc <- read_csv("vdem_frassoc_final.csv")

VDEM <- VDEM %>% select(country_name, year, v2x_elecoff,v2x_suffr, v2x_polyarchy)
# Se unifica en un solo df
polyarchy_df <- VDEM %>%
  select(country_name, year, v2x_elecoff, v2x_suffr, v2x_polyarchy) %>%
  left_join(vdem_free %>% select(country_name, year, freexp), by = c("country_name", "year")) %>%
  left_join(vdem_clean %>% select(country_name, year, cleanelec), by = c("country_name", "year")) %>%
  left_join(vdem_frassoc %>% select(country_name, year, frassoc), by = c("country_name", "year"))

# Calculo del índice de democracia electoral modificado siguiendo los criterios de agregación de V-Dem
polyarchy_df <- polyarchy_df %>%
  mutate(
    MPI = v2x_elecoff * cleanelec * frassoc * v2x_suffr * freexp,
    API = (1/8) * v2x_elecoff +
      (1/4) * cleanelec +
      (1/4) * frassoc +
      (1/8) * v2x_suffr +
      (1/4) * freexp,
    v2x_polyarchy_replica = 0.5 * MPI + 0.5 * API
  )

# Se estima la correlación y el error con el original
cor(polyarchy_df$v2x_polyarchy_replica, polyarchy_df$v2x_polyarchy, use = "complete.obs")
mean(abs(polyarchy_df$v2x_polyarchy_replica - polyarchy_df$v2x_polyarchy), na.rm = TRUE)

polyarchy_df <- polyarchy_df %>%
  mutate(abs_diff = abs(v2x_polyarchy - v2x_polyarchy_replica))

top50_all_years <- polyarchy_df %>%
  arrange(desc(abs_diff)) %>%
  select(country_name, year, v2x_polyarchy, v2x_polyarchy_replica, abs_diff) %>%
  head(50)

polyarchy_post1980 <- polyarchy_df %>% filter(year >= 1980)

cor_post1980 <- cor(polyarchy_post1980$v2x_polyarchy,
                    polyarchy_post1980$v2x_polyarchy_replica,
                    use = "complete.obs")

mae_post1980 <- mean(abs(polyarchy_post1980$v2x_polyarchy - polyarchy_post1980$v2x_polyarchy_replica),
                     na.rm = TRUE)

cat("Correlation since 1980:", cor_post1980, "| MAE:", mae_post1980, "\n")


top50_post1980 <- polyarchy_post1980 %>%
  mutate(abs_diff = abs(v2x_polyarchy - v2x_polyarchy_replica)) %>%
  arrange(desc(abs_diff)) %>%
  select(country_name, year, v2x_polyarchy, v2x_polyarchy_replica, abs_diff) %>%
  head(50)

polyarchy_df %>%
  mutate(abs_diff = abs(v2x_polyarchy - v2x_polyarchy_replica)) %>%
  filter(abs_diff > 0.1) %>%
  summarise(n_big_diff = n(),
            pct = 100 * n() / nrow(polyarchy_df))


ggplot(polyarchy_df, aes(x = abs(v2x_polyarchy - v2x_polyarchy_replica))) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "gray") +
  labs(title = "Distribution of Absolute Differences",
       x = "|Original − Replica|",
       y = "Number of cases") +
  theme_minimal()

outliers <- polyarchy_df %>%
  mutate(abs_diff = abs(v2x_polyarchy - v2x_polyarchy_replica)) %>%
  filter(abs_diff > 0.2) %>%
  arrange(desc(abs_diff)) %>%
  select(country_name, year, v2x_polyarchy, v2x_polyarchy_replica, abs_diff)

print(outliers)


write.csv(polyarchy_df, "polyarchy_replica.csv", row.names = FALSE)


