data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

# ============================
# PACCHETTI
# ============================
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(janitor)

# ============================
# 1. SHAPEFILE
# ============================
comuni_sf <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                     quiet = TRUE) %>%
  st_transform(32632) %>%
  clean_names() %>%
  filter(cod_reg == 3) %>%
  mutate(pro_com = as.character(pro_com))

# ============================
# 2. CARICA INDICI MACROAREE
# ============================
df_indici <- read_excel(file.path(data_root, "MAPPE_INDICI_2021/indice_macroaree_comuni_2021.xlsx")) %>%
  mutate(pro_com = as.character(pro_com))

# ============================
# 3. SELEZIONE MACROINDICI
# ============================
macro_df <- df_indici %>%
  select(pro_com,
         indice_ambientale,
         indice_socio,
         indice_economico,
         indice_servizi)

# ============================
# 4. NORMALIZZAZIONE (RANK 0–1)
# ============================
norm_rank <- function(x){
  n_valid <- sum(!is.na(x))
  
  if(n_valid > 1){
    return((rank(x, na.last = "keep", ties.method = "average") - 1) / (n_valid - 1))
  } else {
    return(rep(0, length(x)))
  }
}

macro_norm <- macro_df

for(v in names(macro_norm)[-1]){
  macro_norm[[v]] <- norm_rank(macro_norm[[v]])
}

# ============================
# 5. DISTANZA DAL PUNTO IDEALE
# ============================
mat <- as.matrix(macro_norm[ , -1])

ideal <- rep(0, ncol(mat))

dist <- sqrt(rowSums((mat - ideal)^2))

# normalizzazione finale 0–1
dist_norm <- (dist - min(dist, na.rm = TRUE)) /
  (max(dist, na.rm = TRUE) - min(dist, na.rm = TRUE))

macro_norm$indice_finale <- dist_norm

# ============================
# 6. JOIN CON SHAPEFILE
# ============================
comuni_sf <- comuni_sf %>%
  left_join(
    macro_norm %>% select(pro_com, indice_finale),
    by = "pro_com"
  )

# ============================
# 7. OUTPUT
# ============================
out_dir <- file.path(data_root, "MAPPA_INDICE_FINALE_2021/")
dir.create(out_dir, showWarnings = FALSE)

# ============================
# 8. MAPPA CONTINUA
# ============================
p_cont <- ggplot(comuni_sf) +
  geom_sf(aes(fill = indice_finale), color = NA) +
  scale_fill_gradient(low = "green", high = "red", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Indice finale di fragilità")

ggsave(paste0(out_dir, "indice_finale_continua.png"),
       p_cont, width = 8, height = 6)

# ============================
# 9. MAPPA DECILI
# ============================
df <- comuni_sf %>%
  mutate(decile = ntile(indice_finale, 10))

p_dec <- ggplot(df) +
  geom_sf(aes(fill = factor(decile)), color = NA) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "grey90") +
  theme_minimal() +
  labs(title = "Indice finale di fragilità (decili)")

ggsave(paste0(out_dir, "indice_finale_decili.png"),
       p_dec, width = 8, height = 6)

# ============================
# 10. EXPORT
# ============================
write_xlsx(
  macro_norm,
  paste0(out_dir, "indice_finale_comuni.xlsx")
)
