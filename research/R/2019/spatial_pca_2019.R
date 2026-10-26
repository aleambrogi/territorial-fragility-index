data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

############################################################
# INDICE AMBIENTALE COMUNALE - SPCA
############################################################

library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(adespatial)
library(spdep)
library(tmap)
library(openxlsx)

############################################################
# 1️⃣ Carica shapefile comuni Lombardia
############################################################

comuni <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                  quiet = TRUE) %>%
  filter(COD_REG == 3) %>%   # Lombardia
  st_transform(32632) %>%
  clean_names()

############################################################
# 2️⃣ Carica dataset ambientale
############################################################

dataset <- read_excel(file.path(data_root, "DATASET PER AMBIENTE/dataset_ambientale_2019.xlsx")) %>%
  clean_names()

# Seleziona solo variabili ambientali
vars_ambientali <- dataset %>%
  select(
    pro_com,
    no2,
    pm25,
    pm10,
    o3,
    aree_protette,
    superficie_a_rischio_di_frane,
    consumo_del_suolo,
    raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante
  )

# Merge con shapefile
comuni_finale <- comuni %>%
  left_join(vars_ambientali, by = c("pro_com"))

############################################################
# 3️⃣ Pulizia dati per SPCA
############################################################

# Seleziono solo le variabili ambientali utili
vars_env <- comuni_finale %>%
  st_drop_geometry() %>%
  select(no2, pm25, pm10, o3, aree_protette)

# Trovo righe senza NA solo su queste
complete_idx <- complete.cases(vars_env)

# Mantieni solo quelle righe
comuni_clean <- comuni_finale[complete_idx, ]
vars_clean   <- vars_env[complete_idx, ]

# Standardizza variabili
X <- scale(vars_clean)

############################################################
# 4️⃣ Costruzione matrice pesi spaziale
############################################################

# Usa centroidi dei comuni
coords <- st_coordinates(st_centroid(comuni_clean))
coords <- as.matrix(coords[, 1:2])  # prendi solo X e Y
mode(coords) <- "numeric"

# Vicini entro 20 km (adatta a seconda della densità dei comuni)
nb_dist <- dnearneigh(coords, 0, 20000)  

# Trasforma in lista pesi
lw <- nb2listw(nb_dist, style = "W", zero.policy = TRUE)

############################################################
# 5️⃣ Spatial PCA
############################################################

# PCA multivariata con autocorrelazione spaziale
ms <- multispati(
  dudi = dudi.pca(X, scannf = FALSE, nf = 5),
  listw = lw,
  scannf = FALSE
)

# Estrai primo componente (indice ambientale)
comuni_clean$indice_ambientale <- ms$li[,1]

############################################################
# 6️⃣ Esporta il dataset completo con indice
############################################################

write.xlsx(comuni_clean, 
           file.path(data_root, "DATASET PER AMBIENTE/dataset_ambientale_completo.xlsx"))

############################################################
# 7️⃣ Mappa dell'indice ambientale
############################################################

tm <- tm_shape(comuni_clean) +
  tm_polygons(
    col = "indice_ambientale",
    palette = "plasma",
    title = "Indice Ambientale (SPCA)",
    id = "comune"
  ) +
  tm_layout(legend.outside = TRUE)

tmap_mode("view")
############################################################
# INDICE AMBIENTALE COMUNALE - SPCA
############################################################

library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(adespatial)
library(spdep)
library(tmap)
library(openxlsx)

############################################################
# 1️⃣ Carica shapefile comuni Lombardia
############################################################

comuni <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                  quiet = TRUE) %>%
  filter(COD_REG == 3) %>%   # Lombardia
  st_transform(32632) %>%
  clean_names()

############################################################
# 2️⃣ Carica dataset ambientale
############################################################

dataset <- read_excel(file.path(data_root, "DATASET PER AMBIENTE/dataset_ambientale_2019.xlsx")) %>%
  clean_names()

# Seleziona solo variabili ambientali
vars_ambientali <- dataset %>%
  select(
    pro_com,
    no2,
    pm25,
    pm10,
    o3,
    aree_protette,
    superficie_a_rischio_di_frane,
    consumo_del_suolo,
    raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante
  )

# Merge con shapefile
comuni_finale <- comuni %>%
  left_join(vars_ambientali, by = "pro_com")

############################################################
# 3️⃣ Pulizia dati per SPCA
############################################################

# Seleziono solo le variabili ambientali utili
vars_env <- comuni_finale %>%
  st_drop_geometry() %>%
  transmute(
    no2 = -no2,
    pm25 = -pm25,
    pm10 = -pm10,
    o3 = -o3,
    aree_protette = aree_protette,
    superficie_a_rischio_di_frane = -superficie_a_rischio_di_frane,
    consumo_del_suolo = -consumo_del_suolo,
    raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante = -raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante
  )

# Trova righe senza NA solo su queste colonne
complete_idx <- complete.cases(vars_env)

# Mantieni solo quelle righe
comuni_clean <- comuni_finale[complete_idx, ]
vars_clean   <- vars_env[complete_idx, ]

# Standardizza variabili
X <- scale(vars_clean)

############################################################
# 4️⃣ Costruzione matrice pesi spaziale
############################################################

# Usa centroidi dei comuni
coords <- st_coordinates(st_centroid(comuni_clean))
coords <- as.matrix(coords[,1:2])
mode(coords) <- "numeric"

# Vicini entro 20 km (adatta a seconda della densità dei comuni)
nb_dist <- dnearneigh(coords, 0, 20000)  

# Trasforma in lista pesi con zero.policy per comuni isolati
lw <- nb2listw(nb_dist, style = "W", zero.policy = TRUE)

############################################################
# 5️⃣ Spatial PCA
############################################################

# PCA multivariata con autocorrelazione spaziale
ms <- multispati(
  dudi = dudi.pca(X, scannf = FALSE, nf = 5),
  listw = lw,
  scannf = FALSE
)

# Estrai primo componente (indice ambientale)
comuni_clean$indice_ambientale <- ms$li[,1]


############################################################
# Mappa dell'indice ambientale
############################################################

tmap_mode("view")

tm <- tm_shape(comuni_clean) +
  tm_polygons(
    col = "indice_ambientale",
    palette = "plasma",
    title = "Indice Ambientale (SPCA)",
    id = "comune"
  ) +
  tm_layout(legend.outside = TRUE)

tmap_save(tm, file.path(data_root, "DATASET PER AMBIENTE/mappa_indice_ambientale.png"),
          width = 2000, height = 1500, dpi = 300)
