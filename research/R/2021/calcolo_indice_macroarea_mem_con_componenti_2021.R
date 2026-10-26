data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

############################################################
# INDICI TERRITORIALI COMUNALI - 4 MACROAREE (MEM + PCA)
# + SCREE PLOT VARIANZA CUMULATA
############################################################

library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(spdep)
library(adespatial)
library(tmap)
library(openxlsx)
library(classInt)
library(RColorBrewer)

############################################################
# 1️⃣ SHAPEFILE COMUNI LOMBARDIA
############################################################

comuni <- st_read(
  file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
  quiet = TRUE
) %>%
  filter(COD_REG == 3) %>%
  st_transform(32632) %>%
  clean_names()

############################################################
# 2️⃣ CARICA DATASET MACROAREE
############################################################

ambiente <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_ambientale_2021.xlsx")) %>% clean_names()
socio    <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_socio_2021.xlsx")) %>% clean_names()
economia <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_economica_2021.xlsx")) %>% clean_names()
servizi  <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_servizi_2021.xlsx")) %>% clean_names()

############################################################
# 3️⃣ MERGE CON SHAPEFILE
############################################################

ambiente_sf <- comuni %>% left_join(ambiente, by="pro_com")
socio_sf    <- comuni %>% left_join(socio, by="pro_com")
economia_sf <- comuni %>% left_join(economia, by="pro_com")
servizi_sf  <- comuni %>% left_join(servizi, by="pro_com")

############################################################
# 4️⃣ MATRICE SPAZIALE PER MEM
############################################################

coords <- st_coordinates(st_centroid(comuni))
coords <- as.matrix(coords[,1:2])
mode(coords) <- "numeric"

nb_dist <- dnearneigh(coords, 0, 20000)
lw <- nb2listw(nb_dist, style="W", zero.policy=TRUE)
mem <- scores.listw(lw)
mem <- as.data.frame(mem)

############################################################
# 5️⃣ FUNZIONE GENERALE CALCOLO INDICE
############################################################

calcola_indice <- function(sf_data, var_mem_threshold = 0.6, max_mem = 5){
  
  df <- sf_data %>% st_drop_geometry()
  
  vars <- df %>%
    select(
      -cod_rip,
      -cod_reg,
      -cod_uts,
      -pro_com,
      -pro_com_t,
      -cc_uts,
      -pop21,
      -fam21,
      -shape_leng,
      -shape_area,
      -comune.x,
      -comune.y,
      -comune_a
    ) %>%
    select(where(is.numeric))
  
  ##########################################################
  # ORIENTAMENTO VARIABILI (ALTO = PIÙ FRAGILITÀ)
  ##########################################################
  
  if("aree_protette" %in% names(vars)) vars$aree_protette <- -vars$aree_protette
  if("saldo_migratorio_2019" %in% names(vars)) vars$saldo_migratorio_2019 <- -vars$saldo_migratorio_2019
  if("tasso_migratorio_totale_2019" %in% names(vars)) vars$tasso_migratorio_totale_2019 <- -vars$tasso_migratorio_totale_2019
  if("saldo_naturale_2019" %in% names(vars)) vars$saldo_naturale_2019 <- -vars$saldo_naturale_2019
  if("tasso_crescita_naturale_2019" %in% names(vars)) vars$tasso_crescita_naturale_2019 <- -vars$tasso_crescita_naturale_2019
  if("quota_pop_attiva_19" %in% names(vars)) vars$quota_pop_attiva_19 <- -vars$quota_pop_attiva_19
  
  ##########################################################
  # RIMOZIONE VARIANZA ZERO
  ##########################################################
  
  vars <- vars[, apply(vars, 2, var, na.rm = TRUE) > 0]
  complete_idx <- complete.cases(vars)
  vars <- vars[complete_idx,]
  geo  <- sf_data[complete_idx,]
  
  ##########################################################
  # STANDARDIZZAZIONE
  ##########################################################
  
  X <- scale(vars)
  
  ##########################################################
  # MEM + PCA
  ##########################################################
  
  mem_filt <- mem[complete_idx, , drop = FALSE]
  mem_vals <- apply(mem_filt, 2, var)
  mem_pos_idx <- which(mem_vals > 0)
  
  cum_mem <- cumsum(mem_vals[mem_pos_idx]) / sum(mem_vals[mem_pos_idx])
  n_mem_use <- which(cum_mem >= var_mem_threshold)[1]
  n_mem_use <- min(n_mem_use, max_mem)
  
  message("Numero di MEM selezionate: ", n_mem_use)
  
  X_mem <- cbind(X, mem_filt[, mem_pos_idx[1:n_mem_use], drop = FALSE])
  
  pca <- prcomp(X_mem, center = TRUE, scale. = TRUE)
  
  ##########################################################
  # VARIANZA SPIEGATA
  ##########################################################
  
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  cum_var <- cumsum(var_exp) * 100
  
  message("Varianza cumulata (%) delle componenti principali:")
  print(round(cum_var, 2))
  
  plot(1:length(var_exp), cum_var, type="b", pch=19, col="blue",
       xlab="Componenti principali", ylab="Varianza cumulata (%)",
       main=paste0("Scree plot - ", deparse(substitute(sf_data))),
       ylim=c(0,100))
  abline(h=80, col="red", lty=2)
  
  ##########################################################
  # CALCOLO INDICE MULTICOMPONENTE
  ##########################################################
  
  k <- which(cum_var >= 80)[1]
  
  message("Numero componenti usate per indice: ", k)
  
  pesi <- var_exp[1:k] / sum(var_exp[1:k])
  
  indice <- as.matrix(pca$x[,1:k]) %*% pesi
  
  indice <- scale(indice)   # standardizzazione finale
  
  geo$indice <- as.numeric(indice)
  
  return(geo)
}

############################################################
# 6️⃣ CALCOLO INDICI
############################################################

amb_ind  <- calcola_indice(ambiente_sf)
soc_ind  <- calcola_indice(socio_sf)
eco_ind  <- calcola_indice(economia_sf)
serv_ind <- calcola_indice(servizi_sf)

############################################################
# 7️⃣ FUNZIONE MAPPA DECILI
############################################################

mappa_decili <- function(obj, titolo, file){
  
  decili <- classIntervals(obj$indice, n=10, style="quantile")$brks
  
  obj$decile <- cut(
    obj$indice,
    breaks = decili,
    include.lowest = TRUE,
    labels = paste0("D",1:10)
  )
  
  palette <- brewer.pal(10,"RdYlGn")
  
  tm <- tm_shape(obj) +
    tm_polygons(
      col="decile",
      palette=palette,
      title=titolo,
      border.col="grey60",
      lwd=0.2
    ) +
    tm_layout(legend.outside=TRUE)
  
  tmap_save(tm, file)
}

############################################################
# 8️⃣ CREA MAPPE
############################################################

mappa_decili(
  amb_ind,
  "Indice Ambientale",
  file.path(data_root, "IMMAGINI 2021/mappa_indice_ambientale_componenti_2021.png")
)

mappa_decili(
  soc_ind,
  "Indice Socio-demografico",
  file.path(data_root, "IMMAGINI 2021/mappa_indice_socio_componenti_2021.png")
)

mappa_decili(
  eco_ind,
  "Indice Economico",
  file.path(data_root, "IMMAGINI 2021/mappa_indice_economico_componenti_2021.png")
)

mappa_decili(
  serv_ind,
  "Indice Servizi",
  file.path(data_root, "IMMAGINI 2021/mappa_indice_servizi_componenti_2021.png")
)

############################################################
# 9️⃣ DATASET FINALE INDICI
############################################################

amb_df <- amb_ind %>%
  st_drop_geometry() %>%
  select(pro_com, comune = comune.x, indice_ambientale = indice)

soc_df <- soc_ind %>%
  st_drop_geometry() %>%
  select(pro_com, indice_socio = indice)

eco_df <- eco_ind %>%
  st_drop_geometry() %>%
  select(pro_com, indice_economico = indice)

serv_df <- serv_ind %>%
  st_drop_geometry() %>%
  select(pro_com, indice_servizi = indice)

indici <- amb_df %>%
  left_join(soc_df, by="pro_com") %>%
  left_join(eco_df, by="pro_com") %>%
  left_join(serv_df, by="pro_com")

write.xlsx(
  indici,
  file.path(data_root, "INDICI 2021/dataset_indici_macroaree_componenti_2021.xlsx")
)

