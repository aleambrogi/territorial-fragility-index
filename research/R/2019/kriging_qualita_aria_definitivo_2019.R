data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

############################################################
# KRIGING UNIVERSALE Matérn - Multi-inquinante Lombardia
# + GIORNI DI SUPERAMENTO (k-nearest pesato)
############################################################

# ==========================================================
# PACCHETTI
# ==========================================================
library(sf)
library(sp)
library(gstat)
library(dplyr)
library(readxl)
library(readr)
library(openxlsx)
library(janitor)
library(ggplot2)

# ==========================================================
# 1. CONFIGURAZIONE DATI
# ==========================================================

comuni_sf <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                     quiet = TRUE) %>%
  filter(COD_REG == 3) %>%
  st_transform(32632) %>%
  clean_names()

dataset_comunale <- read_excel(file.path(external_root, "[Tesi AMBROGI] Kick off/dataset_2019.xlsx"),
                               sheet = 2, skip = 1) %>% clean_names()

dataset_comunale$pro_com <- as.numeric(dataset_comunale$codice_comune_num)
comuni_sf$pro_com <- as.numeric(comuni_sf$pro_com)

comuni_sf <- comuni_sf %>% left_join(dataset_comunale, by = "pro_com")

cols_to_join <- c(
  "popolazione_1gen2019",
  "densita_abitativa_2019",
  "quota_pop_attiva_19",
  "grado_di_urbanizzazione",
  "altitudine_del_centro_metri",
  "superficie_km2",
  "dinamici_interni_lavoro_2019",
  "dinamici_verso_esterno_lavoro_2019",
  "indice_di_autocontenimento_2019",
  "presenza_stazioni_fs_rfi_trenord_2019",
  "addetti_2019",
  "percent_addetti_alle_imprese_su_popolazione_2019"
)

# ==========================================================
# 2. FUNZIONE KRIGING (INVARIATA)
# ==========================================================
kriging_per_inquinante <- function(file_path, nome_inquinante) {
  
  stations <- read_excel(file_path) %>% clean_names()
  
  stations_sf <- st_as_sf(stations, coords = c("lon","lat"), crs = 4326) %>%
    st_transform(32632)
  
  stations_sf$pollution <- as.numeric(as.character(stations_sf$media_yy))
  stations_sf <- stations_sf[!duplicated(stations_sf$geometry), ]
  
  stations_sf <- st_join(stations_sf, comuni_sf)
  stations_sp <- as(stations_sf, "Spatial")
  
  stations_sp@data <- stations_sp@data %>% select(-any_of(cols_to_join))
  stations_sp@data <- stations_sp@data %>%
    left_join(
      st_drop_geometry(comuni_sf) %>% select(pro_com, all_of(cols_to_join)),
      by = "pro_com"
    )
  
  vgm_emp <- variogram(pollution ~ 1, stations_sp)
  
  vgm_model <- fit.variogram(
    vgm_emp,
    model = vgm(psill = 1, model = "Mat", range = 100000, nugget = 1, kappa = 0.5)
  )
  
  formula_krig <- as.formula(paste("pollution ~", paste(cols_to_join, collapse = " + ")))
  
  comuni_centroid <- st_centroid(comuni_sf)
  comuni_sp <- as(comuni_centroid, "Spatial")
  
  kriging_model <- gstat(formula = formula_krig, data = stations_sp, model = vgm_model)
  pred <- predict(kriging_model, comuni_sp)
  
  pred_mean <- pred$var1.pred
  pred_sd   <- sqrt(pred$var1.var)
  
  comuni_sf[[nome_inquinante]] <- pred_mean
  comuni_sf[[paste0(nome_inquinante,"_sd")]] <- pred_sd
  
  comuni_sf <- comuni_sf %>%
    mutate(decile = ntile(!!sym(nome_inquinante), 10))
  
  p1 <- ggplot(comuni_sf) +
    geom_sf(aes(fill = factor(decile)), color = NA) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +
    theme_minimal() +
    labs(title = paste("Stima", nome_inquinante)) +
    theme(axis.text = element_blank(), axis.title = element_blank())
  
  p2 <- ggplot(comuni_sf) +
    geom_sf(aes(fill = !!sym(paste0(nome_inquinante,"_sd"))), color = NA) +
    scale_fill_viridis_c(option = "plasma") +
    theme_minimal()
  
  return(list(comuni_sf = comuni_sf, map_valori = p1, map_sd = p2))
}

# ==========================================================
# 3. KRIGING
# ==========================================================
inquinanti_files <- list(
  NO2 = file.path(data_root, "DATASET PER AMBIENTE 2019/NO2_2019_filtered.xlsx"),
  PM25 = file.path(data_root, "DATASET PER AMBIENTE 2019/PM25_2019_filtered.xlsx"),
  PM10 = file.path(data_root, "DATASET PER AMBIENTE 2019/PM10_2019_filtered.xlsx"),
  O3 = file.path(data_root, "DATASET PER AMBIENTE 2019/O3_2019_filtered.xlsx")
)

results_list <- list()

for(nome in names(inquinanti_files)) {
  cat("Elaborazione:", nome, "\n")
  res <- kriging_per_inquinante(inquinanti_files[[nome]], nome)
  results_list[[nome]] <- res
  print(res$map_valori)
  print(res$map_sd)
}

# ==========================================================
# 4. GIORNI DI SUPERAMENTO (NUOVO BLOCCO)
# ==========================================================

calcola_giorni <- function(file_path, nome_var, k = 5, p = 1) {
  
  stations <- read_excel(file_path) %>% clean_names()
  
  stations_sf <- st_as_sf(stations, coords = c("lon","lat"), crs = 4326) %>%
    st_transform(32632)
  
  centroidi <- st_centroid(comuni_sf)
  dist_matrix <- st_distance(centroidi, stations_sf)
  
  calc_weighted <- function(dist_row, values, k, p) {
    idx <- order(dist_row)[1:k]
    d <- as.numeric(dist_row[idx])
    y <- values[idx]
    d[d == 0] <- 0.0001
    w <- 1 / (d^p)
    sum(w * y, na.rm = TRUE) / sum(w, na.rm = TRUE)
  }
  
  comuni_sf[[nome_var]] <<- apply(dist_matrix, 1, calc_weighted,
                                  values = stations_sf[[nome_var]],
                                  k = k, p = p)
}

cat("Calcolo giorni PM10...\n")
calcola_giorni(inquinanti_files$PM10, "sup45")

cat("Calcolo giorni O3...\n")
calcola_giorni(inquinanti_files$O3, "giorni_mm8_over_100")

# ==========================================================
# 5. UNIONE RISULTATI
# ==========================================================

comuni_kriging <- results_list[[1]]$comuni_sf %>%
  select(pro_com, NO2)

for(nome in c("PM25","PM10","O3")){
  comuni_kriging <- comuni_kriging %>%
    left_join(
      results_list[[nome]]$comuni_sf %>%
        st_drop_geometry() %>%
        select(pro_com, !!sym(nome)),
      by = "pro_com"
    )
}

comuni_kriging <- comuni_kriging %>%
  left_join(
    comuni_sf %>%
      st_drop_geometry() %>%
      select(pro_com, sup45, giorni_mm8_over_100),
    by = "pro_com"
  )

# ==========================================================
# 6. NORMALIZZAZIONE + INDICE
# ==========================================================

comuni_kriging <- comuni_kriging %>%
  mutate(
    NO2_norm  = NO2 / 10,
    PM10_norm = PM10 / 15,
    PM25_norm = PM25 / 5,
    
    PM10_daily = log(sup45 + 1) / log(4 + 1),
    O3_daily   = log(giorni_mm8_over_100 + 1) / log(4 + 1),
    
    YACAQI = NO2_norm +
      PM10_norm +
      PM25_norm +
      PM10_daily +
      O3_daily
  )

# ==========================================================
# 7. SALVATAGGIO
# ==========================================================

write.xlsx(st_drop_geometry(comuni_kriging),
           file.path(data_root, "DATASET PER AMBIENTE 2019/Lombardia_inquinanti_2019.xlsx"),
           overwrite = TRUE)

cat("Elaborazione completata!\n")
