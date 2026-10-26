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
# 2. CARICAMENTO 4 DATASET
# ============================
ambientale <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_ambientale_2021.xlsx"))
socio <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_socio_2021.xlsx"))
economico <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_economica_2021.xlsx"))
servizi <- read_excel(file.path(data_root, "INDICI 2021/dataset_macroarea_servizi_2021.xlsx"))

# Uniforma chiavi
ambientale <- ambientale %>% mutate(pro_com = as.character(pro_com))
socio <- socio %>% mutate(pro_com = as.character(pro_com))
economico <- economico %>% mutate(pro_com = as.character(pro_com))
servizi <- servizi %>% mutate(pro_com = as.character(pro_com))

# ============================
# 3. GESTIONE RISCHIO
# ============================
trasforma_rischio <- function(df){
  
  if("rischio" %in% names(df)){
    
    # ----------------------------
    # SCALE DIFFERENZIATE
    # ----------------------------
    
    # primo rischio (3 livelli + nullo)
    scala_r1 <- c(
      "Nessun" = 0,
      "Basso" = 1,
      "Medio" = 2,
      "Alto" = 3
    )
    
    # secondo rischio (4 livelli + nullo)
    scala_r2 <- c(
      "Nessun" = 0,
      "Basso" = 1,
      "Medio" = 2,
      "Alto" = 3,
      "Molto alto" = 4
    )
    
    df <- df %>%
      
      # ----------------------------
    # SPLIT
    # ----------------------------
    tidyr::separate(rischio, into = c("r1", "r2"), sep = "\\|", remove = FALSE) %>%
      
      # ----------------------------
    # PULIZIA
    # ----------------------------
    mutate(
      r1 = trimws(r1),
      r2 = trimws(r2)
    ) %>%
      
      # ----------------------------
    # NUMERIZZAZIONE
    # ----------------------------
    mutate(
      r1_num = scala_r1[r1],
      r2_num = scala_r2[r2]
    ) %>%
      
      # ----------------------------
    # NON-COMPENSATORY (MAX)
    # ----------------------------
    mutate(
      rischio_num = pmax(r1_num, r2_num, na.rm = TRUE)
    ) %>%
      
      # ----------------------------
    # CLEAN
    # ----------------------------
    select(-r1, -r2, -r1_num, -r2_num, -rischio)
  }
  
  return(df)
}

ambientale <- trasforma_rischio(ambientale)
socio <- trasforma_rischio(socio)
economico <- trasforma_rischio(economico)
servizi <- trasforma_rischio(servizi)

# ============================
# 4. DIREZIONE FRAGILITÀ
# ============================
direction_list <- list(
  # AMBIENTE
  indice_inquinamento = 1,
  consumo_del_suolo = 1,
  raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante = 1,
  tasso_di_motorizzazione_ad_alta_emissione = 1,
  rischio_num = 1,
  scarto_aree_protette = -1,
  
  # SOCIO
  ind_vecchiaia_21 = 1,
  quota_pop_attiva_21 = -1,
  tasso_crescita_naturale_2021 = -1,
  `Tasso migratorio totale 2021` = 1,
  `Stranieri/pop 2021` = -1,
  `Densità abitativa 2021` = -1,
  
  # ECONOMICO
  reddito_medio_per_contribuente_2021 = -1,
  addetti_100_res = -1,
  imprese_per_1000_ab = -1,
  laureati_per_1000_ab = -1,
  `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2021 (%)` = 1,
  
  # SERVIZI
  indice_di_accessibilita_ai_servizi_essenziali = 1,
  sportelli_dev = -1,
  musei_dev = -1,
  sanita_dev = -1,
  `Superficie esercizi commerciali per kmq 2021` = -1,
  `Addetti dei servizi alla persona per 1.000 abitanti nel 2021` = -1
)

# ============================
# 5. FUNZIONE INDICE EUCLIDEO (RANK NORMALIZZATO)
# ============================
calcola_indice_euclideo <- function(df, direction){
  
  vars <- names(df)[sapply(df, is.numeric)]
  
  # polarità
  for(v in names(direction)){
    if(v %in% vars) df[[v]] <- df[[v]] * direction[[v]]
  }
  
  # NORMALIZZAZIONE RANK (robusta)
  for(v in vars){
    n_valid <- sum(!is.na(df[[v]]))
    
    if(n_valid > 1){
      df[[v]] <- (rank(df[[v]], na.last = "keep", ties.method = "average") - 1) / (n_valid - 1)
    } else {
      df[[v]] <- 0
    }
  }
  
  # rimuove NA
  df_clean <- df[complete.cases(df[, vars]), ]
  
  # matrice
  mat <- as.matrix(df_clean[, vars])
  
  # punto ideale
  ideal <- rep(0, ncol(mat))
  
  # distanza euclidea
  dist <- sqrt(rowSums((mat - ideal)^2))
  
  # normalizzazione finale 0-1
  dist_norm <- (dist - min(dist)) / (max(dist) - min(dist))
  
  df_clean$indice <- dist_norm
  
  return(df_clean)
}

# ============================
# 6. CALCOLO INDICI
# ============================
ambientale <- calcola_indice_euclideo(ambientale, direction_list)
socio <- calcola_indice_euclideo(socio, direction_list)
economico <- calcola_indice_euclideo(economico, direction_list)
servizi <- calcola_indice_euclideo(servizi, direction_list)

# ============================
# 7. JOIN CON SHAPEFILE
# ============================
comuni_sf <- comuni_sf %>%
  dplyr::left_join(ambientale %>% dplyr::select(pro_com, indice) %>% dplyr::rename(indice_ambientale = indice), by = "pro_com") %>%
  dplyr::left_join(socio %>% dplyr::select(pro_com, indice) %>% dplyr::rename(indice_socio = indice), by = "pro_com") %>%
  dplyr::left_join(economico %>% dplyr::select(pro_com, indice) %>% dplyr::rename(indice_economico = indice), by = "pro_com") %>%
  dplyr::left_join(servizi %>% dplyr::select(pro_com, indice) %>% dplyr::rename(indice_servizi = indice), by = "pro_com")

# ============================
# 8. OUTPUT
# ============================
out_dir <- file.path(data_root, "MAPPE_INDICI_2021/")
dir.create(out_dir, showWarnings = FALSE)

# ============================
# 9. MAPPE
# ============================
macro_areas <- c("indice_ambientale","indice_socio","indice_economico","indice_servizi")

for(v in macro_areas){
  
  df <- comuni_sf
  
  p_cont <- ggplot(df) +
    geom_sf(aes(fill = !!sym(v)), color = NA) +
    scale_fill_gradient(low = "green", high = "red", na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Indice fragilità -", v))
  
  ggsave(paste0(out_dir, v, "_continua.png"), p_cont, width = 8, height = 6)
  
  df <- df %>% mutate(decile = ntile(!!sym(v), 10))
  
  p_dec <- ggplot(df) +
    geom_sf(aes(fill = factor(decile)), color = NA) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Indice fragilità (decili) -", v))
  
  ggsave(paste0(out_dir, v, "_decili.png"), p_dec, width = 8, height = 6)
}

# ============================
# 10. EXPORT
# ============================
write_xlsx(st_drop_geometry(comuni_sf),
           paste0(out_dir, "indice_macroaree_comuni_2021.xlsx"))

