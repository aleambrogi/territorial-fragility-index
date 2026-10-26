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
library(tidyr)

# ============================
# 1. SHAPEFILE
# ============================
comuni_sf <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                     quiet = TRUE) %>%
  st_transform(32632) %>%
  clean_names() %>%
  mutate(pro_com = as.character(pro_com))

# ============================
# 2. CARICAMENTO 4 DATASET
# ============================
ambientale <- read_excel(file.path(data_root, "INDICI 2019/dataset_macroarea_ambientale_2019.xlsx"))
socio <- read_excel(file.path(data_root, "INDICI 2019/dataset_macroarea_socio_2019.xlsx"))
economico <- read_excel(file.path(data_root, "INDICI 2019/dataset_macroarea_economica_2019.xlsx"))
servizi <- read_excel(file.path(data_root, "INDICI 2019/dataset_macroarea_servizi_2019.xlsx"))

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
  ind_vecchiaia_19 = 1,
  quota_pop_attiva_19 = -1,
  `Tasso crescita naturale 2019` = -1,
  `Tasso migratorio totale 2019` = 1,
  `Stranieri/pop 2019` = -1,
  `Densità abitativa 2019` = -1,
  
  # ECONOMICO
  `Reddito medio per contribuente 2019` = -1,
  imprese_per_1000_ab = -1,
  laureati_per_1000_ab = -1,
  `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2019 (%)` = 1,
  addetti_100_res = -1,
  
  # SERVIZI
  indice_di_accessibilita_ai_servizi_essenziali = 1,
  sportelli_dev = -1,
  musei_dev = -1,
  sanita_dev = -1,
  `Superficie esercizi commerciali per kmq 2019` = -1,
  `Addetti dei servizi alla persona per 1.000 abitanti nel 2019` = -1
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
  
  # NORMALIZZAZIONE RANK
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
out_dir <- file.path(data_root, "MAPPE_INDICI_2019/")
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
           paste0(out_dir, "indice_macroaree_comuni_2019.xlsx"))










# ============================
# 11. DISTRIBUZIONI AVANZATE
# ============================

for(v in macro_areas){
  
  p <- ggplot(comuni_sf, aes(x = !!sym(v))) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6) +
    geom_vline(xintercept = quantile(comuni_sf[[v]], c(0.25, 0.5, 0.75), na.rm = TRUE),
               linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Distribuzione + quartili -", v))
  
  ggsave(paste0(out_dir, v, "_distribuzione_avanzata.png"),
         p, width = 7, height = 5)
}

# ============================
# 12. BOXPLOT (OUTLIER)
# ============================

for(v in macro_areas){
  
  p <- ggplot(comuni_sf, aes(y = !!sym(v))) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Boxplot (outlier) -", v))
  
  ggsave(paste0(out_dir, v, "_boxplot.png"),
         p, width = 5, height = 6)
}

# ============================
# 13. MAPPA OUTLIER (TOP 10%)
# ============================

for(v in macro_areas){
  
  thr <- quantile(comuni_sf[[v]], 0.9, na.rm = TRUE)
  
  df <- comuni_sf %>%
    mutate(outlier_top = ifelse(!!sym(v) >= thr, "Top 10%", "Altri"))
  
  p <- ggplot(df) +
    geom_sf(aes(fill = outlier_top), color = NA) +
    scale_fill_manual(values = c("Top 10%" = "red", "Altri" = "grey90")) +
    theme_minimal() +
    labs(title = paste("Comuni più fragili (top 10%) -", v))
  
  ggsave(paste0(out_dir, v, "_outlier_map.png"),
         p, width = 8, height = 6)
}

# ============================
# 14. CORRELAZIONE TRA INDICI
# ============================

mat <- comuni_sf %>%
  st_drop_geometry() %>%
  select(all_of(macro_areas)) %>%
  na.omit()

cor_mat <- cor(mat)

png(paste0(out_dir, "correlazione_indici.png"), width = 800, height = 700)
corrplot::corrplot(cor_mat, method = "color", type = "upper",
                   addCoef.col = "black", tl.cex = 1)
dev.off()

# ============================
# 15. SCATTER PLOT TRA INDICI
# ============================

comb <- combn(macro_areas, 2, simplify = FALSE)

for(coppia in comb){
  
  v1 <- coppia[1]
  v2 <- coppia[2]
  
  p <- ggplot(comuni_sf, aes(x = !!sym(v1), y = !!sym(v2))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(title = paste("Relazione:", v1, "vs", v2))
  
  ggsave(paste0(out_dir, v1, "_vs_", v2, ".png"),
         p, width = 6, height = 5)
}

# ============================
# 16. RANKING E POSIZIONE
# ============================

df_rank <- comuni_sf %>%
  st_drop_geometry()

for(v in macro_areas){
  df_rank[[paste0("rank_", v)]] <- rank(-df_rank[[v]], ties.method = "average")
}

write_xlsx(df_rank,
           paste0(out_dir, "ranking_comuni_2019.xlsx"))

# ============================
# 17. GAP TRA INDICI (STRUTTURA)
# ============================

df_rank <- df_rank %>%
  mutate(
    max_indice = apply(select(., all_of(macro_areas)), 1, max, na.rm = TRUE),
    min_indice = apply(select(., all_of(macro_areas)), 1, min, na.rm = TRUE),
    gap = max_indice - min_indice
  )

p_gap <- ggplot(df_rank, aes(x = gap)) +
  geom_histogram(bins = 30, fill = "purple", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribuzione gap tra macroaree")

ggsave(paste0(out_dir, "gap_indici.png"),
       p_gap, width = 7, height = 5)

# ============================
# 18. MAPPA SQUILIBRIO
# ============================

p_gap_map <- ggplot(comuni_sf %>% left_join(df_rank %>% select(pro_com, gap), by="pro_com")) +
  geom_sf(aes(fill = gap), color = NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Squilibrio tra dimensioni di fragilità")

ggsave(paste0(out_dir, "mappa_gap.png"),
       p_gap_map, width = 8, height = 6)

# ============================
# 19. CODE (TAILS) + COLORI
# ============================

for(v in macro_areas){
  
  # soglie code (10% - 90%)
  low_thr  <- quantile(comuni_sf[[v]], 0.10, na.rm = TRUE)
  high_thr <- quantile(comuni_sf[[v]], 0.90, na.rm = TRUE)
  
  df <- comuni_sf %>%
    mutate(classe_coda = case_when(
      !!sym(v) <= low_thr  ~ "Bassa fragilità (coda sx)",
      !!sym(v) >= high_thr ~ "Alta fragilità (coda dx)",
      TRUE ~ "Intermedio"
    ))
  
  # ============================
  # ISTOGRAMMA COLORATO
  # ============================
  
  p_hist <- ggplot(df, aes(x = !!sym(v), fill = classe_coda)) +
    geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
    scale_fill_manual(values = c(
      "Bassa fragilità (coda sx)" = "green",
      "Alta fragilità (coda dx)" = "red",
      "Intermedio" = "grey70"
    )) +
    theme_minimal() +
    labs(title = paste("Distribuzione con code -", v),
         fill = "Classe")
  
  ggsave(paste0(out_dir, v, "_distribuzione_code.png"),
         p_hist, width = 7, height = 5)
  
  # ============================
  # MAPPA CODE
  # ============================
  
  p_map <- ggplot(df) +
    geom_sf(aes(fill = classe_coda), color = NA) +
    scale_fill_manual(values = c(
      "Bassa fragilità (coda sx)" = "green",
      "Alta fragilità (coda dx)" = "red",
      "Intermedio" = "grey90"
    )) +
    theme_minimal() +
    labs(title = paste("Comuni nelle code -", v))
  
  ggsave(paste0(out_dir, v, "_mappa_code.png"),
         p_map, width = 8, height = 6)
}


# ============================
# 20. IDENTIFICAZIONE CODE MULTIPLE
# ============================

df_code <- comuni_sf %>%
  st_drop_geometry()

for(v in macro_areas){
  
  low_thr  <- quantile(df_code[[v]], 0.20, na.rm = TRUE)
  high_thr <- quantile(df_code[[v]], 0.80, na.rm = TRUE)
  
  df_code[[paste0(v, "_coda")]] <- case_when(
    df_code[[v]] <= low_thr  ~ -1,  # bassa fragilità
    df_code[[v]] >= high_thr ~  1,  # alta fragilità
    TRUE ~ 0
  )
}

# ============================
# 21. CONTEGGIO
# ============================

df_code <- df_code %>%
  rowwise() %>%
  mutate(
    n_fragili = sum(c_across(ends_with("_coda")) == 1, na.rm = TRUE),
    n_resilienti = sum(c_across(ends_with("_coda")) == -1, na.rm = TRUE)
  ) %>%
  ungroup()

# ============================
# 22. CLASSIFICAZIONE PROFILI
# ============================

df_code <- df_code %>%
  mutate(profilo_code = case_when(
    
    n_fragili >= 2 ~ "Fragilità sistemica",
    
    n_fragili == 1 ~ "Fragilità selettiva",
    
    n_resilienti >= 2 ~ "Resiliente",
    
    TRUE ~ "Intermedio"
  ))

# ============================
# 23. MAPPA PROFILI
# ============================

comuni_sf <- comuni_sf %>%
  left_join(df_code %>% select(pro_com, profilo_code), by = "pro_com")

p_profili_code <- ggplot(comuni_sf) +
  geom_sf(aes(fill = profilo_code), color = NA) +
  scale_fill_manual(values = c(
    "Fragilità sistemica" = "darkred",
    "Fragilità selettiva" = "orange",
    "Resiliente" = "darkgreen",
    "Intermedio" = "grey80"
  )) +
  theme_minimal() +
  labs(title = "Profili di fragilità basati sulle code")

ggsave(paste0(out_dir, "profili_code.png"),
       p_profili_code, width = 8, height = 6)

# ============================
# 24. DISTRIBUZIONE PROFILI
# ============================

p_bar <- ggplot(df_code, aes(x = profilo_code)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribuzione dei profili di fragilità")

ggsave(paste0(out_dir, "distribuzione_profili_code.png"),
       p_bar, width = 6, height = 5)

# ============================
# 25. EXPORT LISTE
# ============================

write_xlsx(df_code %>% filter(profilo_code == "Fragilità sistemica"),
           paste0(out_dir, "comuni_fragilita_sistemica.xlsx"))

write_xlsx(df_code %>% filter(profilo_code == "Fragilità selettiva"),
           paste0(out_dir, "comuni_fragilita_selettiva.xlsx"))

write_xlsx(df_code %>% filter(profilo_code == "Resiliente"),
           paste0(out_dir, "comuni_resilienti.xlsx"))


# ============================
# 26. CORRELAZIONE INDICE - COVARIATE AMBIENTALI
# ============================

# ============================
# VARIABLE RENAME (PAPER VERSION)
# ============================

ambientale_r <- ambientale %>%
  rename(
    AirQuality = indice_inquinamento,
    HighEmissionVehicles = tasso_di_motorizzazione_ad_alta_emissione,
    Waste = raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante,
    ProtectedAreas = scarto_aree_protette,
    HydroRisk = rischio_num,
    LandUse = consumo_del_suolo
  )

socio_r <- socio %>%
  rename(
    OldAge = Ind_vecchiaia_19,
    ActivePop = Quota_pop_attiva_19,
    NaturalGrowth = `Tasso crescita naturale 2019`,
    Migration = `Tasso migratorio totale 2019`,
    ForeignShare = `Stranieri/pop 2019`,
    Density = `Densità abitativa 2019`
  )

economico_r <- economico %>%
  rename(
    Income = `Reddito medio per contribuente 2019`,
    Firms = imprese_per_1000_ab,
    Graduates = laureati_per_1000_ab,
    LowIncome = `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2019 (%)`,
    Employment = addetti_100_res
  )

servizi_r <- servizi %>%
  rename(
    ServiceAccess = indice_di_accessibilita_ai_servizi_essenziali,
    Banks = sportelli_dev,
    Museums = musei_dev,
    Healthcare = sanita_dev,
    Retail = `Superficie esercizi commerciali per kmq 2019`,
    PersonalServices = `Addetti dei servizi alla persona per 1.000 abitanti nel 2019`
  )


plot_corr <- function(df, title, file){
  
  mat <- df %>% select(where(is.numeric)) %>% na.omit()
  
  cor_mat <- cor(mat)
  
  # stampa matrice numerica (per dopo)
  print(round(cor_mat, 3))
  
  png(file, width = 800, height = 700)
  
  corrplot::corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 45,
    number.cex = 0.8
  )
  
  dev.off()
}

# ============================
# CORRPLOT ENVIRONMENTAL
# ============================
plot_corr(
  ambientale_r,
  "Environmental",
  paste0(out_dir, "corr_environmental.png")
)

# ============================
# CORRPLOT SOCIO
# ============================
plot_corr(
  socio_r,
  "Socio-demographic",
  paste0(out_dir, "corr_social.png")
)

# ============================
# CORRPLOT ECONOMIC
# ============================
plot_corr(
  economico_r,
  "Economic",
  paste0(out_dir, "corr_economic.png")
)

# ============================
# CORRPLOT SERVICES
# ============================
plot_corr(
  servizi_r,
  "Services",
  paste0(out_dir, "corr_services.png")
)

# ============================
# SPATIAL AUTOCORRELATION (MORAN'S I)
# ============================

library(spdep)

# ============================
# 1. CREATE SPATIAL WEIGHTS
# ============================

# neighbors (queen contiguity)
nb <- poly2nb(comuni_sf)

# listw object
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ============================
# 2. FUNCTION MORAN TEST
# ============================

morans_test <- function(sf_data, var, lw){
  
  x <- sf_data[[var]]
  
  x <- na.omit(x)
  
  moran <- moran.test(x, lw, zero.policy = TRUE)
  
  cat("\n============================\n")
  cat("MORAN'S I -", var, "\n")
  cat("============================\n")
  print(moran$estimate)
  print(moran$p.value)
  
  # Monte Carlo (more robust for thesis)
  mc <- moran.mc(x, lw, nsim = 999, zero.policy = TRUE)
  
  cat("\nMonte Carlo p-value:\n")
  print(mc$p.value)
  
  return(list(moran = moran, mc = mc))
}

# ============================
# 3. APPLY TO INDICES
# ============================

macro_vars <- c(
  "indice_ambientale",
  "indice_socio",
  "indice_economico",
  "indice_servizi"
)

results_moran <- list()

for(v in macro_vars){
  results_moran[[v]] <- morans_test(comuni_sf, v, lw)
}



library(spdep)
library(dplyr)
library(ggplot2)

run_lisa <- function(sf_obj, var, nb, out_file){
  
  library(spdep)
  library(dplyr)
  library(ggplot2)
  
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  x <- sf_obj[[var]]
  
  # ============================
  # GLOBAL MORAN
  # ============================
  cat("\n============================\n")
  cat("MORAN'S I -", var, "\n")
  cat("============================\n")
  
  moran_test <- moran.test(x, lw, zero.policy = TRUE)
  print(moran_test)
  
  mc <- moran.mc(x, lw, nsim = 999, zero.policy = TRUE)
  print(mc)
  
  # ============================
  # LISA
  # ============================
  local <- localmoran(x, lw, zero.policy = TRUE)
  
  sf_obj$lisa_I <- local[,1]
  sf_obj$lisa_p <- local[,5]
  
  lag_x <- as.numeric(lag.listw(lw, x, zero.policy = TRUE))
  
  # deviation from mean (NOT scale)
  x_dev <- x - mean(x, na.rm = TRUE)
  
  sf_obj$lisa_cluster <- case_when(
    x_dev > 0 & lag_x > mean(lag_x, na.rm = TRUE) ~ "High-High (Hotspot)",
    x_dev < 0 & lag_x < mean(lag_x, na.rm = TRUE) ~ "Low-Low (Coldspot)",
    x_dev > 0 & lag_x < mean(lag_x, na.rm = TRUE) ~ "High-Low (Outlier)",
    x_dev < 0 & lag_x > mean(lag_x, na.rm = TRUE) ~ "Low-High (Outlier)",
    TRUE ~ "Not significant"
  )
  
  sf_obj$lisa_sig <- ifelse(sf_obj$lisa_p <= 0.05,
                            sf_obj$lisa_cluster,
                            "Not significant")
  
  # ============================
  # MAP
  # ============================
  p <- ggplot(sf_obj) +
    geom_sf(aes(fill = lisa_sig), color = NA) +
    scale_fill_manual(values = c(
      "High-High (Hotspot)" = "red",
      "Low-Low (Coldspot)" = "blue",
      "High-Low (Outlier)" = "orange",
      "Low-High (Outlier)" = "lightblue",
      "Not significant" = "grey90"
    )) +
    theme_minimal() +
    labs(
      title = paste("LISA Cluster Map -", var),
      fill = "Cluster type"
    )
  
  ggsave(out_file, p, width = 8, height = 6, dpi = 300)
  
  return(sf_obj)
}
comuni_sf <- run_lisa(comuni_sf, "indice_ambientale", nb,
                      paste0(out_dir, "lisa_ambientale.png"))

comuni_sf <- run_lisa(comuni_sf, "indice_socio", nb,
                      paste0(out_dir, "lisa_socio.png"))

comuni_sf <- run_lisa(comuni_sf, "indice_economico", nb,
                      paste0(out_dir, "lisa_economico.png"))

comuni_sf <- run_lisa(comuni_sf, "indice_servizi", nb,
                      paste0(out_dir, "lisa_servizi.png"))






tab_profili <- df_code %>%
  count(profilo_code) %>%
  mutate(percent = n / sum(n) * 100)

print(tab_profili)

run_lisa_save <- function(sf_obj, var, nb){
  
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  x <- sf_obj[[var]]
  
  local <- localmoran(x, lw, zero.policy = TRUE)
  
  lisa_I  <- local[,1]
  lisa_p  <- local[,5]
  lag_x   <- as.numeric(lag.listw(lw, x, zero.policy = TRUE))
  x_dev   <- x - mean(x, na.rm = TRUE)
  
  lisa_cluster <- case_when(
    x_dev > 0 & lag_x > mean(lag_x, na.rm = TRUE) ~ "High-High (Hotspot)",
    x_dev < 0 & lag_x < mean(lag_x, na.rm = TRUE) ~ "Low-Low (Coldspot)",
    x_dev > 0 & lag_x < mean(lag_x, na.rm = TRUE) ~ "High-Low (Outlier)",
    x_dev < 0 & lag_x > mean(lag_x, na.rm = TRUE) ~ "Low-High (Outlier)",
    TRUE ~ "Not significant"
  )
  
  lisa_sig <- ifelse(lisa_p <= 0.05, lisa_cluster, "Not significant")
  
  # salva con nome specifico
  sf_obj[[paste0("lisa_", var)]] <- lisa_sig
  
  return(sf_obj)
}

for(v in macro_areas){
  comuni_sf <- run_lisa_save(comuni_sf, v, nb)
}

results_all <- list()

for(v in macro_areas){
  
  lisa_var <- paste0("lisa_", v)
  
  tab <- comuni_sf %>%
    st_drop_geometry() %>%
    select(profilo_code, !!sym(lisa_var)) %>%
    count(profilo_code, !!sym(lisa_var)) %>%
    group_by(profilo_code) %>%
    mutate(percent = n / sum(n) * 100) %>%
    mutate(indice = v)
  
  results_all[[v]] <- tab
}

tab_lisa_all <- bind_rows(results_all)

print(tab_lisa_all, n =100)

tab_lisa_wide <- tab_lisa_all %>%
  rename(lisa = starts_with("lisa_")) %>%
  select(indice, profilo_code, lisa, percent) %>%
  pivot_wider(names_from = lisa, values_from = percent)

print(tab_lisa_wide)

df_multi <- df_code %>%
  count(n_fragili)

df_multi <- df_multi %>%
  mutate(percent = n / sum(n) * 100)

print(df_multi)


df_gap_prof <- df_rank %>%
  left_join(df_code %>% select(pro_com, profilo_code), by = "pro_com") %>%
  group_by(profilo_code) %>%
  summarise(
    mean_gap = mean(gap, na.rm = TRUE),
    sd_gap = sd(gap, na.rm = TRUE)
  )

print(df_gap_prof)



# ============================
# 1. SICUREZZA: CHECK COLONNA
# ============================
if(!"profilo_code" %in% names(comuni_sf)){
  
  comuni_sf <- comuni_sf %>%
    left_join(
      df_code %>% select(pro_com, profilo_code),
      by = "pro_com"
    )
}

# ============================
# 2. LABEL INGLESE (SAFE RECODE)
# ============================
label_map <- c(
  "Fragilità sistemica" = "Systemic fragility",
  "Fragilità selettiva" = "Selective fragility",
  "Resiliente" = "Stable low-fragility",
  "Intermedio" = "Intermediate"
)

# ============================
# 3. CREA VARIABILE INGLESE (SAFE MODE)
# ============================
comuni_sf$profilo_code_en <- label_map[comuni_sf$profilo_code]

# fallback per NA (importantissimo)
comuni_sf$profilo_code_en[is.na(comuni_sf$profilo_code_en)] <- "Unknown"

# ============================
# 4. MAPPA PROFILI
# ============================
p_profili_code <- ggplot(comuni_sf) +
  geom_sf(aes(fill = profilo_code_en), color = NA) +
  scale_fill_manual(values = c(
    "Systemic fragility" = "darkred",
    "Selective fragility" = "orange",
    "Stable low-fragility" = "darkgreen",
    "Intermediate" = "grey80",
    "Unknown" = "grey90"
  )) +
  theme_minimal() +
  labs(
    title = "Territorial fragility profiles",
    fill = "Profile"
  )

# ============================
# 5. SAVE
# ============================
ggsave(
  filename = paste0(out_dir, "profili_code_EN.png"),
  plot = p_profili_code,
  width = 8,
  height = 6
)


# ============================
# LISA vs FRAGILITÀ PROFILE (20/80)
# ============================

library(dplyr)
library(tidyr)

# scegli un indice LISA (puoi ciclare dopo)
lisa_vars <- paste0("lisa_", macro_areas)

results_lisa_frag <- list()

for(v in lisa_vars){
  
  tab <- comuni_sf %>%
    st_drop_geometry() %>%
    select(profilo_code, all_of(v)) %>%
    rename(lisa = all_of(v)) %>%
    
    # classifica LISA semplificata
    mutate(lisa_class = case_when(
      grepl("High-High", lisa) ~ "High-High",
      grepl("Low-Low", lisa) ~ "Low-Low",
      lisa == "Not significant" ~ "Not significant",
      TRUE ~ "Outlier"
    )) %>%
    
    count(profilo_code, lisa_class) %>%
    group_by(profilo_code) %>%
    mutate(percent = n / sum(n) * 100) %>%
    mutate(indicator = v)
  
  results_lisa_frag[[v]] <- tab
}

tab_lisa_frag <- bind_rows(results_lisa_frag)

print(tab_lisa_frag, n=60)



coherence_index <- comuni_sf %>%
  st_drop_geometry() %>%
  mutate(
    systemic_lisa = ifelse(lisa_indice_ambientale == "High-High (Hotspot)" |
                             lisa_indice_socio == "High-High (Hotspot)" |
                             lisa_indice_economico == "High-High (Hotspot)" |
                             lisa_indice_servizi == "High-High (Hotspot)", 1, 0)
  ) %>%
  group_by(profilo_code) %>%
  summarise(
    coherence = mean(systemic_lisa, na.rm = TRUE)
  )

print(coherence_index)




prepare_raw <- function(df){
  df %>%
    select(where(is.numeric)) %>%
    na.omit()
}

ambientale_raw <- ambientale
socio_raw <- socio
economico_raw <- economico
servizi_raw <- servizi


plot_corr_rank <- function(df, direction_list, title, file){
  
  # ============================
  # SELECT NUMERIC VARIABLES
  # ============================
  mat <- df %>%
    select(where(is.numeric)) %>%
    na.omit()
  
  vars <- names(mat)
  
  # ============================
  # APPLY POLARITY
  # ============================
  for(v in names(direction_list)){
    if(v %in% vars){
      mat[[v]] <- mat[[v]] * direction_list[[v]]
    }
  }
  
  # ============================
  # RANK TRANSFORMATION
  # ============================
  mat_rank <- as.data.frame(lapply(mat, function(x){
    n_valid <- sum(!is.na(x))
    
    if(n_valid > 1){
      (rank(x, na.last = "keep", ties.method = "average") - 1) / (n_valid - 1)
    } else {
      rep(0, length(x))
    }
  }))
  
  # ============================
  # CORRELATION MATRIX
  # ============================
  cor_mat <- cor(mat_rank, use = "pairwise.complete.obs")
  
  print(paste("Correlation matrix -", title))
  print(round(cor_mat, 3))
  
  # ============================
  # PLOT
  # ============================
  png(file, width = 800, height = 700)
  
  corrplot::corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 45,
    number.cex = 0.8
  )
  
  dev.off()
}


plot_corr_rank(
  ambientale,
  direction_list,
  "Environmental (Rank + Polarity)",
  paste0(out_dir, "corr_environmental_rank.png")
)

plot_corr_rank(
  socio,
  direction_list,
  "Socio-demographic (Rank + Polarity)",
  paste0(out_dir, "corr_social_rank.png")
)

plot_corr_rank(
  economico,
  direction_list,
  "Economic (Rank + Polarity)",
  paste0(out_dir, "corr_economic_rank.png")
)

plot_corr_rank(
  servizi,
  direction_list,
  "Services (Rank + Polarity)",
  paste0(out_dir, "corr_services_rank.png")
)
