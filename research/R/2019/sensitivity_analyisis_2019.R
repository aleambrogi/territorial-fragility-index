data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

# ============================
# SENSITIVITY ANALYSIS COMPLETA 2019
# ============================

library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(janitor)

# ============================
# OUTPUT DIR
# ============================

base_dir <- file.path(data_root, "Sensitivity analysis 2019/")

dir_noise     <- paste0(base_dir, "noise/")
dir_distance  <- paste0(base_dir, "distance/")
dir_polarity  <- paste0(base_dir, "polarity/")

dir.create(base_dir, showWarnings = FALSE)
dir.create(dir_noise, showWarnings = FALSE)
dir.create(dir_distance, showWarnings = FALSE)
dir.create(dir_polarity, showWarnings = FALSE)

# ============================
# PARAMETRI
# ============================

n_sim <- 100
noise_levels <- c(0.01, 0.05, 0.10)
distance_methods <- c("euclidea","manhattan","chebyshev")

# ============================
# SHAPEFILE
# ============================

comuni_sf <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                     quiet = TRUE) %>%
  st_transform(32632) %>%
  clean_names() %>%
  filter(cod_reg == 3) %>%
  mutate(pro_com = as.character(pro_com))

# ============================
# DATASET
# ============================

load_data <- function(path){
  read_excel(path) %>% mutate(pro_com = as.character(pro_com))
}

macro_datasets <- list(
  ambientale = load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_ambientale_2019.xlsx")),
  socio      = load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_socio_2019.xlsx")),
  economico  = load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_economica_2019.xlsx")),
  servizi    = load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_servizi_2019.xlsx"))
)

# ============================
# DIREZIONE
# ============================

direction_list <- list(
  indice_inquinamento = 1, consumo_del_suolo = 1,
  raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante = 1,
  tasso_di_motorizzazione_ad_alta_emissione = 1,
  rischio = 1, scarto_aree_protette = -1,
  ind_vecchiaia_19 = 1, quota_pop_attiva_19 = -1,
  `Tasso crescita naturale 2019` = -1,
  `Tasso migratorio totale 2019` = 1,
  `Stranieri/pop 2019` = -1,
  `Densità abitativa 2019` = -1,
  `Reddito medio per contribuente 2019` = -1,
  imprese_per_1000_ab = -1,
  laureati_per_1000_ab = -1,
  `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2019 (%)` = 1,
  addetti_100_res = -1,
  indice_di_accessibilita_ai_servizi_essenziali = 1,
  sportelli_dev = -1, musei_dev = -1,
  sanita_dev = -1,
  `Superficie esercizi commerciali per kmq 2019` = -1,
  `Addetti dei servizi alla persona per 1.000 abitanti nel 2019` = -1
)

# ============================
# FUNZIONE INDICE
# ============================

calcola_indice <- function(df, direction, metodo="euclidea"){
  
  vars <- names(df)[sapply(df, is.numeric)]
  vars <- setdiff(vars, "pro_com")
  
  for(v in names(direction)){
    if(v %in% vars) df[[v]] <- df[[v]] * direction[[v]]
  }
  
  for(v in vars){
    n_valid <- sum(!is.na(df[[v]]))
    if(n_valid > 1){
      df[[v]] <- (rank(df[[v]]) - 1)/(n_valid-1)
    } else df[[v]] <- 0
  }
  
  df <- df[complete.cases(df[, vars]), ]
  mat <- as.matrix(df[, vars])
  
  if(metodo=="euclidea")  dist <- sqrt(rowSums(mat^2))
  if(metodo=="manhattan") dist <- rowSums(abs(mat))
  if(metodo=="chebyshev") dist <- apply(mat,1,max)
  
  dist_norm <- (dist - min(dist))/(max(dist)-min(dist))
  
  df$indice <- dist_norm
  return(df)
}

# ============================
# RUMORE
# ============================

aggiungi_rumore <- function(df, noise_level){
  
  vars <- names(df)[sapply(df, is.numeric)]
  vars <- setdiff(vars, "pro_com")
  
  for(v in vars){
    sd_var <- sd(df[[v]], na.rm=TRUE)
    if(!is.na(sd_var) && sd_var>0){
      df[[v]] <- df[[v]] + rnorm(nrow(df),0,noise_level*sd_var)
    }
  }
  return(df)
}

# ==========================================================
# 1. SENSITIVITY - RUMORE (MONTE CARLO)
# ==========================================================

results_noise <- list()

for(nome in names(macro_datasets)){
  
  df_base <- macro_datasets[[nome]]
  
  base <- calcola_indice(df_base, direction_list) %>%
    select(pro_com, indice_base = indice)
  
  for(noise_level in noise_levels){
    
    sim_matrix <- matrix(NA, nrow=nrow(base), ncol=n_sim)
    
    for(s in 1:n_sim){
      df_noise <- aggiungi_rumore(df_base, noise_level)
      sim <- calcola_indice(df_noise, direction_list) %>% pull(indice)
      sim_matrix[,s] <- sim
    }
    
    df_res <- base %>%
      mutate(
        mean_sim = rowMeans(sim_matrix),
        sd_sim = apply(sim_matrix,1,sd),
        noise = noise_level,
        macroarea = nome,
        rank_base = rank(-indice_base),
        rank_mean = rank(-mean_sim),
        rank_shift = abs(rank_base-rank_mean)
      )
    
    results_noise[[paste(nome,noise_level)]] <- df_res
    
    # distribuzione
    ggsave(paste0(dir_noise,nome,"_",noise_level,"_density.png"),
           ggplot(df_res)+
             geom_density(aes(x=indice_base), fill="blue", alpha=0.4)+
             geom_density(aes(x=mean_sim), fill="red", alpha=0.4)+
             theme_minimal())
    
    # rank shift
    ggsave(paste0(dir_noise,nome,"_",noise_level,"_rankshift.png"),
           ggplot(df_res,aes(x=rank_shift))+
             geom_histogram(bins=30, fill="darkred")+
             theme_minimal())
  }
}

# ==========================================================
# 2. SENSITIVITY - DISTANZE
# ==========================================================

results_dist <- list()

for(nome in names(macro_datasets)){
  
  df <- macro_datasets[[nome]]
  
  base <- calcola_indice(df, direction_list, "euclidea") %>%
    select(pro_com, indice_euclidea = indice)
  
  for(met in distance_methods){
    
    alt <- calcola_indice(df, direction_list, met) %>%
      select(pro_com, indice_alt = indice)
    
    df_res <- left_join(base, alt, by="pro_com") %>%
      mutate(
        metodo = met,
        diff = abs(indice_euclidea - indice_alt),
        rank_shift = abs(rank(-indice_euclidea) - rank(-indice_alt))
      )
    
    results_dist[[paste(nome,met)]] <- df_res
    
    ggsave(paste0(dir_distance,nome,"_",met,"_scatter.png"),
           ggplot(df_res,aes(indice_euclidea,indice_alt))+
             geom_point(alpha=0.5)+
             geom_abline(color="red")+
             theme_minimal())
  }
}

# ==========================================================
# 3. SENSITIVITY - POLARITÀ
# ==========================================================

results_pol <- list()

for(nome in names(macro_datasets)){
  
  df <- macro_datasets[[nome]]
  
  base <- calcola_indice(df, direction_list) %>%
    select(pro_com, indice_base = indice)
  
  vars <- intersect(names(df), names(direction_list))
  
  for(v in vars){
    
    direction_mod <- direction_list
    direction_mod[[v]] <- -direction_mod[[v]]   # flip
    
    alt <- calcola_indice(df, direction_mod) %>%
      select(pro_com, indice_alt = indice)
    
    df_res <- left_join(base, alt, by="pro_com") %>%
      mutate(
        variabile = v,
        diff = abs(indice_base - indice_alt),
        rank_shift = abs(rank(-indice_base) - rank(-indice_alt))
      )
    
    results_pol[[paste(nome,v)]] <- df_res
    
    ggsave(paste0(dir_polarity,nome,"_",v,"_rankshift.png"),
           ggplot(df_res,aes(x=rank_shift))+
             geom_histogram(bins=30, fill="purple")+
             theme_minimal())
  }
}

# ==========================================================
# ANALISI GLOBALI
# ==========================================================

# noise globale
all_noise <- bind_rows(results_noise)

ggsave(paste0(dir_noise,"global_sd.png"),
       ggplot(all_noise,aes(x=sd_sim, fill=factor(noise)))+
         geom_density(alpha=0.4)+
         theme_minimal())

# distance globale
all_dist <- bind_rows(results_dist)

ggsave(paste0(dir_distance,"global_diff.png"),
       ggplot(all_dist,aes(x=diff, fill=metodo))+
         geom_density(alpha=0.4)+
         theme_minimal())

# polarity globale
all_pol <- bind_rows(results_pol)

ggsave(paste0(dir_polarity,"global_rankshift.png"),
       ggplot(all_pol,aes(x=rank_shift))+
         geom_density(fill="purple", alpha=0.5)+
         theme_minimal())

