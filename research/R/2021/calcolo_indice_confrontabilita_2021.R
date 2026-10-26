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
library(ggalluvial)
library(patchwork)
library(spdep)

# ============================
# PATH
# ============================
out_dir <- file.path(data_root, "CONFRONTABILITA_2021/")
dir.create(out_dir, showWarnings = FALSE)

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
# LOAD DATI
# ============================
load_data <- function(path){
  read_excel(path) %>%
    mutate(pro_com = as.character(pro_com))
}

# 2019 (BASE)
ambientale_2019 <- load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_ambientale_2019.xlsx"))
socio_2019      <- load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_socio_2019.xlsx"))
economico_2019  <- load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_economica_2019.xlsx"))
servizi_2019    <- load_data(file.path(data_root, "INDICI 2019/dataset_macroarea_servizi_2019.xlsx"))

# 2021 (TARGET)
ambientale_2021 <- load_data(file.path(data_root, "INDICI 2021/dataset_macroarea_ambientale_2021.xlsx"))
socio_2021      <- load_data(file.path(data_root, "INDICI 2021/dataset_macroarea_socio_2021.xlsx"))
economico_2021  <- load_data(file.path(data_root, "INDICI 2021/dataset_macroarea_economica_2021.xlsx"))
servizi_2021    <- load_data(file.path(data_root, "INDICI 2021/dataset_macroarea_servizi_2021.xlsx"))

datasets_2019 <- list(ambientale=ambientale_2019,
                      socio=socio_2019,
                      economico=economico_2019,
                      servizi=servizi_2019)

datasets_2021 <- list(ambientale=ambientale_2021,
                      socio=socio_2021,
                      economico=economico_2021,
                      servizi=servizi_2021)

# ============================
# RISCHIO
# ============================
trasforma_rischio <- function(df){
  
  if("rischio" %in% names(df)){
    
    scala_r1 <- c("Nessun"=0,"Basso"=1,"Medio"=2,"Alto"=3)
    scala_r2 <- c("Nessun"=0,"Basso"=1,"Medio"=2,"Alto"=3,"Molto alto"=4)
    
    df <- df %>%
      separate(rischio, into=c("r1","r2"), sep="\\|", remove=FALSE) %>%
      mutate(
        r1 = trimws(r1),
        r2 = trimws(r2),
        r1_num = scala_r1[r1],
        r2_num = scala_r2[r2],
        rischio_num = pmax(r1_num, r2_num, na.rm=TRUE)
      ) %>%
      select(-r1,-r2,-r1_num,-r2_num,-rischio)
  }
  
  df
}

datasets_2019 <- lapply(datasets_2019, trasforma_rischio)
datasets_2021 <- lapply(datasets_2021, trasforma_rischio)

# ============================
# RINOMINA 2021 → 2019
# ============================
rinomina_2021 <- function(df, macro){
  
  if(macro=="socio"){
    df <- df %>%
      rename(
        "Tasso migratorio totale 2019" = "Tasso migratorio totale 2021",
        "Tasso crescita naturale 2019" = "Tasso crescita naturale 2021",
        "Ind_vecchiaia_19" = "Ind_vecchiaia_21",
        "Quota_pop_attiva_19" = "Quota_pop_attiva_21",
        "Densità abitativa 2019" = "Densità abitativa 2021",
        "Stranieri/pop 2019" = "Stranieri/pop 2021"
      )
  }
  
  if(macro=="economico"){
    df <- df %>%
      rename(
        "Reddito medio per contribuente 2019" = "Reddito medio per contribuente 2021",
        "Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2019 (%)" =
          "Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2021 (%)"
      )
  }
  
  if(macro=="servizi"){
    df <- df %>%
      rename(
        "Superficie esercizi commerciali per kmq 2019" =
          "Superficie esercizi commerciali per kmq 2021",
        "Addetti dei servizi alla persona per 1.000 abitanti nel 2019" =
          "Addetti dei servizi alla persona per 1.000 abitanti nel 2021"
      )
  }
  
  df
}

datasets_2021$ambientale <- rinomina_2021(datasets_2021$ambientale,"ambientale")
datasets_2021$socio      <- rinomina_2021(datasets_2021$socio,"socio")
datasets_2021$economico  <- rinomina_2021(datasets_2021$economico,"economico")
datasets_2021$servizi    <- rinomina_2021(datasets_2021$servizi,"servizi")

# ============================
# IMPUTAZIONE MEDIANA
# ============================
imputa_mediana <- function(df){
  
  vars <- names(df)[sapply(df, is.numeric)]
  
  for(v in vars){
    med <- median(df[[v]], na.rm=TRUE)
    df[[v]][is.na(df[[v]])] <- med
  }
  
  df
}

datasets_2019 <- lapply(datasets_2019, imputa_mediana)
datasets_2021 <- lapply(datasets_2021, imputa_mediana)

# ============================
# DIREZIONE
# ============================
direction_list <- list(
  indice_inquinamento = 1,
  consumo_del_suolo = 1,
  raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante = 1,
  tasso_di_motorizzazione_ad_alta_emissione = 1,
  rischio_num = 1,
  scarto_aree_protette = -1,
  
  ind_vecchiaia_19 = 1,
  quota_pop_attiva_19 = -1,
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
  sportelli_dev = -1,
  musei_dev = -1,
  sanita_dev = -1,
  `Superficie esercizi commerciali per kmq 2019` = -1,
  `Addetti dei servizi alla persona per 1.000 abitanti nel 2019` = -1
)

# ============================
# BASE ECDF 2019
# ============================
costruisci_base <- function(df){
  
  vars <- names(df)[sapply(df, is.numeric)]
  
  for(v in names(direction_list)){
    if(v %in% vars) df[[v]] <- df[[v]] * direction_list[[v]]
  }
  
  ecdf_list <- lapply(df[vars], ecdf)
  
  Z <- df
  for(v in vars){
    Z[[v]] <- ecdf_list[[v]](df[[v]])
  }
  
  d <- sqrt(rowSums(as.matrix(Z[, vars])^2))
  
  list(ecdf=ecdf_list, vars=vars, min=min(d), max=max(d))
}

# ============================
# INDICE
# ============================
calcola_indice <- function(df, base){
  
  df <- df %>% select(pro_com, all_of(base$vars))
  
  for(v in names(direction_list)){
    if(v %in% base$vars){
      df[[v]] <- df[[v]] * direction_list[[v]]
    }
  }
  
  Z <- df
  for(v in base$vars){
    Z[[v]] <- base$ecdf[[v]](df[[v]])
  }
  
  d <- sqrt(rowSums(as.matrix(Z[, base$vars])^2))
  idx <- (d - base$min)/(base$max - base$min)
  
  df$indice <- idx
  df
}

# ============================
# RUN
# ============================
results_2019 <- list()
results_2021 <- list()

for(name in names(datasets_2019)){
  
  base <- costruisci_base(datasets_2019[[name]])
  
  results_2019[[name]] <- calcola_indice(datasets_2019[[name]], base)
  results_2021[[name]] <- calcola_indice(datasets_2021[[name]], base)
}

# ============================
# MERGE
# ============================
for(name in names(results_2021)){
  
  comuni_sf <- comuni_sf %>%
    left_join(results_2019[[name]] %>%
                select(pro_com, indice) %>%
                rename(!!paste0("indice_",name,"_2019") := indice),
              by="pro_com") %>%
    left_join(results_2021[[name]] %>%
                select(pro_com, indice) %>%
                rename(!!paste0("indice_",name,"_2021") := indice),
              by="pro_com") %>% distinct()
}

# ============================
# ANALISI DINAMICA + ADVANCED UPGRADE
# ============================

macro <- names(results_2021)

# =========================================================
# 1. DIFFERENZE + DECILI + MOBILITÀ
# =========================================================
for(name in macro){
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  comuni_sf[[paste0("diff_", name)]] <- comuni_sf[[v21]] - comuni_sf[[v19]]
  
  comuni_sf[[paste0("dec_2019_", name)]] <- ntile(comuni_sf[[v19]], 10)
  comuni_sf[[paste0("dec_2021_", name)]] <- ntile(comuni_sf[[v21]], 10)
  
  comuni_sf[[paste0("mob_", name)]] <- 
    comuni_sf[[paste0("dec_2021_", name)]] -
    comuni_sf[[paste0("dec_2019_", name)]]
}

# =========================================================
# 2. INDICE SINTETICO DI MIGLIORAMENTO GLOBALE
# =========================================================
comuni_sf$delta_globale <- rowMeans(
  st_drop_geometry(comuni_sf)[, grep("^diff_", names(comuni_sf))],
  na.rm = TRUE
)

# =========================================================
# 3. CLUSTER TRAIETTORIE (k-means su variazioni)
# =========================================================
mat_diff <- st_drop_geometry(comuni_sf)[, grep("^diff_", names(comuni_sf))]

mat_diff <- mat_diff[, colSums(is.na(mat_diff)) == 0]

set.seed(123)
km <- kmeans(scale(mat_diff), centers = 4)

comuni_sf$cluster_traiettoria <- factor(km$cluster)

# =========================================================
# 4. MAPPA CLUSTER TRAIETTORIE
# =========================================================
p <- ggplot(comuni_sf) +
  geom_sf(aes(fill = cluster_traiettoria), color = NA) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Cluster di evoluzione della fragilità (2019–2021)")

ggsave(paste0(out_dir, "cluster_traiettorie.png"), p, width = 8, height = 6)

# =========================================================
# 5. CLASSIFICAZIONE DINAMICA SEMPLIFICATA
# =========================================================
comuni_sf$profilo_globale <- case_when(
  comuni_sf$delta_globale <= -0.05 ~ "Miglioramento forte",
  comuni_sf$delta_globale < 0 ~ "Miglioramento lieve",
  comuni_sf$delta_globale == 0 ~ "Stabile",
  comuni_sf$delta_globale < 0.05 ~ "Peggioramento lieve",
  TRUE ~ "Peggioramento forte"
)

p <- ggplot(comuni_sf) +
  geom_sf(aes(fill = profilo_globale), color = NA) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Miglioramento forte" = "darkgreen",
    "Miglioramento lieve" = "lightgreen",
    "Stabile" = "grey80",
    "Peggioramento lieve" = "orange",
    "Peggioramento forte" = "red"
  )) +
  labs(title = "Profilo globale di evoluzione")

ggsave(paste0(out_dir, "profilo_globale.png"), p, width = 8, height = 6)

# =========================================================
# 6. ISTOGRAMMA DELTA GLOBALE
# =========================================================
p <- ggplot(comuni_sf, aes(x = delta_globale)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribuzione miglioramento globale")

ggsave(paste0(out_dir, "delta_globale_hist.png"), p, width = 7, height = 5)

# =========================================================
# 7. TRANSIZIONE DECILI (SANKey READY TABLE)
# =========================================================
for(name in macro){
  
  d19 <- paste0("dec_2019_", name)
  d21 <- paste0("dec_2021_", name)
  
  sankey_tab <- as.data.frame(table(
    from = comuni_sf[[d19]],
    to = comuni_sf[[d21]]
  ))
  
  write_xlsx(
    sankey_tab,
    paste0(out_dir, "sankey_decili_", name, ".xlsx")
  )
}

# =========================================================
# 8. TOP MIGLIORI / PEGGIORI GLOBALI
# =========================================================
df_rank <- st_drop_geometry(comuni_sf) %>%
  select(pro_com, comune, delta_globale) %>%
  arrange(desc(delta_globale))

write_xlsx(
  list(
    migliori = head(df_rank, 20),
    peggiori = tail(df_rank, 20)
  ),
  paste0(out_dir, "top_globali.xlsx")
)

# =========================================================
# 9. MAPPA MIGLIORAMENTO GLOBALE
# =========================================================
p <- ggplot(comuni_sf) +
  geom_sf(aes(fill = delta_globale), color = NA) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  theme_minimal() +
  labs(title = "Variazione complessiva fragilità")

ggsave(paste0(out_dir, "delta_globale_mappa.png"), p, width = 8, height = 6)

# =========================================================
# 10. EXPORT FINALE
# =========================================================
write_xlsx(
  st_drop_geometry(comuni_sf),
  paste0(out_dir, "dataset_finale_completo.xlsx")
)


# =========================================================
#  11. HEATMAP TRANSIZIONE DECILI
# =========================================================
  
for(name in macro){
    
  d19 <- paste0("dec_2019_", name)
  d21 <- paste0("dec_2021_", name)
    
  tab <- as.data.frame(table(
    comuni_sf[[d19]],
     comuni_sf[[d21]]
  ))
    
  colnames(tab) <- c("dec_2019","dec_2021","freq")
    
  p <- ggplot(tab, aes(x = dec_2019, y = dec_2021, fill = freq)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
     labs(title = paste("Heatmap transizione decili -", name))
    
  ggsave(paste0(out_dir, "heatmap_decili_", name, ".png"),
          p, width = 7, height = 6)
}

#=========================================================
#  12. SANKEY (ALLUVIAL PLOT 2019 → 2021)
#=========================================================
  
for(name in macro){
    
  d19 <- paste0("dec_2019_", name)
  d21 <- paste0("dec_2021_", name)
    
  df_alluv <- comuni_sf %>%
    st_drop_geometry() %>%
    select(all_of(c(d19, d21))) %>%
    mutate(n = 1) %>%
    filter(!is.na(.data[[d19]]), !is.na(.data[[d21]]))
    
  p <- ggplot(df_alluv,
              aes(axis1 = .data[[d19]],
                  axis2 = .data[[d21]],
                  y = n)) +
    geom_alluvium(aes(fill = factor(.data[[d19]])), alpha = 0.7) +
    geom_stratum(width = 1/6, fill = "grey80") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_x_discrete(limits = c("2019 decili", "2021 decili")) +
    theme_minimal() +
    labs(title = paste("Sankey decili fragilità -", name))
    
  ggsave(paste0(out_dir, "sankey_decili_", name, ".png"),
          p, width = 9, height = 6)
}

#=========================================================
#  13. SINTESI MOBILITÀ
#=========================================================
  
mobilita_summary <- data.frame()

for(name in macro){
  
  mob <- paste0("mob_", name)
  
  tmp <- data.frame(
    macroarea = name,
    migliorati = sum(comuni_sf[[mob]] > 0, na.rm = TRUE),
    peggiorati = sum(comuni_sf[[mob]] < 0, na.rm = TRUE),
    stabili = sum(comuni_sf[[mob]] == 0, na.rm = TRUE)
  )
  
  mobilita_summary <- rbind(mobilita_summary, tmp)
}

write_xlsx(mobilita_summary,
           paste0(out_dir, "sintesi_mobilita_macroaree.xlsx"))


# =========================================================
# 14. MAPPE COVARIATE (CONTINUE - SCALA COMUNE 2019/2021)
# =========================================================

get_vars <- function(df){
  names(df)[sapply(df, is.numeric)]
}

for(name in names(datasets_2019)){
  
  cat("\nProcessing macroarea:", name)
  
  # crea cartella macroarea
  dir_macro <- paste0(out_dir, name, "/")
  dir.create(dir_macro, showWarnings = FALSE)
  
  df19 <- datasets_2019[[name]]
  df21 <- datasets_2021[[name]]
  
  # variabili comuni
  vars <- intersect(get_vars(df19), get_vars(df21))
  vars <- setdiff(vars, "pro_com")
  
  for(v in vars){
    
    cat("\n  Variabile:", v)
    
    # join con shape
    mappa19 <- comuni_sf %>%
      select(pro_com, comune, geometry) %>%
      left_join(df19 %>% select(pro_com, !!sym(v)), by="pro_com")
    
    mappa21 <- comuni_sf %>%
      select(pro_com, comune, geometry) %>%
      left_join(df21 %>% select(pro_com, !!sym(v)), by="pro_com")
    
    # range comune (FONDAMENTALE)
    range_tot <- range(
      c(mappa19[[v]], mappa21[[v]]),
      na.rm = TRUE
    )
    
    # plot 2019
    p19 <- ggplot(mappa19) +
      geom_sf(aes(fill = !!sym(v)), color = NA) +
      scale_fill_gradient(
        low = "green",
        high = "red",
        limits = range_tot,
        na.value = "grey90"
      ) +
      theme_minimal() +
      labs(title = paste(v, "- 2019"), fill = v)
    
    # plot 2021
    p21 <- ggplot(mappa21) +
      geom_sf(aes(fill = !!sym(v)), color = NA) +
      scale_fill_gradient(
        low = "green",
        high = "red",
        limits = range_tot,
        na.value = "grey90"
      ) +
      theme_minimal() +
      labs(title = paste(v, "- 2021"), fill = v)
    
    # affiancamento
    p_comb <- p19 + p21
    
    # salva
    ggsave(
      paste0(dir_macro, gsub("[^[:alnum:]]","_", v), "_confronto.png"),
      p_comb,
      width = 12,
      height = 6
    )
  }
}


# =========================================================
# 15. MAPPE DIFFERENZE COVARIATE (2021 - 2019)
# =========================================================

get_vars <- function(df){
  names(df)[sapply(df, is.numeric)]
}

for(name in names(datasets_2019)){
  
  cat("\nDifferenze macroarea:", name)
  
  # cartella (usa la stessa di prima)
  dir_macro <- paste0(out_dir, name, "/")
  
  df19 <- datasets_2019[[name]]
  df21 <- datasets_2021[[name]]
  
  vars <- intersect(get_vars(df19), get_vars(df21))
  vars <- setdiff(vars, "pro_com")
  
  for(v in vars){
    
    cat("\n  Variabile:", v)
    
    # join
    mappa <- comuni_sf %>%
      select(pro_com, comune, geometry) %>%
      left_join(df19 %>% select(pro_com, !!sym(v)), by="pro_com") %>%
      rename(val_2019 = !!sym(v)) %>%
      left_join(df21 %>% select(pro_com, !!sym(v)), by="pro_com") %>%
      rename(val_2021 = !!sym(v))
    
    # differenza
    mappa$diff <- mappa$val_2021 - mappa$val_2019
    
    # scala simmetrica (fondamentale)
    max_abs <- max(abs(mappa$diff), na.rm = TRUE)
    
    # plot
    p <- ggplot(mappa) +
      geom_sf(aes(fill = diff), color = "black", size = 0.1) +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-max_abs, max_abs),
        na.value = "grey90"
      ) +
      theme_minimal() +
      labs(
        title = paste("Differenza", v, "(2021 - 2019)"),
        fill = "Δ"
      )
    
    # salva
    ggsave(
      paste0(dir_macro, gsub("[^[:alnum:]]","_", v), "_DIFF.png"),
      p,
      width = 8,
      height = 6
    )
  }
}

# =========================================================
# 16. MORAN SCATTERPLOT (FIXED)
# =========================================================

dir_scatter <- paste0(out_dir, "SPATIAL_SCATTER/")
dir.create(dir_scatter, showWarnings = FALSE)

for(name in macro){
  
  cat("\nMoran scatter:", name)
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  # =========================
  # DATI PULITI
  # =========================
  comuni_tmp <- comuni_sf %>%
    select(pro_com, geometry, all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  x19 <- comuni_tmp[[v19]]
  x21 <- comuni_tmp[[v21]]
  
  # =========================
  # PESI SPAZIALI COERENTI
  # =========================
  nb_tmp <- poly2nb(comuni_tmp)
  lw_tmp <- nb2listw(nb_tmp, style = "W", zero.policy = TRUE)
  
  # =========================
  # STANDARDIZZAZIONE
  # =========================
  z19 <- scale(x19)
  z21 <- scale(x21)
  
  lag19 <- lag.listw(lw_tmp, z19, zero.policy = TRUE)
  lag21 <- lag.listw(lw_tmp, z21, zero.policy = TRUE)
  
  df_plot19 <- data.frame(z = z19, lag = lag19)
  df_plot21 <- data.frame(z = z21, lag = lag21)
  
  # =========================
  # PLOT
  # =========================
  p1 <- ggplot(df_plot19, aes(x = z, y = lag)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_minimal() +
    labs(title = paste("Moran scatter 2019 -", name),
         x = "Valore standardizzato",
         y = "Lag spaziale")
  
  p2 <- ggplot(df_plot21, aes(x = z, y = lag)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_minimal() +
    labs(title = paste("Moran scatter 2021 -", name),
         x = "Valore standardizzato",
         y = "Lag spaziale")
  
  ggsave(paste0(dir_scatter, "scatter_", name, "_2019.png"), p1, width=6, height=5)
  ggsave(paste0(dir_scatter, "scatter_", name, "_2021.png"), p2, width=6, height=5)
}

# =========================================================
# BLOCCO LISA + MORAN (FIX COMPLETO)
# =========================================================

dir_spatial <- paste0(out_dir, "SPATIAL/")
dir.create(dir_spatial, showWarnings = FALSE)

sigma_conv <- data.frame()

for(name in macro){
  
  cat("\nAnalisi spaziale macroarea:", name)
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  # =========================
  # DATI PULITI
  # =========================
  comuni_tmp <- comuni_sf %>%
    select(pro_com, geometry, all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  x19 <- comuni_tmp[[v19]]
  x21 <- comuni_tmp[[v21]]
  delta <- x21 - x19
  
  # =========================
  # PESI SPAZIALI COERENTI
  # =========================
  nb_tmp <- poly2nb(comuni_tmp, snap = 500)
  lw_tmp <- nb2listw(nb_tmp, style = "W", zero.policy = TRUE)
  
  # =========================
  # 1. MORAN GLOBALI
  # =========================
  moran19 <- moran.test(x19, lw_tmp, zero.policy = TRUE)
  moran21 <- moran.test(x21, lw_tmp, zero.policy = TRUE)
  moran_delta <- moran.test(delta, lw_tmp, zero.policy = TRUE)
  
  cat("\nMoran 2019:", moran19$estimate[1])
  cat("\nMoran 2021:", moran21$estimate[1])
  cat("\nMoran DELTA:", moran_delta$estimate[1])
  
  # =========================
  # 2. LISA
  # =========================
  lisa19 <- localmoran(x19, lw_tmp, zero.policy = TRUE)
  lisa21 <- localmoran(x21, lw_tmp, zero.policy = TRUE)
  
  comuni_tmp$lisa19 <- lisa19[,1]
  comuni_tmp$lisa21 <- lisa21[,1]
  
  lag19 <- lag.listw(lw_tmp, x19, zero.policy = TRUE)
  lag21 <- lag.listw(lw_tmp, x21, zero.policy = TRUE)
  
  comuni_tmp$cluster19 <- "Non significativo"
  comuni_tmp$cluster21 <- "Non significativo"
  
  signif19 <- lisa19[,5] < 0.05
  signif21 <- lisa21[,5] < 0.05
  
  comuni_tmp$cluster19[signif19 & x19 > mean(x19) & lag19 > mean(lag19)] <- "HH"
  comuni_tmp$cluster19[signif19 & x19 < mean(x19) & lag19 < mean(lag19)] <- "LL"
  comuni_tmp$cluster19[signif19 & x19 > mean(x19) & lag19 < mean(lag19)] <- "HL"
  comuni_tmp$cluster19[signif19 & x19 < mean(x19) & lag19 > mean(lag19)] <- "LH"
  
  comuni_tmp$cluster21[signif21 & x21 > mean(x21) & lag21 > mean(lag21)] <- "HH"
  comuni_tmp$cluster21[signif21 & x21 < mean(x21) & lag21 < mean(lag21)] <- "LL"
  comuni_tmp$cluster21[signif21 & x21 > mean(x21) & lag21 < mean(lag21)] <- "HL"
  comuni_tmp$cluster21[signif21 & x21 < mean(x21) & lag21 > mean(lag21)] <- "LH"
  
  # =========================
  # 3. MAPPE LISA
  # =========================
  p1 <- ggplot(comuni_tmp) +
    geom_sf(aes(fill = cluster19), color = "black", size = 0.1) +
    scale_fill_manual(values = c(
      "HH"="red","LL"="blue","HL"="orange","LH"="lightblue","Non significativo"="grey90"
    )) +
    theme_minimal() +
    labs(title = paste("LISA 2019 -", name))
  
  p2 <- ggplot(comuni_tmp) +
    geom_sf(aes(fill = cluster21), color = "black", size = 0.1) +
    scale_fill_manual(values = c(
      "HH"="red","LL"="blue","HL"="orange","LH"="lightblue","Non significativo"="grey90"
    )) +
    theme_minimal() +
    labs(title = paste("LISA 2021 -", name))
  
  ggsave(paste0(dir_spatial, "LISA_", name, "_2019.png"), p1, width=7, height=6)
  ggsave(paste0(dir_spatial, "LISA_", name, "_2021.png"), p2, width=7, height=6)
  
  # =========================
  # 4. TRANSIZIONI
  # =========================
  transizioni <- table(comuni_tmp$cluster19, comuni_tmp$cluster21)
  
  write_xlsx(
    as.data.frame(transizioni),
    paste0(dir_spatial, "transizioni_LISA_", name, ".xlsx")
  )
  
  # =========================
  # 5. MAPPA TRANSIZIONI
  # =========================
  comuni_tmp$transizione <- paste(comuni_tmp$cluster19, "→", comuni_tmp$cluster21)
  
  p_trans <- ggplot(comuni_tmp) +
    geom_sf(aes(fill = transizione), color = "black", size = 0.1) +
    theme_minimal() +
    labs(title = paste("Transizioni LISA -", name))
  
  ggsave(paste0(dir_spatial, "LISA_transition_map_", name, ".png"),
         p_trans, width=8, height=6)
  
  # =========================
  # 6. SIGMA CONVERGENZA
  # =========================
  sigma_conv <- rbind(sigma_conv, data.frame(
    macroarea = name,
    var_2019 = var(x19),
    var_2021 = var(x21),
    delta_var = var(x21) - var(x19)
  ))
}

write_xlsx(sigma_conv,
           paste0(dir_spatial, "sigma_convergence.xlsx"))

# =========================
# SALVA MORAN + P-VALUE
# =========================
moran_results <- data.frame()

for(name in macro){
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  comuni_tmp <- comuni_sf %>%
    select(pro_com, geometry, all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  x19 <- comuni_tmp[[v19]]
  x21 <- comuni_tmp[[v21]]
  delta <- x21 - x19
  
  nb_tmp <- poly2nb(comuni_tmp)
  lw_tmp <- nb2listw(nb_tmp, style = "W", zero.policy = TRUE)
  
  m19 <- moran.test(x19, lw_tmp, zero.policy = TRUE)
  m21 <- moran.test(x21, lw_tmp, zero.policy = TRUE)
  md  <- moran.test(delta, lw_tmp, zero.policy = TRUE)
  
  moran_results <- rbind(moran_results, data.frame(
    macroarea = name,
    moran_2019 = m19$estimate[1],
    p_2019 = m19$p.value,
    moran_2021 = m21$estimate[1],
    p_2021 = m21$p.value,
    moran_delta = md$estimate[1],
    p_delta = md$p.value
  ))
}

write_xlsx(moran_results,
           paste0(out_dir, "moran_significativita.xlsx"))
# =========================================================
# BETA CONVERGENZA (FIX DEFINITIVO)
# =========================================================

dir_conv <- paste0(out_dir, "CONVERGENZA/")
dir.create(dir_conv, showWarnings = FALSE)

beta_results <- data.frame()

for(name in macro){
  
  cat("\nBeta convergence:", name)
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  # =========================
  # DATASET COERENTE
  # =========================
  df <- comuni_sf %>%
    st_drop_geometry() %>%
    select(all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  # rinomina variabili (fondamentale)
  df <- df %>%
    rename(x0 = all_of(v19),
           x1 = all_of(v21))
  
  df$delta <- df$x1 - df$x0
  
  # =========================
  # MODELLO
  # =========================
  model <- lm(delta ~ x0, data = df)
  
  beta <- coef(model)[2]
  
  beta_results <- rbind(beta_results, data.frame(
    macroarea = name,
    beta = beta
  ))
  
  # =========================
  # PLOT
  # =========================
  p <- ggplot(df, aes(x = x0, y = delta)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(title = paste("Beta convergence -", name),
         x = "Indice 2019",
         y = "Variazione 2019–2021")
  
  ggsave(paste0(dir_conv, "beta_", name, ".png"),
         p, width=6, height=5)
}

write_xlsx(beta_results,
           paste0(dir_conv, "beta_convergence.xlsx"))

summary(model)

rank19 <- rank(x19)
rank21 <- rank(x21)

cor_rank <- cor(rank19, rank21, method = "spearman")
cor_rank


# =========================================================
# STABILITÀ RANKING
# =========================================================

rank_stability <- data.frame()

for(name in macro){
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  df <- comuni_sf %>%
    st_drop_geometry() %>%
    select(all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  rank19 <- rank(df[[v19]])
  rank21 <- rank(df[[v21]])
  
  cor_rank <- cor(rank19, rank21, method = "spearman")
  
  rank_stability <- rbind(rank_stability, data.frame(
    macroarea = name,
    spearman_rank = cor_rank
  ))
}

write_xlsx(rank_stability,
           paste0(out_dir, "stabilita_ranking.xlsx"))



# =========================================================
# INTENSITÀ MOBILITÀ DECILI
# =========================================================

mobilita_intensita <- data.frame()

for(name in macro){
  
  mob <- paste0("mob_", name)
  
  delta_abs <- abs(comuni_sf[[mob]])
  
  mobilita_intensita <- rbind(mobilita_intensita, data.frame(
    macroarea = name,
    media_spostamento = mean(delta_abs, na.rm = TRUE),
    max_spostamento = max(delta_abs, na.rm = TRUE),
    pct_mobili = mean(delta_abs > 0, na.rm = TRUE)
  ))
}

write_xlsx(mobilita_intensita,
           paste0(out_dir, "intensita_mobilita.xlsx"))



# =========================================================
# ANALISI PER PROVINCIA
# =========================================================

prov_summary <- data.frame()

for(name in macro){
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  tmp <- comuni_sf %>%
    st_drop_geometry() %>%
    select(pro_com_t, all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  # =========================
  # COSTRUZIONE PROVINCIA
  # =========================
  tmp$provincia <- substr(tmp$pro_com_t, 1, 3)
  
  # variazione
  tmp$delta <- tmp[[v21]] - tmp[[v19]]
  
  # =========================
  # AGGREGAZIONE
  # =========================
  agg <- tmp %>%
    group_by(provincia) %>%
    summarise(
      mean_2019 = mean(.data[[v19]]),
      mean_2021 = mean(.data[[v21]]),
      delta_mean = mean(delta),
      n_comuni = n(),
      .groups = "drop"
    )
  
  agg$macroarea <- name
  
  prov_summary <- rbind(prov_summary, agg)
}

write_xlsx(prov_summary,
           paste0(out_dir, "analisi_province.xlsx"))


# =========================================================
# ANALISI DETTAGLIO RANK SHIFT PER MACROAREA
# =========================================================

rank_shift_summary <- data.frame()

for(name in macro){
  
  cat("\nRank shift:", name)
  
  v19 <- paste0("indice_", name, "_2019")
  v21 <- paste0("indice_", name, "_2021")
  
  df <- comuni_sf %>%
    st_drop_geometry() %>%
    select(all_of(c(v19, v21))) %>%
    filter(!is.na(.data[[v19]]), !is.na(.data[[v21]]))
  
  r19 <- rank(df[[v19]])
  r21 <- rank(df[[v21]])
  
  d <- r21 - r19
  
  tmp <- data.frame(
    macroarea = name,
    mean_abs_shift = mean(abs(d)),
    sd_shift = sd(d),
    median_shift = median(d),
    min_shift = min(d),
    max_shift = max(d),
    pct_shift_100 = mean(abs(d) > 100),
    pct_shift_200 = mean(abs(d) > 200)
  )
  
  rank_shift_summary <- rbind(rank_shift_summary, tmp)
}

write_xlsx(
  rank_shift_summary,
  paste0(out_dir, "rank_shift_summary.xlsx")
)
