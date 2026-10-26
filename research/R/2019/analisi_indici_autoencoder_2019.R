data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

# ============================
# PACCHETTI
# ============================
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(intrinsicDimension)
library(Rdimtools)
library(uwot)
library(transport)
library(randomForest)

# ============================
# 1. SHAPEFILE LOMBARDIA
# ============================
comuni_sf <- st_read(file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp"),
                     quiet = TRUE) %>%
  clean_names() %>%
  filter(cod_reg == 3) %>%   # Lombardia
  mutate(pro_com = as.character(pro_com)) %>%
  st_transform(32632)

# ============================
# 2. CARICAMENTO INDICI AUTOENCODER (PYTHON)
# ============================
indici <- read_excel(file.path(data_root, "BLACKBOX_AE1D/indici_blackbox_AE1D.xlsx")) %>%
  clean_names() %>%
  mutate(pro_com = as.character(pro_com))

# ============================
# 3. JOIN
# ============================
comuni_sf <- comuni_sf %>%
  left_join(indici, by = "pro_com")

# ============================
# 4. INDICI DA ANALIZZARE
# ============================
macro_areas <- c(
  "indice_ambientale",
  "indice_socio",
  "indice_economico",
  "indice_servizi"
)

# ============================
# 5. OUTPUT DIRECTORY
# ============================
out_dir <- file.path(data_root, "MAPPE_AUTOENCODER_2019/")
dir.create(out_dir, showWarnings = FALSE)

# ============================
# 6. MAPPE CONTINUE
# ============================
for(v in macro_areas){
  
  p <- ggplot(comuni_sf) +
    geom_sf(aes(fill = !!sym(v)), color = NA) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Autoencoder index -", v))
  
  ggsave(paste0(out_dir, v, "_continua_2019.png"),
         p, width = 8, height = 6)
}

# ============================
# 7. MAPPE DECILI
# ============================
for(v in macro_areas){
  
  df <- comuni_sf %>%
    mutate(decile = ntile(!!sym(v), 10))
  
  p <- ggplot(df) +
    geom_sf(aes(fill = factor(decile)), color = NA) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Autoencoder - decili", v))
  
  ggsave(paste0(out_dir, v, "_decili_2019.png"),
         p, width = 8, height = 6)
}

# ============================
# 8. CLUSTERING K-MEANS
# ============================
cluster_df <- comuni_sf %>%
  st_drop_geometry() %>%
  select(all_of(macro_areas)) %>%
  na.omit()

set.seed(123)
k4 <- kmeans(cluster_df, centers = 4, nstart = 25)

comuni_sf$cluster <- NA
idx <- complete.cases(st_drop_geometry(comuni_sf)[, macro_areas])
comuni_sf$cluster[idx] <- k4$cluster

# ============================
# 9. MAPPA CLUSTER
# ============================
p_cluster <- ggplot(comuni_sf) +
  geom_sf(aes(fill = factor(cluster)), color = NA) +
  theme_minimal() +
  labs(title = "Clustering su indici autoencoder")

ggsave(paste0(out_dir, "cluster_map_2019.png"),
       p_cluster, width = 8, height = 6)


# ============================
# 10. CORRELAZIONI WHITE vs BLACK
# ============================

white <- read_excel(file.path(data_root, "MAPPE_INDICI_2019/indice_macroaree_comuni_2019.xlsx")) %>%
  clean_names() %>%
  mutate(pro_com = as.character(pro_com)) %>% distinct() %>%
  rename(indice_ambientale_white = indice_ambientale,
         indice_economico_white = indice_economico,
         indice_servizi_white = indice_servizi,
         indice_socio_white = indice_socio)

df_corr <- white %>%
  inner_join(indici, by = "pro_com") %>% distinct()

macro_areas_white <- c(
  "indice_ambientale_white",
  "indice_socio_white",
  "indice_economico_white",
  "indice_servizi_white"
)

for(i in 1:4){
  
  x <- df_corr[[macro_areas[i]]]
  y <- df_corr[[macro_areas_white[i]]]
  
  cat("\n", macro_areas[i], "\n")
  print(cor(x, y, method = "pearson"))
  print(cor(x, y, method = "spearman"))
}


# ============================
# 11. SCATTER WHITE vs BLACK
# ============================

for(i in 1:4){
  
  df_plot <- df_corr %>%
    select(x = all_of(macro_areas[i]),
           y = all_of(macro_areas_white[i]))
  
  p <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(
      title = paste("White vs Black -", macro_areas[i]),
      x = "Black-box",
      y = "White-box"
    )
  
  ggsave(paste0(out_dir, "scatter_", macro_areas[i], ".png"),
         p, width = 6, height = 5)
}


# ============================
# 12. MAPPA DIFFERENZE
# ============================

comuni_sf <- comuni_sf %>%
  left_join(white, by = "pro_com")

for(i in 1:4){
  
  diff_name <- paste0("diff_", macro_areas[i])
  
  comuni_sf[[diff_name]] <- comuni_sf[[macro_areas[i]]] -
    comuni_sf[[macro_areas_white[i]]]
  
  p <- ggplot(comuni_sf) +
    geom_sf(aes(fill = !!sym(diff_name)), color = NA) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
    ) +
    theme_minimal() +
    labs(title = paste("Differenza Black - White:", macro_areas[i]))
  
  ggsave(paste0(out_dir, diff_name, ".png"),
         p, width = 8, height = 6)
}



# ============================
# 13. KENDALL TAU
# ============================

for(i in 1:4){
  
  tau <- cor(
    df_corr[[macro_areas[i]]],
    df_corr[[macro_areas_white[i]]],
    method = "kendall"
  )
  
  cat("\nKendall tau -", macro_areas[i], ":", tau, "\n")
}

# ============================
# 14. DISTRIBUZIONI
# ============================

for(i in 1:4){
  
  df_plot <- df_corr %>%
    select(
      black = all_of(macro_areas[i]),
      white = all_of(macro_areas_white[i])
    ) %>%
    tidyr::pivot_longer(cols = everything())
  
  p <- ggplot(df_plot, aes(x = value, fill = name)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(title = paste("Distribuzioni:", macro_areas[i]))
  
  ggsave(paste0(out_dir, "density_", macro_areas[i], ".png"),
         p, width = 6, height = 5)
}


summary(df_corr$indice_ambientale)
summary(df_corr$indice_ambientale_white)
cor(df_corr$indice_ambientale,
    df_corr$indice_ambientale_white,
    method = "spearman")
ggplot(df_corr, aes(x = indice_ambientale,
                    y = indice_ambientale_white)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red")



# ============================
# 15. ANALISI STRUTTURA DATI (MACROAREE)
# ============================

out_dir_struct <- paste0(out_dir, "STRUTTURA_DATI/")
dir.create(out_dir_struct, showWarnings = FALSE)

analyze_structure <- function(df, var_name){
  
  cat("\n============================\n")
  cat("MACROAREA:", var_name, "\n")
  cat("============================\n")
  
  # ----------------------------
  # COSTRUISCO DATASET ORIGINALE
  # (usa i dati prima dell'indice!)
  # ----------------------------
  
  # ATTENZIONE: qui servono le variabili ORIGINALI
  # quindi leggiamo i dataset macroarea
  
  file_map <- list(
    indice_ambientale = "dataset_macroarea_ambientale_2019.xlsx",
    indice_socio = "dataset_macroarea_socio_2019.xlsx",
    indice_economico = "dataset_macroarea_economica_2019.xlsx",
    indice_servizi = "dataset_macroarea_servizi_2019.xlsx"
  )
  
  path_base <- file.path(data_root, "INDICI 2019/")
  
  df_raw <- read_excel(paste0(path_base, file_map[[var_name]])) %>%
    clean_names() %>%
    select(where(is.numeric)) %>%
    na.omit()
  
  # ----------------------------
  # 1. PCA
  # ----------------------------
  pca <- prcomp(df_raw, scale. = TRUE)
  var_exp <- summary(pca)$importance[2,]
  
  cat("\nVarianza spiegata PCA (prime 5):\n")
  print(round(var_exp[1:5], 3))
  
  write.csv(var_exp,
            paste0(out_dir_struct, "pca_variance_", var_name, ".csv"))
  
  png(paste0(out_dir_struct, "scree_", var_name, ".png"), 600, 400)
  plot(var_exp, type="b",
       main=paste("Scree plot -", var_name),
       ylab="Varianza spiegata",
       xlab="Componenti")
  dev.off()
  
  # ----------------------------
  # 2. CORRELAZIONE MEDIA
  # ----------------------------
  cor_mat <- cor(df_raw)
  mean_cor <- mean(abs(cor_mat[upper.tri(cor_mat)]))
  
  cat("\nCorrelazione media assoluta:", round(mean_cor, 3), "\n")
  
  write.csv(cor_mat,
            paste0(out_dir_struct, "cor_matrix_", var_name, ".csv"))
  
  png(paste0(out_dir_struct, "cor_heatmap_", var_name, ".png"), 600, 600)
  image(cor_mat, main=paste("Correlation -", var_name))
  dev.off()
  
  # ============================
  # 3. INTRINSIC DIMENSION (ROBUSTA)
  # ============================
  
  X_mat <- as.matrix(df_raw)
  storage.mode(X_mat) <- "double"
  X_mat <- X_mat[complete.cases(X_mat), ]
  
  id_est <- est.twonn(X_mat)
  
  cat("\nIntrinsic dimension (TwoNN):", round(id_est$estdim, 2), "\n")
  
  write.csv(data.frame(dim_est=id_est$estdim),
            paste0(out_dir_struct, "intrinsic_dim_", var_name, ".csv"))
  
  # ----------------------------
  # 4. VARIANZA VARIABILI
  # ----------------------------
  var_vars <- apply(df_raw, 2, var)
  
  write.csv(var_vars,
            paste0(out_dir_struct, "variance_variables_", var_name, ".csv"))
  
  # ----------------------------
  # 5. PAIRPLOT
  # ----------------------------
  # ATTENZIONE: pesante
  
  
  # png(paste0(out_dir_struct, "pairs_", var_name, ".png"), 800, 800)
  # pairs(df_raw, main=paste("Pairs plot -", var_name))
  # dev.off()
  
  cat("\n✔ Analisi completata per", var_name, "\n")
}

# ============================
# RUN ANALISI STRUTTURA
# ============================

for(v in macro_areas){
  analyze_structure(df_corr, v)
}



# ============================
# 16. UMAP (STRUTTURA LATENTE)
# ============================

out_dir_umap <- paste0(out_dir, "UMAP/")
dir.create(out_dir_umap, showWarnings = FALSE)

analyze_umap <- function(var_name){
  
  cat("\nUMAP:", var_name, "\n")
  
  file_map <- list(
    indice_ambientale = "dataset_macroarea_ambientale_2019.xlsx",
    indice_socio = "dataset_macroarea_socio_2019.xlsx",
    indice_economico = "dataset_macroarea_economica_2019.xlsx",
    indice_servizi = "dataset_macroarea_servizi_2019.xlsx"
  )
  
  path_base <- file.path(data_root, "INDICI 2019/")
  
  # ============================
  # 1. CARICAMENTO + ALLINEAMENTO
  # ============================
  df_raw <- read_excel(paste0(path_base, file_map[[var_name]])) %>%
    clean_names() %>%
    mutate(pro_com = as.character(pro_com)) %>%
    filter(pro_com %in% df_corr$pro_com)
  
  # ============================
  # 2. NUMERICO + NA
  # ============================
  df_raw_num <- df_raw %>%
    select(pro_com, where(is.numeric)) %>%
    na.omit()
  
  # ============================
  # 3. UMAP
  # ============================
  X <- scale(df_raw_num %>% select(-pro_com))
  
  set.seed(42)
  umap_emb <- umap(X, n_neighbors = 15, min_dist = 0.1)
  
  # ============================
  # 4. DATAFRAME BASE
  # ============================
  df_plot <- data.frame(
    pro_com = df_raw_num$pro_com,
    U1 = umap_emb[,1],
    U2 = umap_emb[,2]
  )
  
  # ============================
  # 5. MERGE CORRETTO
  # ============================
  df_plot <- df_plot %>%
    left_join(df_corr, by = "pro_com") %>%
    filter(
      !is.na(.data[[var_name]]),
      !is.na(.data[[paste0(var_name, "_white")]])
    )
  
  # ============================
  # 6. VARIABILI ALLINEATE
  # ============================
  df_plot$black <- df_plot[[var_name]]
  df_plot$white <- df_plot[[paste0(var_name, "_white")]]
  
  check_local_consistency <- function(df, variable){
    
    X_umap <- as.matrix(df[, c("U1","U2")])
    
    knn <- get.knn(
      X_umap,
      k = 10
    )
    
    local_var <- sapply(1:nrow(df), function(i){
      
      var(
        df[[variable]][knn$nn.index[i,]],
        na.rm = TRUE
      )
      
    })
    
    mean(local_var, na.rm = TRUE)
  }
  
  
  lc_black <- check_local_consistency(df_plot, "black")
  lc_white <- check_local_consistency(df_plot, "white")
  
  
  cat("\nLocal consistency:\n")
  cat("Black:", lc_black, "\n")
  cat("White:", lc_white, "\n")
  
  
  write.csv(
    data.frame(
      index=c("black","white"),
      local_variance=c(lc_black,lc_white)
    ),
    paste0(out_dir_umap,
           "local_consistency_",
           var_name,
           ".csv"),
    row.names=FALSE
  )
  
  # ============================
  # 7. PLOT STRUTTURA
  # ============================
  p1 <- ggplot(df_plot, aes(U1, U2)) +
    geom_point(alpha = 0.4) +
    theme_minimal() +
    labs(title = paste("UMAP struttura -", var_name))
  
  ggsave(paste0(out_dir_umap, "umap_structure_", var_name, ".png"),
         p1, width = 6, height = 5)
  
  # ============================
  # 8. BLACK INDEX
  # ============================
  p2 <- ggplot(df_plot, aes(U1, U2, color = black)) +
    geom_point() +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = paste("UMAP + Black index -", var_name))
  
  ggsave(paste0(out_dir_umap, "umap_black_", var_name, ".png"),
         p2, width = 6, height = 5)
  
  # ============================
  # 9. WHITE INDEX
  # ============================
  p3 <- ggplot(df_plot, aes(U1, U2, color = white)) +
    geom_point() +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = paste("UMAP + White index -", var_name))
  
  ggsave(paste0(out_dir_umap, "umap_white_", var_name, ".png"),
         p3, width = 6, height = 5)
  
  # ============================
  # 10. CHECK
  # ============================
  cat("Righe finali:", nrow(df_plot), "\n")
  cat("✔ UMAP completato:", var_name, "\n")
}

for(v in macro_areas){
  analyze_umap(v)
}

# ============================
# 17. TEST NON LINEARITÀ
# ============================

for(i in 1:4){
  
  df_plot <- df_corr %>%
    select(
      x = all_of(macro_areas[i]),
      y = all_of(macro_areas_white[i])
    )
  
  p <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    theme_minimal() +
    labs(
      title = paste("Linear vs Nonlinear -", macro_areas[i]),
      subtitle = "Rosso = lineare | Blu = LOESS"
    )
  
  ggsave(paste0(out_dir, "nonlinear_", macro_areas[i], ".png"),
         p, width = 6, height = 5)
}


# ============================
#  18. TEST DISTRIBUZIONI BLACK VS WHITE
# ============================

out_dir_stats <- paste0(out_dir, "STATISTICAL_CHECKS/")
dir.create(out_dir_stats, showWarnings = FALSE)


for(i in 1:4){
  
  black <- df_corr[[macro_areas[i]]]
  white <- df_corr[[macro_areas_white[i]]]
  
  ks <- ks.test(
    black,
    white
  )
  
  wasser <- wasserstein1d(
    black,
    white
  )
  
  
  result <- data.frame(
    macroarea = macro_areas[i],
    mean_black = mean(black),
    mean_white = mean(white),
    median_black = median(black),
    median_white = median(white),
    KS_pvalue = ks$p.value,
    Wasserstein = wasser
  )
  
  
  print(result)
  
  write.csv(
    result,
    paste0(out_dir_stats,
           "distribution_test_",
           macro_areas[i],
           ".csv"),
    row.names = FALSE
  )
}



for(i in 1:4){
  
  x <- df_corr[[macro_areas[i]]]
  y <- df_corr[[macro_areas_white[i]]]
  
  
  mi <- mine(
    cbind(x,y)
  )
  
  
  cat(
    "\nMutual information -",
    macro_areas[i],
    ":",
    mi$MIC[1,2],
    "\n"
  )
}


out_dir_error <- paste0(out_dir,"ERROR_ANALYSIS/")
dir.create(out_dir_error, showWarnings = FALSE)



for(i in 1:4){
  
  
  df_error <- df_corr %>%
    mutate(
      error = abs(
        .data[[macro_areas[i]]] -
          .data[[macro_areas_white[i]]]
      )
    )
  
  
  # top 20 comuni con maggiore differenza
  
  top_error <- df_error %>%
    arrange(desc(error)) %>%
    select(
      pro_com,
      error,
      all_of(macro_areas[i]),
      all_of(macro_areas_white[i])
    ) %>%
    head(20)
  
  
  write.csv(
    top_error,
    paste0(
      out_dir_error,
      "top_errors_",
      macro_areas[i],
      ".csv"
    ),
    row.names = FALSE
  )
  
  
}

for(i in 1:4){
  
  
  name <- paste0(
    "relative_error_",
    macro_areas[i]
  )
  
  
  comuni_sf[[name]] <-
    abs(
      comuni_sf[[macro_areas[i]]] -
        comuni_sf[[macro_areas_white[i]]]
    ) /
    comuni_sf[[macro_areas_white[i]]]
  
  
  
  p <- ggplot(comuni_sf)+
    geom_sf(
      aes(fill=.data[[name]]),
      color=NA
    )+
    scale_fill_viridis_c()+
    theme_minimal()+
    labs(
      title=paste(
        "Errore relativo",
        macro_areas[i]
      )
    )
  
  
  ggsave(
    paste0(
      out_dir_error,
      name,
      ".png"
    ),
    p,
    width=8,
    height=6
  )
}


library(randomForest)


for(v in macro_areas){
  
  
  file_map <- list(
    indice_ambientale="dataset_macroarea_ambientale_2019.xlsx",
    indice_socio="dataset_macroarea_socio_2019.xlsx",
    indice_economico="dataset_macroarea_economica_2019.xlsx",
    indice_servizi="dataset_macroarea_servizi_2019.xlsx"
  )
  
  
  df_raw <- read_excel(
    paste0(
      file.path(data_root, "INDICI 2019/"),
      file_map[[v]]
    )
  ) %>%
    clean_names()
  
  
  df_rf <- df_raw %>%
    inner_join(
      indici %>% select(pro_com, all_of(v)),
      by="pro_com"
    ) %>%
    select(where(is.numeric)) %>%
    na.omit()
  
  
  model <- randomForest(
    as.formula(
      paste(v,"~.")
    ),
    data=df_rf,
    importance=TRUE
  )
  
  
  imp <- importance(model)
  
  
  write.csv(
    imp,
    paste0(
      out_dir_struct,
      "RF_importance_",
      v,
      ".csv"
    )
  )
}


library(FNN)

rank_preservation <- function(x,y){
  
  cor(
    rank(x),
    rank(y),
    method="spearman"
  )
  
}


for(v in macro_areas){
  
  cat(
    "\nRanking preservation:",
    v,
    "\n"
  )
  
  print(
    rank_preservation(
      df_corr[[v]],
      df_corr[[paste0(v,"_white")]]
    )
  )
  
}




for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    select(
      pro_com,
      black=all_of(v),
      white=all_of(paste0(v,"_white"))
    )%>%
    mutate(
      diff=black-white
    )
  
  
  write.csv(
    df_tmp %>% arrange(desc(diff)) %>% head(10),
    paste0(out_dir,"top_black_",v,".csv")
  )
  
  
  write.csv(
    df_tmp %>% arrange(diff) %>% head(10),
    paste0(out_dir,"top_white_",v,".csv")
  )
  
}



# ============================================================
# 19. CONTRIBUTO VARIABILI ALL'INDICE WHITE
# ============================================================


out_dir_white_contrib <- paste0(out_dir,
                                "WHITE_VARIABLE_CONTRIBUTION/")
dir.create(out_dir_white_contrib,
           showWarnings = FALSE)



file_map <- list(
  indice_ambientale = "dataset_macroarea_ambientale_2019.xlsx",
  indice_socio = "dataset_macroarea_socio_2019.xlsx",
  indice_economico = "dataset_macroarea_economica_2019.xlsx",
  indice_servizi = "dataset_macroarea_servizi_2019.xlsx"
)



for(v in macro_areas){
  
  
  df_raw <- read_excel(
    paste0(
      file.path(data_root, "INDICI 2019/"),
      file_map[[v]]
    )
  ) %>%
    clean_names() %>%
    select(where(is.numeric)) %>%
    na.omit()
  
  
  # rank transformation come white index
  
  Z <- apply(df_raw,2,function(x){
    
    (rank(x)-1)/(length(x)-1)
    
  })
  
  
  contribution <- Z^2 / rowSums(Z^2)
  
  
  mean_contribution <- colMeans(contribution)
  
  
  result <- data.frame(
    variable=names(mean_contribution),
    contribution=mean_contribution
  ) %>%
    arrange(desc(contribution))
  
  
  print(result)
  
  
  write.csv(
    result,
    paste0(
      out_dir_white_contrib,
      "white_contribution_",
      v,
      ".csv"
    ),
    row.names=FALSE
  )
  
}

# ============================================================
# 20. INTRINSIC DIMENSION WHITE VS BLACK
# ============================================================


out_dir_id <- paste0(out_dir,
                     "INTRINSIC_COMPARISON/")
dir.create(out_dir_id,
           showWarnings = FALSE)



for(v in macro_areas){
  
  
  black <- df_corr[[v]]
  white <- df_corr[[paste0(v,"_white")]]
  
  
  mat <- cbind(
    black,
    white
  )
  
  
  id <- est.twonn(
    scale(mat)
  )
  
  
  result <- data.frame(
    macroarea=v,
    intrinsic_dimension=id$estdim
  )
  
  
  print(result)
  
  
  write.csv(
    result,
    paste0(
      out_dir_id,
      "ID_black_white_",
      v,
      ".csv"
    ),
    row.names=FALSE
  )
}


# ============================================================
# 21. MORAN BLACK VS WHITE
# ============================================================


library(spdep)



out_dir_spatial <- paste0(out_dir,
                          "SPATIAL_ANALYSIS/")
dir.create(out_dir_spatial,
           showWarnings = FALSE)



coords <- st_coordinates(
  st_centroid(comuni_sf)
)


neighbors <- knn2nb(
  knearneigh(
    coords,
    k=8
  )
)


weights <- nb2listw(
  neighbors,
  style="W"
)



for(v in macro_areas){
  
  
  x1 <- comuni_sf[[v]]
  x2 <- comuni_sf[[paste0(v,"_white")]]
  
  
  moran_black <- moran.test(
    x1,
    weights
  )
  
  
  moran_white <- moran.test(
    x2,
    weights
  )
  
  
  result <- data.frame(
    macroarea=v,
    Moran_black=moran_black$estimate[1],
    Moran_white=moran_white$estimate[1],
    p_black=moran_black$p.value,
    p_white=moran_white$p.value
  )
  
  
  print(result)
  
  
  write.csv(
    result,
    paste0(
      out_dir_spatial,
      "moran_",
      v,
      ".csv"
    ),
    row.names=FALSE
  )
  
}
# ============================================================
# 22. MATRICE DISCORDANZA BLACK WHITE
# ============================================================


out_dir_discord <- paste0(out_dir,
                          "DISCORDANCE/")
dir.create(out_dir_discord,
           showWarnings=FALSE)



for(v in macro_areas){
  
  
  df_tmp <- df_corr %>%
    select(
      pro_com,
      black=all_of(v),
      white=all_of(paste0(v,"_white"))
    )
  
  
  df_tmp <- df_tmp %>%
    mutate(
      
      black_class=cut(
        black,
        quantile(black,
                 probs=c(0,0.5,1)),
        labels=c("low","high"),
        include.lowest=TRUE
      ),
      
      white_class=cut(
        white,
        quantile(white,
                 probs=c(0,0.5,1)),
        labels=c("low","high"),
        include.lowest=TRUE
      )
      
    )
  
  
  table_discord <- table(
    df_tmp$black_class,
    df_tmp$white_class
  )
  
  
  print(table_discord)
  
  
  write.csv(
    as.data.frame(table_discord),
    paste0(
      out_dir_discord,
      "discordance_table_",
      v,
      ".csv"
    )
  )
}


# ============================================================
# 23. DIFFERENZA RANK (BLACK vs WHITE)
# ============================================================

out_dir_rank <- paste0(out_dir, "RANK_ANALYSIS/")
dir.create(out_dir_rank, showWarnings = FALSE)

for(v in macro_areas){
  
  df_rank <- df_corr %>%
    mutate(
      rank_black = rank(.data[[v]]),
      rank_white = rank(.data[[paste0(v,"_white")]]),
      rank_diff = rank_black - rank_white
    ) %>%
    select(pro_com, rank_diff)
  
  # join TEMPORANEA (NON sovrascrivere comuni_sf)
  comuni_tmp <- comuni_sf %>%
    left_join(df_rank, by = "pro_com")
  
  # DEBUG utile (puoi lasciarlo o toglierlo)
  cat("\nCheck NA rank_diff:", sum(is.na(comuni_tmp$rank_diff)), "\n")
  
  p <- ggplot(comuni_tmp) +
    geom_sf(aes(fill = rank_diff), color = NA) +
    scale_fill_gradient2(low="blue", mid="white", high="red", na.value="grey90") +
    theme_minimal() +
    labs(title = paste("Rank difference (Black - White):", v))
  
  ggsave(paste0(out_dir_rank, "rank_diff_map_", v, ".png"),
         p, width=8, height=6)
}


# ============================================================
# 24. TRANSIZIONE TRA DECILI
# ============================================================

out_dir_decili <- paste0(out_dir, "DECILI_TRANSITION/")
dir.create(out_dir_decili, showWarnings = FALSE)

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10)
    )
  
  tab <- table(df_tmp$dec_black, df_tmp$dec_white)
  
  print(tab)
  
  write.csv(
    as.data.frame(tab),
    paste0(out_dir_decili, "transition_decili_", v, ".csv")
  )
}

# ============================================================
# 25. OVERLAP TOP 10% PIÙ FRAGILI
# ============================================================

out_dir_top <- paste0(out_dir, "TOP_FRAGILI/")
dir.create(out_dir_top, showWarnings = FALSE)

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10)
    )
  
  top_black <- df_tmp %>% filter(dec_black == 10)
  top_white <- df_tmp %>% filter(dec_white == 10)
  
  overlap <- length(
    intersect(top_black$pro_com, top_white$pro_com)
  )
  
  perc_overlap <- overlap / nrow(top_black)
  
  result <- data.frame(
    macroarea = v,
    overlap = overlap,
    perc_overlap = perc_overlap
  )
  
  print(result)
  
  write.csv(
    result,
    paste0(out_dir_top, "overlap_top10_", v, ".csv"),
    row.names = FALSE
  )
}


# ============================================================
# 26. SHIFT MEDIO DECILI
# ============================================================

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10),
      shift = abs(dec_black - dec_white)
    )
  
  mean_shift <- mean(df_tmp$shift)
  
  cat("\nShift medio decili -", v, ":", mean_shift, "\n")
}




# ============================================================
# SHIFT PER ZONE (ESTREMI vs CENTRO)
# ============================================================

out_dir_shift <- paste0(out_dir, "SHIFT_STRUCTURE/")
dir.create(out_dir_shift, showWarnings = FALSE)

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10),
      shift = abs(dec_black - dec_white)
    )
  
  df_tmp <- df_tmp %>%
    mutate(
      zona = case_when(
        dec_black >= 9 ~ "alta_fragilita",
        dec_black <= 3 ~ "bassa_fragilita",
        TRUE ~ "media"
      )
    )
  
  res <- df_tmp %>%
    group_by(zona) %>%
    summarise(
      mean_shift = mean(shift),
      n = n()
    )
  
  print(v)
  print(res)
  
  write.csv(result,
            paste0(out_dir_shift, "shift_zone_", v, ".csv"),
            row.names = FALSE)
}


# ============================================================
# OVERLAP TOP/BOTTOM 20%
# ============================================================

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10)
    )
  
  top_black <- df_tmp %>% filter(dec_black >= 9)
  top_white <- df_tmp %>% filter(dec_white >= 9)
  
  bottom_black <- df_tmp %>% filter(dec_black <= 2)
  bottom_white <- df_tmp %>% filter(dec_white <= 2)
  
  overlap_top <- length(intersect(top_black$pro_com, top_white$pro_com)) / nrow(top_black)
  overlap_bottom <- length(intersect(bottom_black$pro_com, bottom_white$pro_com)) / nrow(bottom_black)
  
  cat("\n", v, "\n")
  cat("Top 20% overlap:", overlap_top, "\n")
  cat("Bottom 20% overlap:", overlap_bottom, "\n")
}


# ============================================================
# DIREZIONE DELLO SHIFT
# ============================================================

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10),
      shift_signed = dec_black - dec_white
    )
  
  mean_shift <- mean(df_tmp$shift_signed)
  
  cat("\n", v, "- Shift medio SIGNED:", mean_shift, "\n")
}



# ============================================================
# MAPPA ERRORI GRANDI (>3 DECILI)
# ============================================================

for(v in macro_areas){
  
  df_tmp <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]], 10),
      dec_white = ntile(.data[[paste0(v,"_white")]], 10),
      shift = abs(dec_black - dec_white)
    ) %>%
    select(pro_com, shift)
  
  comuni_tmp <- comuni_sf %>%
    left_join(df_tmp, by = "pro_com")
  
  p <- ggplot(comuni_tmp) +
    geom_sf(aes(fill = shift > 3), color = NA) +
    scale_fill_manual(values = c("grey90","red")) +
    theme_minimal() +
    labs(title = paste("Errori rilevanti (>3 decili):", v))
  
  ggsave(paste0(out_dir, "big_errors_", v, ".png"),
         p, width=8, height=6)
}



# ============================================================
# 23. TRANSIZIONE DEI COMUNI AD ALTA FRAGILITA'
# ============================================================

out_dir_transition <- paste0(out_dir,
                             "HIGH_FRAGILITY_TRANSITION/")
dir.create(out_dir_transition,
           showWarnings = FALSE)


for(v in macro_areas){
  
  
  df_transition <- df_corr %>%
    mutate(
      
      dec_black = ntile(.data[[v]],10),
      dec_white = ntile(.data[[paste0(v,"_white")]],10)
      
    )
  
  
  # comuni più fragili secondo black-box
  critical_black <- df_transition %>%
    filter(dec_black == 10)
  
  
  transition_table <- critical_black %>%
    count(dec_white) %>%
    mutate(
      percentage = n / sum(n)
    )
  
  
  print(v)
  print(transition_table)
  
  
  write.csv(
    transition_table,
    paste0(
      out_dir_transition,
      "transition_black10_to_white_",
      v,
      ".csv"
    ),
    row.names = FALSE
  )
  
}


# ============================================================
# 24. PERDITA DEI COMUNI AD ALTA FRAGILITA' WHITE-BOX
# ============================================================

out_dir_loss <- paste0(out_dir,
                       "FRAGILITY_LOSS_ANALYSIS/")
dir.create(out_dir_loss,
           showWarnings = FALSE)


for(v in macro_areas){
  
  
  df_loss <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]],10),
      dec_white = ntile(.data[[paste0(v,"_white")]],10)
    )
  
  
  # comuni critici secondo white
  white_critical <- df_loss %>%
    filter(dec_white >= 9)
  
  
  # distribuzione dove finiscono nel black
  transition <- white_critical %>%
    count(dec_black) %>%
    mutate(
      percentage = n / sum(n)
    )
  
  
  cat("\n====================\n")
  cat(v,"\n")
  print(transition)
  
  
  write.csv(
    transition,
    paste0(
      out_dir_loss,
      "white_high_to_black_",
      v,
      ".csv"
    ),
    row.names=FALSE
  )
  
}
for(v in macro_areas){
  
  
  df_loss <- df_corr %>%
    mutate(
      dec_black = ntile(.data[[v]],10),
      dec_white = ntile(.data[[paste0(v,"_white")]],10)
    )
  
  
  lost <- df_loss %>%
    filter(
      dec_white >= 9 &
        dec_black <= 8
    )
  
  
  total <- df_loss %>%
    filter(dec_white >= 9)
  
  
  result <- data.frame(
    macroarea = v,
    comuni_persi = nrow(lost),
    totale_critici_white = nrow(total),
    percentuale_persi = nrow(lost)/nrow(total)
  )
  
  
  print(result)
  
  
}


# ============================================================
# OUTPUT DIRECTORY
# ============================================================
out_dir_rf <- paste0(out_dir, "RF_IMPORTANCE/")
dir.create(out_dir_rf, showWarnings = FALSE)

# ============================================================
# NOMI CUSTOM (COERENTI TESI)
# ============================================================
custom_names <- list(
  
  indice_ambientale = c(
    "Air quality index",
    "High-emission vehicles",
    "Unsorted waste",
    "Protected areas deviation",
    "Land consumption"
  ),
  
  indice_socio = c(
    "Migration rate",
    "Natural growth rate",
    "Old-age index",
    "Working-age share",
    "Population density",
    "Foreign population share"
  ),
  
  indice_economico = c(
    "Income per taxpayer",
    "Low-income taxpayers",
    "Employees per 100 residents",
    "Businesses per 1000 residents",
    "Graduates per 1000 residents"
  ),
  
  indice_servizi = c(
    "Retail space",
    "Personal service workers",
    "Service accessibility",
    "Bank deviation",
    "Museum deviation",
    "Healthcare deviation"
  )
)

# ============================================================
# FILE MAP
# ============================================================
file_map <- list(
  indice_ambientale="dataset_macroarea_ambientale_2019.xlsx",
  indice_socio="dataset_macroarea_socio_2019.xlsx",
  indice_economico="dataset_macroarea_economica_2019.xlsx",
  indice_servizi="dataset_macroarea_servizi_2019.xlsx"
)

# ============================================================
# STORAGE RISULTATI
# ============================================================
rf_all <- data.frame()

# ============================================================
# LOOP RANDOM FOREST
# ============================================================
for (v in macro_areas) {
  
  cat("\n====================\n")
  cat("MACROAREA:", v, "\n")
  cat("====================\n")
  
  # ----------------------------
  # LOAD RAW
  # ----------------------------
  df_raw <- read_excel(
    paste0(file.path(data_root, "INDICI 2019/"),
           file_map[[v]])
  ) %>%
    clean_names() %>%
    mutate(pro_com = as.character(pro_com))
  
  # ----------------------------
  # ALIGNMENT CON RESPONSE CORRETTO
  # ----------------------------
  df_join <- df_raw %>%
    inner_join(
      df_corr %>% select(pro_com, all_of(v)),
      by = "pro_com"
    )
  
  # ----------------------------
  # CHECK SICUREZZA
  # ----------------------------
  cat("Rows after join:", nrow(df_join), "\n")
  
  # ----------------------------
  # MODEL MATRIX
  # ----------------------------
  df_rf <- df_join %>%
    select(where(is.numeric)) %>%
    na.omit()
  
  y <- df_rf[[v]]
  x <- df_rf %>% select(-all_of(v))
  
  cat("Predictors:", ncol(x), "\n")
  
  # ----------------------------
  # RANDOM FOREST
  # ----------------------------
  model <- randomForest(
    x = x,
    y = y,
    importance = TRUE
  )
  
  imp <- importance(model)
  
  # ----------------------------
  # CHECK NOMI
  # ----------------------------
  if (nrow(imp) != length(custom_names[[v]])) {
    stop(paste(
      "Mismatch variabili in",
      v,
      ": RF =", nrow(imp),
      "custom =", length(custom_names[[v]])
    ))
  }
  
  # ----------------------------
  # OUTPUT
  # ----------------------------
  df_imp <- data.frame(
    variable = custom_names[[v]],
    importance = imp[, 1],
    macroarea = v
  )
  
  rf_all <- rbind(rf_all, df_imp)
}
# ============================================================
# PLOT UNICO (4 MACROAREE)
# ============================================================
rf_all$macroarea <- dplyr::recode(
  rf_all$macroarea,
  
  indice_ambientale = "Environmental",
  indice_socio = "Socio-demographic",
  indice_economico = "Economic",
  indice_servizi = "Services"
)

p <- ggplot(rf_all,
            aes(x = reorder(variable, importance),
                y = importance)) +
  
  geom_col(fill = "#2C7FB8", alpha = 0.9) +
  
  coord_flip() +
  
  facet_wrap(~macroarea, scales = "free_y") +
  
  theme_minimal(base_size = 12) +
  
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank()
  ) +
  
  labs(
    title = "Random Forest Variable Importance (Black-box index)",
    subtitle = "Feature contribution across macro-areas",
    x = NULL,
    y = "Importance"
  )

ggsave(
  paste0(out_dir_rf, "RF_importance_ALL_MACROAREE.png"),
  p,
  width = 12,
  height = 8
)
