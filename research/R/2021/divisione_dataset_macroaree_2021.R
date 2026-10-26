data_root <- Sys.getenv("THESIS_DATA_DIR", unset = "data/external/TESI")
external_root <- Sys.getenv("THESIS_EXTERNAL_DIR", unset = "data/external")

library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(corrplot)
library(sf)
library(janitor)


# ============================
# DATASET BASE
# ============================

dataset_cov <- read_excel(
  file.path(external_root, "[Tesi AMBROGI] Kick off/dataset_2021.xlsx"),
  sheet = 2, 
  skip = 1
)


dataset_env <- read_excel(
  file.path(data_root, "DATASET PER AMBIENTE 2021/dataset_ambientale_2021.xlsx")
)


dataset_inquinanti <- read_excel(
  file.path(data_root, "superamenti_comuni_2021.xlsx")
) %>%
  mutate(
    comune = toupper(comune)
  )


dataset_rischio <- read_excel(
  file.path(data_root, "DATASET PER AMBIENTE 2021/risk_total_2021.xlsx")
)


dataset_addetti <- read_excel(
  file.path(data_root, "addetti_100_residenti_2021.xlsx")
) %>%
  setNames(c("pro_com", "comune", "addetti_100_res")) %>%
  mutate(
    pro_com = as.character(pro_com),
    pro_com = sub("^0+", "", pro_com),
    addetti_100_res = as.numeric(gsub(",", ".", addetti_100_res))
  )


dataset_rischio <- dataset_rischio %>%
  mutate(
    pro_com = as.character(PRO_COM)
  )



############################
# SHAPEFILE CORRETTO
############################

path_shape <- file.path(external_root, "Limiti2021_g/Com2021_g/Com2021_g.shp")


shapefile <- st_read(
  path_shape,
  quiet = TRUE
) %>%
  st_transform(32632) %>%
  clean_names() %>%
  filter(cod_reg == 3) %>%
  mutate(
    pro_com = as.character(pro_com)
  )


nrow(shapefile)



############################
# FUNZIONE RICOSTRUZIONE FUSIONI
############################

ricostruisci_comune <- function(df,
                                vecchi_comuni,
                                nuovo_nome,
                                nuovo_codice){
  
  
  dati <- df %>%
    filter(
      comune %in% vecchi_comuni
    )
  
  
  if(nrow(dati) == 0){
    return(NULL)
  }
  
  
  variabili_num <- dati %>%
    select(where(is.numeric)) %>%
    names()
  
  
  nuova_riga <- dati %>%
    summarise(
      across(
        all_of(variabili_num),
        ~ifelse(
          all(is.na(.x)),
          NA,
          mean(.x, na.rm = TRUE)
        )
      )
    ) %>%
    mutate(
      pro_com = nuovo_codice,
      comune = nuovo_nome
    )
  
  
  nuova_riga
  
}



############################
# MACROAREA AMBIENTALE
############################


dataset_env <- dataset_env %>%
  mutate(
    pro_com = as.character(pro_com)
  )



dataset_env <- dataset_env %>%
  bind_rows(
    
    ricostruisci_comune(
      dataset_env,
      c("Bardello",
        "Malgesso",
        "Bregano"),
      "Bardello con Malgesso e Bregano",
      "12144"
    ),
    
    ricostruisci_comune(
      dataset_env,
      c("Uggiate-Trevano",
        "Ronago"),
      "Uggiate con Ronago",
      "13256"
    ),
    
    ricostruisci_comune(
      dataset_env,
      c("Campospinoso",
        "Albaredo Arnaboldi"),
      "Campospinoso Albaredo",
      "18026"
    )
    
  ) %>%
  distinct(pro_com, .keep_all = TRUE)



ambientale <- dataset_env %>%
  
  left_join(
    dataset_cov %>%
      select(
        `Codice Comune (num)`,
        `Grado di urbanizzazione`
      ) %>%
      rename(
        pro_com = `Codice Comune (num)`
      ) %>%
      mutate(
        pro_com = as.character(pro_com)
      ),
    by = "pro_com"
  ) %>%
  
  
  left_join(
    dataset_inquinanti %>%
      select(
        pro_com,
        indice_superamenti
      ),
    by = "pro_com"
  ) %>%
  
  
  left_join(
    dataset_rischio %>%
      select(
        pro_com,
        risk_combo
      ),
    by = "pro_com"
  ) %>%
  
  
  group_by(
    `Grado di urbanizzazione`
  ) %>%
  
  mutate(
    media_aree_protette =
      mean(
        aree_protette,
        na.rm = TRUE
      ),
    
    scarto_aree_protette =
      aree_protette -
      media_aree_protette
  ) %>%
  
  ungroup() %>%
  
  select(
    pro_com,
    comune,
    indice_inquinamento = indice_superamenti,
    tasso_di_motorizzazione_ad_alta_emissione,
    raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante,
    scarto_aree_protette,
    rischio = risk_combo,
    consumo_del_suolo
  )



############################
# IMPUTAZIONE INDICE INQUINAMENTO
# (COMUNI CON MANCANTI)
############################


comuni_fix_inquinamento <- c(
  "Borgo Mantovano",
  "Borgocarbonara",
  "Cadrezzate con Osmate",
  "Castelgerundo",
  "Centro Valle Intelvi",
  "Colli Verdi",
  "Piadena Drizzona",
  "Solbiate con Cagno",
  "Torre de' Busi",
  "Valvarrone",
  "Vermezzo con Zelo",
  "Campospinoso Albaredo",
  "Uggiate con Ronago",
  "Bardello con Malgesso e Bregano"
)



media_confinanti <- function(codice, shapefile, dati){
  
  geom_target <- shapefile %>%
    filter(pro_com == codice)
  
  if(nrow(geom_target) == 0){
    return(NA_real_)
  }
  
  
  confinanti <- shapefile %>%
    filter(
      st_touches(
        geometry,
        geom_target$geometry,
        sparse = FALSE
      )[,1]
    ) %>%
    pull(pro_com)
  
  
  valori <- dati %>%
    filter(
      pro_com %in% confinanti
    ) %>%
    pull(indice_inquinamento)
  
  
  valori <- valori[!is.na(valori)]
  
  
  if(length(valori) == 0){
    return(NA_real_)
  }
  
  
  mean(valori)
}



for(i in 1:nrow(ambientale)){
  
  if(
    is.na(ambientale$indice_inquinamento[i]) &
    ambientale$comune[i] %in% comuni_fix_inquinamento
  ){
    
    codice <- ambientale$pro_com[i]
    
    val <- media_confinanti(
      codice,
      shapefile,
      ambientale
    )
    
    if(!is.na(val)){
      ambientale$indice_inquinamento[i] <- val
    }
  }
}




############################
# SOCIO DEMOGRAFICO
############################


socio <- dataset_cov %>%
  
  select(
    `Codice Comune (num)`,
    `Denominazione comune`,
    `Tasso migratorio totale 2021`,
    `Tasso crescita naturale 2021`,
    Ind_vecchiaia_21,
    Quota_pop_attiva_21,
    `Densità abitativa 2021`,
    `Stranieri/pop 2021`
  ) %>%
  
  rename(
    pro_com = `Codice Comune (num)`,
    comune = `Denominazione comune`
  ) %>%
  
  mutate(
    pro_com = as.character(pro_com)
  )



socio <- socio %>%
  
  bind_rows(
    
    ricostruisci_comune(
      socio,
      c("Bardello",
        "Malgesso",
        "Bregano"),
      "Bardello con Malgesso e Bregano",
      "12144"
    ),
    
    ricostruisci_comune(
      socio,
      c("Uggiate-Trevano",
        "Ronago"),
      "Uggiate con Ronago",
      "13256"
    ),
    
    ricostruisci_comune(
      socio,
      c("Campospinoso",
        "Albaredo Arnaboldi"),
      "Campospinoso Albaredo",
      "18026"
    )
    
  )


############################
# ECONOMICO
############################


economico <- dataset_cov %>%
  
  select(
    `Codice Comune (num)`,
    `Denominazione comune`,
    `Popolazione 1gen2021`,
    `Imprese 2021`,
    `Residenti 9+ Almeno laurea II livello 2021`,
    `% Addetti alle imprese su popolazione 2021`,
    `Reddito medio per contribuente 2021`,
    `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2021 (%)`
  ) %>%
  
  rename(
    pro_com = `Codice Comune (num)`,
    comune = `Denominazione comune`
  ) %>%
  
  mutate(
    pro_com = as.character(pro_com)
  ) %>%
  
  left_join(
    dataset_addetti %>%
      select(
        pro_com,
        addetti_100_res
      ),
    by = "pro_com"
  ) %>%
  
  mutate(
    imprese_per_1000_ab =
      (`Imprese 2021` /
         `Popolazione 1gen2021`) * 1000,
    
    laureati_per_1000_ab =
      (`Residenti 9+ Almeno laurea II livello 2021` /
         `Popolazione 1gen2021`) * 1000
  ) %>%
  
  select(
    pro_com,
    comune,
    `Reddito medio per contribuente 2021`,
    `Contribuenti IRPEF con reddito complessivo inferiore a 10.000 euro nel 2021 (%)`,
    addetti_100_res,
    imprese_per_1000_ab,
    laureati_per_1000_ab
  )



economico <- economico %>%
  
  bind_rows(
    
    ricostruisci_comune(
      economico,
      c("Bardello",
        "Malgesso",
        "Bregano"),
      "Bardello con Malgesso e Bregano",
      "12144"
    ),
    
    ricostruisci_comune(
      economico,
      c("Uggiate-Trevano",
        "Ronago"),
      "Uggiate con Ronago",
      "13256"
    ),
    
    ricostruisci_comune(
      economico,
      c("Campospinoso",
        "Albaredo Arnaboldi"),
      "Campospinoso Albaredo",
      "18026"
    )
    
  )



############################
# FIX ADDETTI COMUNI FUSI
############################


economico <- economico %>%
  mutate(
    addetti_100_res = case_when(
      
      comune == "Bardello con Malgesso e Bregano" ~ 
        mean(c(61.9, 57.0, 59.6), na.rm = TRUE),
      
      comune == "Uggiate con Ronago" ~ 
        mean(c(30.0, 35.9), na.rm = TRUE),
      
      comune == "Campospinoso Albaredo" ~ 
        addetti_100_res,
      
      comune == "Vermezzo con Zelo" ~ 
        addetti_100_res,
      
      TRUE ~ addetti_100_res
    )
  )



mediana_addetti_econ <- median(
  economico$addetti_100_res,
  na.rm = TRUE
)



economico <- economico %>%
  mutate(
    addetti_100_res = ifelse(
      is.na(addetti_100_res),
      mediana_addetti_econ,
      addetti_100_res
    )
  )




############################
# SERVIZI
############################


servizi <- dataset_cov %>%
  
  select(
    `Codice Comune (num)`,
    `Denominazione comune`,
    `Consistenza strutture sanitarie per 1.000 abitanti nel 2021`,
    `Superficie esercizi commerciali per kmq 2021`,
    `Addetti dei servizi alla persona per 1.000 abitanti nel 2021`,
    `Musei e similari per 10.000 abitanti (2021)`,
    `Sportelli bancari per 1000 abitanti (2021)`,
    area_interna_2021
  ) %>%
  
  rename(
    pro_com = `Codice Comune (num)`,
    comune = `Denominazione comune`
  ) %>%
  
  mutate(
    pro_com = as.character(pro_com)
  ) %>%
  
  
  left_join(
    dataset_env %>%
      select(
        pro_com,
        indice_di_accessibilita_ai_servizi_essenziali
      ) %>%
      mutate(
        pro_com = as.character(pro_com)
      ),
    by = "pro_com"
  ) %>%
  
  
  group_by(
    area_interna_2021
  ) %>%
  
  mutate(
    
    sportelli_dev =
      `Sportelli bancari per 1000 abitanti (2021)` -
      mean(
        `Sportelli bancari per 1000 abitanti (2021)`,
        na.rm = TRUE
      ),
    
    
    musei_dev =
      `Musei e similari per 10.000 abitanti (2021)` -
      mean(
        `Musei e similari per 10.000 abitanti (2021)`,
        na.rm = TRUE
      ),
    
    
    sanita_dev =
      `Consistenza strutture sanitarie per 1.000 abitanti nel 2021` -
      mean(
        `Consistenza strutture sanitarie per 1.000 abitanti nel 2021`,
        na.rm = TRUE
      )
    
  ) %>%
  
  ungroup() %>%
  
  select(
    pro_com,
    comune,
    `Superficie esercizi commerciali per kmq 2021`,
    `Addetti dei servizi alla persona per 1.000 abitanti nel 2021`,
    indice_di_accessibilita_ai_servizi_essenziali,
    sportelli_dev,
    musei_dev,
    sanita_dev
  )



servizi <- servizi %>%
  
  bind_rows(
    
    ricostruisci_comune(
      servizi,
      c("Bardello",
        "Malgesso",
        "Bregano"),
      "Bardello con Malgesso e Bregano",
      "12144"
    ),
    
    ricostruisci_comune(
      servizi,
      c("Uggiate-Trevano",
        "Ronago"),
      "Uggiate con Ronago",
      "13256"
    ),
    
    ricostruisci_comune(
      servizi,
      c("Campospinoso",
        "Albaredo Arnaboldi"),
      "Campospinoso Albaredo",
      "18026"
    )
    
  )





############################
# CHECK COPERTURA
############################


ambientale <- ambientale %>%
  mutate(
    pro_com = as.character(pro_com)
  )


socio <- socio %>%
  mutate(
    pro_com = as.character(pro_com)
  )


economico <- economico %>%
  mutate(
    pro_com = as.character(pro_com)
  )


servizi <- servizi %>%
  mutate(
    pro_com = as.character(pro_com)
  )



check_comuni <- list(
  ambientale = ambientale,
  socio = socio,
  economico = economico,
  servizi = servizi
)



# numero comuni per macroarea

lapply(
  check_comuni,
  nrow
)



# comuni presenti nello shapefile ma mancanti nei dataset

lapply(
  check_comuni,
  function(x){
    
    shapefile %>%
      st_drop_geometry() %>%
      anti_join(
        x,
        by = "pro_com"
      ) %>%
      select(
        pro_com,
        comune
      )
    
  }
)



# duplicati residui

lapply(
  check_comuni,
  function(x){
    
    x %>%
      count(
        pro_com,
        comune
      ) %>%
      filter(
        n > 1
      )
    
  }
)





############################
# DATASET FINALE
############################


dataset_totale <- ambientale %>%
  
  inner_join(
    socio,
    by = c(
      "pro_com",
      "comune"
    )
  ) %>%
  
  inner_join(
    economico,
    by = c(
      "pro_com",
      "comune"
    )
  ) %>%
  
  inner_join(
    servizi,
    by = c(
      "pro_com",
      "comune"
    )
  )



nrow(dataset_totale)





############################
# CONTROLLO NA
############################


na_dettaglio <- dataset_totale %>%
  
  select(
    pro_com,
    comune,
    where(is.numeric)
  ) %>%
  
  pivot_longer(
    cols = -c(
      pro_com,
      comune
    ),
    names_to = "variabile",
    values_to = "valore"
  ) %>%
  
  filter(
    is.na(valore)
  ) %>%
  
  arrange(
    variabile
  )


na_dettaglio





############################
# IMPUTAZIONE ADDETTI
############################


mediana_addetti <- median(
  dataset_totale$addetti_100_res,
  na.rm = TRUE
)


dataset_totale <- dataset_totale %>%
  
  mutate(
    addetti_100_res = ifelse(
      is.na(addetti_100_res),
      mediana_addetti,
      addetti_100_res
    )
  )





############################
# CHECK NA COMPLETO
############################


check_na <- function(df, nome){
  
  cat("\n====================\n")
  cat("DATASET:", nome, "\n")
  cat("====================\n")
  
  
  na_count <- df %>%
    summarise(
      across(
        where(is.numeric),
        ~sum(is.na(.))
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = "variabile",
      values_to = "n_na"
    ) %>%
    filter(
      n_na > 0
    )
  
  
  print(na_count)
}



check_na(ambientale, "AMBIENTALE")
check_na(socio, "SOCIO")
check_na(economico, "ECONOMICO")
check_na(servizi, "SERVIZI")
check_na(dataset_totale, "DATASET TOTALE")





############################
# CONTROLLO DUPLICATI FINALE
############################


dataset_totale %>%
  count(
    pro_com,
    comune
  ) %>%
  filter(
    n > 1
  )





############################
# EXPORT
############################


write_xlsx(
  ambientale,
  file.path(data_root, "INDICI 2021/dataset_macroarea_ambientale_2021.xlsx")
)


write_xlsx(
  socio,
  file.path(data_root, "INDICI 2021/dataset_macroarea_socio_2021.xlsx")
)


write_xlsx(
  economico,
  file.path(data_root, "INDICI 2021/dataset_macroarea_economica_2021.xlsx")
)


write_xlsx(
  servizi,
  file.path(data_root, "INDICI 2021/dataset_macroarea_servizi_2021.xlsx")
)


write_xlsx(
  dataset_totale,
  file.path(data_root, "INDICI 2021/dataset_totale_2021.xlsx")
)



write_xlsx(
  list(
    ambientale = ambientale,
    socio = socio,
    economico = economico,
    servizi = servizi,
    dataset_totale = dataset_totale
  ),
  file.path(data_root, "INDICI 2021/dataset_completo_2021.xlsx")
)

