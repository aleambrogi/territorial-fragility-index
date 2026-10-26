library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(stringr)
library(tidyr)

# =========================
# 📂 DATI
# =========================

dati <- readRDS("dati_finali_clean.rds") %>%
  mutate(codice_istat = sprintf("%06d", as.numeric(codice_istat)))

mappa <- st_read(
  "Com2021_g.shp",
  quiet = TRUE
) %>%
  clean_names() %>%
  filter(cod_reg == 3) %>%
  mutate(codice_istat = sprintf("%06d", as.numeric(pro_com))) %>%
  st_transform(4326)

dati_mappa <- mappa %>%
  left_join(dati, by = "codice_istat") %>%
  mutate(comune = comune.x)

# =========================
# VARIABILI
# =========================

variabili <- names(dati)
variabili <- variabili[str_detect(variabili, "_2019")]
variabili <- str_replace(variabili, "_2019.*$", "")
variabili <- unique(variabili)

variabili <- variabili[!variabili %in% c(
  "codice_istat","cod_reg","cod_uts","cc_uts","cod_rip","rischio"
)]

# =========================
# FUNZIONI
# =========================

get_col <- function(var, year){
  cols <- names(dati_mappa)
  hit <- cols[str_detect(cols, paste0("^", var, "_", year))]
  if(length(hit)==0) return(NA)
  hit[1]
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

apply_scale <- function(df, input){
  
  if(input$scala != "Quantili") return(df)
  
  br <- unique(quantile(df$raw,
                        probs=seq(0,1,length.out=input$nq+1),
                        na.rm=TRUE))
  
  df$raw <- as.numeric(cut(df$raw,
                           breaks=br,
                           include.lowest=TRUE,
                           labels=FALSE))
  
  df
}

prepare_hist_data <- function(df, input){
  
  if(input$scala == "Quantili"){
    
    df$quantile <- factor(df$raw)
    
  }
  
  df
}

# =========================
# DIREZIONE (NON TOCCATA)
# =========================

direction_hint <- list(
  indice_inquinamento = "high_bad",
  tasso_di_motorizzazione_ad_alta_emissione = "high_bad",
  raccolta_indifferenziata_dei_rifiuti_urbani_per_abitante = "high_bad",
  scarto_aree_protette = "high_good",
  rischio = "high_bad",
  consumo_del_suolo = "high_bad",
  
  reddito_medio_per_contribuente = "high_good",
  contribuenti_irpef_con_reddito_complessivo_inferiore_a_10_000_euro_nel = "high_bad",
  addetti_100_res = "high_good",
  imprese_per_1000_ab = "high_good",
  laureati_per_1000_ab = "high_good",
  superficie_esercizi_commerciali_per_kmq = "high_good",
  addetti_dei_servizi_alla_persona_per_1_000_abitanti_nel = "high_good",
  
  indice_di_accessibilita_ai_servizi_essenziali = "high_bad",
  sportelli_dev = "high_good",
  musei_dev = "high_good",
  sanita_dev = "high_good",
  
  tasso_migratorio_totale = "high_good",
  tasso_crescita_naturale = "high_good",
  ind_vecchiaia_19 = "high_bad",
  quota_pop_attiva_19 = "high_good",
  densita_abitativa = "high_good",
  stranieri_pop = "high_bad",
  
  indice_ambientale = "high_bad",
  indice_socio = "high_bad",
  indice_economico = "high_bad",
  indice_servizi = "high_bad"
)

get_dir <- function(v){
  if(v %in% names(direction_hint)) direction_hint[[v]] else "high_bad"
}

# =========================
# UI
# =========================

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#eef2f7;}
    .header{background:#1f3c88;color:white;padding:15px;border-radius:10px;}
    
    .card{
      background:white;
      padding:15px;
      border-radius:10px;
      box-shadow:0 2px 8px rgba(0,0,0,0.1);
      margin-bottom:15px;
    }
  "))),
  
  div(class="header","Fragilità territoriale"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mode","Modalità:",c("Singola","Confronto")),
      selectInput("var","Variabile:",variabili),
      selectInput("anno","Anno:",c("2019","2021")),
      
      selectInput("scala","Scala:",c("Continua","Quantili")),
      conditionalPanel(
        "input.scala == 'Quantili'",
        sliderInput("nq","Quantili",3,10,5)
      ),
      
      selectInput("color_dir","Direzione:",
                  c("Alto = Rosso","Alto = Verde")),
      selectizeInput("search","Comune",choices=NULL)
    ),
    
    mainPanel(uiOutput("ui_main"))
  )
)

# =========================
# SERVER
# =========================

server <- function(input, session, output){
  
  selected <- reactiveVal(NULL)
  
  observe({
    updateSelectizeInput(session,"search",
                         choices=sort(unique(dati_mappa$comune)),
                         server=TRUE)
  })
  
  observeEvent(input$search,{
    id <- dati_mappa %>% filter(comune==input$search) %>% pull(codice_istat)
    if(length(id)>0) selected(id[1])
  })
  
  observeEvent(input$map1_shape_click$id, selected(input$map1_shape_click$id))
  observeEvent(input$map2019_shape_click$id, selected(input$map2019_shape_click$id))
  observeEvent(input$map2021_shape_click$id, selected(input$map2021_shape_click$id))
  
  # =========================
  # DATA
  # =========================
  
  dati_reactive <- reactive({
    
    v <- input$var
    v19 <- get_col(v,"2019")
    v21 <- get_col(v,"2021")
    
    df <- dati_mappa
    
    df$raw <- if(input$anno=="2019") safe_num(df[[v19]]) else safe_num(df[[v21]])
    df <- df %>% filter(!is.na(raw))
    
    df
  })
  
  palette_fun <- function(df){
    pal <- colorRampPalette(c("green","yellow","red"))(100)
    if(input$color_dir=="Alto = Verde") pal <- rev(pal)
    colorNumeric(pal, df$raw)
  }
  
  # =========================
  # UI MAIN
  # =========================
  
  output$ui_main <- renderUI({
    
    if(input$mode=="Singola"){
      
      tagList(
        leafletOutput("map1",height=600),
        
        fluidRow(
          column(6, plotOutput("hist")),
          column(6, plotOutput("trend"))
        )
      )
      
    } else {
      
      tagList(
        fluidRow(
          column(6, leafletOutput("map2019",height=600)),
          column(6, leafletOutput("map2021",height=600))
        ),
        
        fluidRow(
          column(6,
                 div(class="card",
                     h4("Distribuzione 2019"),
                     plotOutput("hist2019")
                 )
          ),
          column(6,
                 div(class="card",
                     h4("Distribuzione 2021"),
                     plotOutput("hist2021")
                 )
          )
        ),
        
        div(class="card",
            h4("Confronto temporale"),
            plotOutput("trend_compare", height=300)
        )
      )
    }
  })
  
  # =========================
  # MAPPE
  # =========================
  
  render_map_year <- function(year){
    
    renderLeaflet({
      
      df <- dati_mappa
      col <- get_col(input$var,year)
      
      df$raw <- safe_num(df[[col]])
      df <- df %>% filter(!is.na(raw))
      df <- apply_scale(df, input)
      
      pal <- palette_fun(df)
      sel <- selected()
      
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          layerId=~codice_istat,
          fillColor=~pal(raw),
          fillOpacity=0.7,
          color=~ifelse(!is.null(sel) & codice_istat==sel,"black","white"),
          weight=~ifelse(!is.null(sel) & codice_istat==sel,3,0.5),
          label=~paste0(comune," → ",round(raw,2))
        )
    })
  }
  
  output$map1 <- render_map_year(input$anno)
  output$map2019 <- render_map_year("2019")
  output$map2021 <- render_map_year("2021")
  
  # =========================
  # HIST
  # =========================
  
  output$hist <- renderPlot({
    
    df <- dati_reactive()
    sel <- selected()
    
    df_sel <- if(!is.null(sel)) df[df$codice_istat==sel,] else df[0,]
    
    if(input$scala=="Quantili"){
      
      df$quantile <- factor(df$raw)
      
      ggplot(df, aes(x=quantile))+
        geom_bar(fill="steelblue", color="white")+
        {if(nrow(df_sel)>0) geom_vline(
          xintercept=as.numeric(factor(df_sel$raw, levels=levels(df$quantile))),
          color="red", size=1.5
        )}+
        labs(x = "Quantile", y = "Numero comuni")+
        theme_minimal()
      
    } else {
      
      ggplot(df,aes(x=raw, y=after_stat(count)))+
        geom_histogram(bins=30,fill="steelblue",color="white")+
        {if(nrow(df_sel)>0) geom_vline(xintercept=df_sel$raw,color="red",size=1.2)}+
        labs(x = "Valore", y = "Numero comuni")+
        theme_minimal()
    }
  })
  
  output$hist2019 <- renderPlot({
    
    df <- dati_mappa
    df$raw <- safe_num(df[[get_col(input$var,"2019")]])
    df <- df %>% filter(!is.na(raw))
    df <- apply_scale(df, input)
    
    sel <- selected()
    df_sel <- if(!is.null(sel)) df[df$codice_istat==sel,] else df[0,]
    
    if(input$scala=="Quantili"){
      
      df$quantile <- factor(df$raw)
      
      ggplot(df, aes(x=quantile))+
        geom_bar(fill="darkgreen", color="white")+
        {if(nrow(df_sel)>0)
          geom_vline(
            xintercept=as.numeric(factor(df_sel$raw, levels=levels(df$quantile))),
            color="red", size=1.5
          )}+
        labs(x="Quantili", y="Numero comuni")+
        theme_minimal()
      
    } else {
      
      ggplot(df,aes(x=raw, y=after_stat(count)))+
        geom_histogram(bins=30,fill="darkgreen",color="white")+
        {if(nrow(df_sel)>0)
          geom_vline(xintercept=df_sel$raw,color="red",size=1.2)}+
        labs(x="Valore", y="Numero comuni")+
        theme_minimal()
    }
  })
  
  output$hist2021 <- renderPlot({
    
    df <- dati_mappa
    df$raw <- safe_num(df[[get_col(input$var,"2021")]])
    df <- df %>% filter(!is.na(raw))
    df <- apply_scale(df, input)
    
    sel <- selected()
    df_sel <- if(!is.null(sel)) df[df$codice_istat==sel,] else df[0,]
    
    if(input$scala=="Quantili"){
      
      df$quantile <- factor(df$raw)
      
      ggplot(df, aes(x=quantile))+
        geom_bar(fill="darkred", color="white")+
        {if(nrow(df_sel)>0)
          geom_vline(
            xintercept=as.numeric(factor(df_sel$raw, levels=levels(df$quantile))),
            color="red", size=1.5
          )}+
        labs(x="Quantili", y="Numero comuni")+
        theme_minimal()
      
    } else {
      
      ggplot(df,aes(x=raw, y=after_stat(count)))+
        geom_histogram(bins=30,fill="darkred",color="white")+
        {if(nrow(df_sel)>0)
          geom_vline(xintercept=df_sel$raw,color="red",size=1.2)}+
        labs(x="Valore", y="Numero comuni")+
        theme_minimal()
    }
  })
  
  # =========================
  # TREND
  # =========================
  
  output$trend <- renderPlot({
    req(selected())
    
    df <- dati_mappa %>% filter(codice_istat==selected())
    
    data.frame(
      anno=c(2019,2021),
      valore=c(
        safe_num(df[[get_col(input$var,"2019")]]),
        safe_num(df[[get_col(input$var,"2021")]])
      )
    ) %>%
      ggplot(aes(anno,valore))+
      geom_line()+
      geom_point(size=3)+
      theme_minimal()+
      labs(
        title = "Evoluzione temporale",
        x = "Anno",
        y = "Valore"
      )
  })
  
  output$trend_compare <- renderPlot({
    req(selected())
    
    df <- dati_mappa %>% filter(codice_istat==selected())
    
    df_plot <- data.frame(
      anno=c(2019,2021),
      valore=c(
        safe_num(df[[get_col(input$var,"2019")]]),
        safe_num(df[[get_col(input$var,"2021")]])
      )
    )
    
    ggplot(df_plot,aes(anno,valore))+
      geom_line(group=1)+
      geom_point(size=3)+
      theme_minimal()+
      labs(
        x = "Anno",
        y = "Valore"
      )
  })
  
}

shinyApp(ui, server)

