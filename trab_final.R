# Limpeza do ambiente de trabalho
rm(list = ls())

# ==============================================================================
# TRABALHO FINAL: APLICAÇÃO SHINY INTERATIVA (MALÁRIA X DESMATAMENTO 2020-2024)
# ==============================================================================

library(sf)
library(tidyverse)
library(leaflet)
library(shiny)
library(viridis)
library(htmlwidgets)
library(foreign)
library(geobr)
library(dplyr)

pasta <- "C:/Users/jandy/OneDrive/Documentos/Doutorado/Fiocruz/Disciplinas/Cartografia/Trab Final"

# ------------------------------------------------------------------------------
# 1. LEITURA E CONSOLIDAÇÃO DOS DADOS (2020 A 2024)
# ------------------------------------------------------------------------------

# 1.1 Leitura do Shapefile do IBGE e dados do DETER
ibge_amz <- st_read(file.path(pasta, "municipalities_legal_amazon.shp"))
deter    <- read.dbf(file.path(pasta, "deter-amz-deter-public.dbf"))

# 1.2 Leitura e empilhamento dos arquivos RDS locais (2020 a 2024)
anos <- 2020:2024
lista_malaria <- list()

for (ano_i in anos) {
  nome_arq <- paste0("malaria", ano_i, "_processada.rds")
  caminho <- file.path(pasta, nome_arq)
  
  if (file.exists(caminho)) {
    lista_malaria[[as.character(ano_i)]] <- readRDS(caminho)
  }
}

sivep <- bind_rows(lista_malaria)

# ------------------------------------------------------------------------------
# 2. GERAR E SALVAR A PLANILHA CONSOLIDADA (CSV)
# ------------------------------------------------------------------------------

sivep_resumo <- sivep %>%
  mutate(ano = lubridate::year(DT_NOTIF)) %>% 
  group_by(MUN_NOTI, ano) %>%
  summarise(
    casos_vivax = sum(VIVAX, na.rm = TRUE),
    casos_falciparum = sum(FALCIPARUM, na.rm = TRUE),
    total_casos = casos_vivax + casos_falciparum,
    .groups = "drop"
  )

write.csv2(
  sivep_resumo, 
  file.path(pasta, "malaria_2020_2024_consolidado.csv"), 
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 3. TRATAMENTO DOS DADOS E GERAÇÃO DO GEOPACKAGE (.GPKG)
# ------------------------------------------------------------------------------

ibge_tratado <- ibge_amz %>%
  mutate(code_muni_6 = str_sub(as.character(geocodigo), 1, 6))

sivep_resumo_tratado <- sivep_resumo %>%
  mutate(MUN_NOTI = str_sub(as.character(MUN_NOTI), 1, 6))

deter_tratado <- deter %>%
  mutate(
    code_muni_6 = str_sub(as.character(GEOCODIBGE), 1, 6),
    ano = lubridate::year(VIEW_DATE)
  ) %>%
  filter(ano %in% 2020:2024) %>%
  group_by(code_muni_6, ano) %>%
  summarise(
    area_desmatada_km2 = sum(AREAMUNKM, na.rm = TRUE),
    total_alertas = n(),
    .groups = "drop"
  )

# Junção dos dados espaciais com epidemio e desmatamento
dados_finais <- ibge_tratado %>%
  left_join(sivep_resumo_tratado, by = c("code_muni_6" = "MUN_NOTI")) %>%
  left_join(deter_tratado, by = c("code_muni_6", "ano" ))

# Identifica automaticamente o nome do campo da UF (seja UF, sigla_uf, etc.)
campo_uf <- names(dados_finais)[grepl("^uf|^sigla|^abbrev", names(dados_finais), ignore.case = TRUE)][1]

estados_amz <- c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")

# Aplica o filtro de forma dinâmica sem quebrar por nome de coluna
if (!is.na(campo_uf)) {
  dados_finais_amz <- dados_finais %>%
    filter(.data[[campo_uf]] %in% estados_amz)
} else {
  # Caso não encontre coluna de texto, usa o shapefile integral (que já é da AMZ Legal)
  dados_finais_amz <- dados_finais
}

# Salvar o GPKG filtrado localmente na pasta de trabalho
st_write(
  dados_finais_amz, 
  file.path(pasta, "malaria_desmatamento_pronto.gpkg"), 
  delete_dsn = TRUE
)

# Preparação da base específica para renderização no Shiny (CRS 4326)
dados_shiny <- dados_finais %>%
  st_transform(crs = 4326) %>%
  filter(!is.na(ano) & ano %in% 2020:2024) %>%
  mutate(
    total_casos        = replace_na(total_casos, 0),
    area_desmatada_km2 = replace_na(area_desmatada_km2, 0)
  )

bbox_amz <- st_bbox(dados_shiny)

# ------------------------------------------------------------------------------
# 4. INTERFACE DO USUÁRIO (UI)
# ------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Monitoramento de Malária e Desmatamento - Amazônia Legal (2020-2024)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        inputId = "ano_selecionado",
        label = "Selecione o Ano:",
        min = min(dados_shiny$ano, na.rm = TRUE),
        max = max(dados_shiny$ano, na.rm = TRUE),
        value = min(dados_shiny$ano, na.rm = TRUE),
        sep = "",
        step = 1,
        animate = animationOptions(interval = 1500, loop = FALSE)
      ),
      hr(),
      radioButtons(
        inputId = "indicador",
        label = "Selecione o Indicador:",
        choices = c(
          "Casos de Malária (SIVEP)" = "total_casos",
          "Área Desmatada em km² (DETER)" = "area_desmatada_km2"
        ),
        selected = "total_casos"
      ),
      hr(),
      htmlOutput("painel_estatisticas")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("mapa_interativo", height = "700px")
    )
  )
)

# ------------------------------------------------------------------------------
# 5. SERVIDOR (SERVER)
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$mapa_interativo <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$Esri.WorldTerrain, group = "Terreno") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Mapa Claro") %>%
      addLayersControl(
        baseGroups = c("Satélite", "Terreno", "Mapa Claro"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(
        lng1 = as.numeric(bbox_amz["xmin"]), lat1 = as.numeric(bbox_amz["ymin"]),
        lng2 = as.numeric(bbox_amz["xmax"]), lat2 = as.numeric(bbox_amz["ymax"])
      )
  })
  
  dados_filtrados <- reactive({
    dados_shiny %>% filter(ano == input$ano_selecionado)
  })
  
  observe({
    df <- dados_filtrados()
    variavel <- input$indicador
    
    col_nome <- names(df)[grepl("NM_MUN|NM_MUNI|NAME|NOME", names(df), ignore.case = TRUE)][1]
    if (is.na(col_nome) || length(col_nome) == 0) col_nome <- "code_muni_6"
    
    nomes_municipios <- df[[col_nome]]
    
    if (variavel == "total_casos") {
      paleta <- colorNumeric(palette = "YlOrRd", domain = df$total_casos)
      valores <- df$total_casos
      titulo  <- "Total de Casos"
    } else {
      paleta <- colorNumeric(palette = "YlGnBu", domain = df$area_desmatada_km2)
      valores <- df$area_desmatada_km2
      titulo  <- "Desmatamento (km²)"
    }
    
    leafletProxy("mapa_interativo") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = df,
        fillColor = ~paleta(get(variavel)),
        fillOpacity = 0.55,
        color = "#ffffff",
        weight = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.85,
          bringToFront = TRUE
        ),
        popup = paste0(
          "<b>Município:</b> ", nomes_municipios, "<br>",
          "<b>Código IBGE:</b> ", df$code_muni_6, "<br>",
          "<b>Ano:</b> ", df$ano, "<br><hr>",
          "<b>Casos de Malária:</b> ", format(df$total_casos, big.mark = ".", decimal.mark = ","), "<br>",
          "<b>Vivax:</b> ", format(df$casos_vivax, big.mark = ".", decimal.mark = ","), " | ",
          "<b>Falciparum:</b> ", format(df$casos_falciparum, big.mark = ".", decimal.mark = ","), "<br>",
          "<b>Área Desmatada:</b> ", format(round(df$area_desmatada_km2, 2), big.mark = ".", decimal.mark = ","), " km²"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paleta,
        values = valores,
        title = titulo,
        opacity = 0.8
      )
  })
  
  output$painel_estatisticas <- renderUI({
    df <- dados_filtrados()
    
    tot_casos <- sum(df$total_casos, na.rm = TRUE)
    tot_desm  <- sum(df$area_desmatada_km2, na.rm = TRUE)
    
    HTML(paste0(
      "<h4><b>Resumo Regional (", input$ano_selecionado, ")</b></h4>",
      "<b>Total de Casos de Malária:</b> ", format(tot_casos, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Desmatamento Alerta (DETER):</b> ", format(round(tot_desm, 1), big.mark = ".", decimal.mark = ","), " km²<br>",
      "<b>Municípios Com Casos:</b> ", sum(df$total_casos > 0, na.rm = TRUE)
    ))
  })
}

# ------------------------------------------------------------------------------
# 6. EXECUÇÃO DO APLICATIVO
# ------------------------------------------------------------------------------
shinyApp(ui, server)
