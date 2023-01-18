######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (SUDESTE) ## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022

# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl)

# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/sih_estrangeiros.xlsx")


# SÃO PAULO --------------------------------------------------------------------

#Filtrar Base para São Paulo
sp_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "SÃO PAULO")

#Carregar GEOBR
sp_municipios <- geobr::read_municipality("SP", 
                                          year = 2020)

sp_municipios <- sp_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(sp_municipios$code_muni, 1, 
                            nchar(sp_municipios$code_muni)-1))


#Mapa São Paulo
sp_mapa <- ggplot(sp_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
sp_mapa

#MERGE
sp_InternacoesEstrangeiros <- left_join(x = sp_municipios,
                                        y = sp_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
sp_internacoes <- sp_InternacoesEstrangeiros |> 
  select(code_muni, VAL_TOT, name_muni) |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto)) |> 
  ungroup() |> 
  group_by(name_muni)

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

sp_internacoes <- sp_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(sp_internacoes, "~\\COBRADI\\DATASUS\\bases\\sp_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(sp_internacoes) |> 
  addTiles() 

#Mapa São Paulo
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colors - divisão dos casos que ocorreram 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = sp_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    )
  )

#Customizando informacoes
labels <- sprintf(
  "<strong>%s</strong></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  sp_internacoes$name_muni, sp_internacoes$n_internacoes, sp_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
sp_mapa <-  map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    ),
    label = labels, 
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |> 
  addLegend(pal = paleta,
            values = ~n_internacoes,
            opacity = 0.7,
            title = "Quantidade de Internações",
            position = "bottomright")
sp_mapa


#Exportando

save...



# MINAS GERAIS -----------------------------------------------------------------

#Filtrar Base para Minas Gerais 
mg_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "MINAS GERAIS")

#Carregar GEOBR
mg_municipios <- geobr::read_municipality("MG", 
                                          year = 2020)

mg_municipios <- mg_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(mg_municipios$code_muni, 1, 
                            nchar(mg_municipios$code_muni)-1))


#Mapa Minas Gerais
mg_mapa <- ggplot(mg_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
mg_mapa

#MERGE
mg_InternacoesEstrangeiros <- left_join(x = mg_municipios,
                                        y = mg_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
mg_internacoes <- mg_InternacoesEstrangeiros |> 
  select(code_muni, VAL_TOT, name_muni) |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto)) |> 
  ungroup() |> 
  group_by(name_muni)

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

mg_internacoes <- mg_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(mg_internacoes, "~\\COBRADI\\DATASUS\\bases\\mg_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(mg_internacoes) |> 
  addTiles() 

#Mapa Minas Gerais
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colors - divisão dos casos que ocorreram 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = mg_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    )
  )

#Customizando informacoes
labels <- sprintf(
  "<strong>%s</strong></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  mg_internacoes$name_muni, mg_internacoes$n_internacoes, mg_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
mg_mapa <-  map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    ),
    label = labels, 
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |> 
  addLegend(pal = paleta,
            values = ~n_internacoes,
            opacity = 0.7,
            title = "Quantidade de Internações",
            position = "bottomright")
mg_mapa


#Exportando

save...



# ESPÍRITO SANTO ---------------------------------------------------------------

#Filtrar Base para Espírito Santo
es_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "ESPÍRITO SANTO")

#Carregar GEOBR
es_municipios <- geobr::read_municipality("ES", 
                                          year = 2020)

es_municipios <- es_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(es_municipios$code_muni, 1, 
                            nchar(es_municipios$code_muni)-1))


#Mapa Minas Gerais
es_mapa <- ggplot(es_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
es_mapa

#MERGE
es_InternacoesEstrangeiros <- left_join(x = es_municipios,
                                        y = es_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
es_internacoes <- es_InternacoesEstrangeiros |> 
  select(code_muni, VAL_TOT, name_muni) |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto)) |> 
  ungroup() |> 
  group_by(name_muni)

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

es_internacoes <- es_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(es_internacoes, "~\\COBRADI\\DATASUS\\bases\\es_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(es_internacoes) |> 
  addTiles() 

#Mapa Minas Gerais
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colors - divisão dos casos que ocorreram 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = es_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    )
  )

#Customizando informacoes
labels <- sprintf(
  "<strong>%s</strong></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  es_internacoes$name_muni, es_internacoes$n_internacoes, es_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
es_mapa <-  map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    ),
    label = labels, 
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |> 
  addLegend(pal = paleta,
            values = ~n_internacoes,
            opacity = 0.7,
            title = "Quantidade de Internações",
            position = "bottomright")
es_mapa


#Exportando

save...

# RIO DE JANEIRO ---------------------------------------------------------------

#Filtrar Base para Rio de Janeiro 
rj_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "RIO DE JANEIRO")

#Carregar GEOBR
rj_municipios <- geobr::read_municipality("RJ", 
                                          year = 2020)

rj_municipios <- rj_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(rj_municipios$code_muni, 1, 
                            nchar(rj_municipios$code_muni)-1))


#Mapa Rio de Janeiro
rj_mapa <- ggplot(rj_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
rj_mapa

#MERGE
rj_InternacoesEstrangeiros <- left_join(x = rj_municipios,
                                        y = rj_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
rj_internacoes <- rj_InternacoesEstrangeiros |> 
  select(code_muni, VAL_TOT, name_muni) |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto)) |> 
  ungroup() |> 
  group_by(name_muni)

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

rj_internacoes <- rj_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(rj_internacoes, "~\\COBRADI\\DATASUS\\bases\\rj_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(rj_internacoes) |> 
  addTiles() 

#Mapa Rio de Janeiro
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colors - divisão dos casos que ocorreram 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = rj_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    )
  )

#Customizando informacoes
labels <- sprintf(
  "<strong>%s</strong></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  rj_internacoes$name_muni, rj_internacoes$n_internacoes, rj_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
rj_mapa <-  map |> 
  addPolygons(
    fillColor = ~paleta(n_internacoes),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    ),
    label = labels, 
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |> 
  addLegend(pal = paleta,
            values = ~n_internacoes,
            opacity = 0.7,
            title = "Quantidade de Internações",
            position = "bottomright")
rj_mapa


#Exportando

save...