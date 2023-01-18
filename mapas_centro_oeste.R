######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (CENTRO-OESTE) ## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022


# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl)

# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/sih_estrangeiros.xlsx")

# MATO GROSSO DO SUL -----------------------------------------------------------

#Filtrar Base para Mato Grosso do Sul
ms_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "MATO GROSSO DO SUL")

#Carregar GEOBR
ms_municipios <- geobr::read_municipality("MS", 
                                          year = 2020)

ms_municipios <- ms_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ms_municipios$code_muni, 1, 
                            nchar(ms_municipios$code_muni)-1))


#Mapa Mato Grosso do Sul
ms_mapa <- ggplot(ms_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ms_mapa

#MERGE
ms_InternacoesEstrangeiros <- left_join(x = ms_municipios,
                                        y = ms_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ms_internacoes <- ms_InternacoesEstrangeiros |> 
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

ms_internacoes <- ms_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base MS 
write_xlsx(ms_internacoes, "~\\COBRADI\\DATASUS\\bases\\ms_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ms_internacoes) |> 
  addTiles() 

#Mapa Mato Grosso do Sul
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
paleta <- colorBin("YlOrRd", domain = ms_internacoes$n_internacoes, bins = bins)

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
  ms_internacoes$name_muni, ms_internacoes$n_internacoes, ms_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ms_mapa <-  map |> 
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
ms_mapa


# MATO GROSSO -------------------------------------------------------------------

#Filtrar Base para Mato Grosso 
mt_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "MATO GROSSO")

#Carregar GEOBR
mt_municipios <- geobr::read_municipality("MT", 
                                          year = 2020)

mt_municipios <- mt_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(mt_municipios$code_muni, 1, 
                            nchar(mt_municipios$code_muni)-1))


#Mapa Mato Grosso 
mt_mapa <- ggplot(mt_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
mt_mapa

#MERGE
mt_InternacoesEstrangeiros <- left_join(x = mt_municipios,
                                        y = mt_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
mt_internacoes <- mt_InternacoesEstrangeiros |> 
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

mt_internacoes <- mt_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base MT
write_xlsx(mt_internacoes, "~\\COBRADI\\DATASUS\\bases\\mt_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(mt_internacoes) |> 
  addTiles() 

#Mapa Mato Grosso 
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
paleta <- colorBin("YlOrRd", domain = mt_internacoes$n_internacoes, bins = bins)

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
  mt_internacoes$name_muni, mt_internacoes$n_internacoes, mt_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
mt_mapa <-  map |> 
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
mt_mapa


# GOIÁS -------------------------------------------------------------------------

#Filtrar Base para GOIÁS
go_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "GOIÁS")

#Carregar GEOBR
go_municipios <- geobr::read_municipality("GO", 
                                          year = 2020)

go_municipios <- go_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(go_municipios$code_muni, 1, 
                            nchar(go_municipios$code_muni)-1))


#Mapa Mato Grosso 
go_mapa <- ggplot(go_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
go_mapa

#MERGE
go_InternacoesEstrangeiros <- left_join(x = go_municipios,
                                        y = go_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
go_internacoes <- go_InternacoesEstrangeiros |> 
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

go_internacoes <- go_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base go
write_xlsx(go_internacoes, "~\\COBRADI\\DATASUS\\bases\\go_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(go_internacoes) |> 
  addTiles() 

#Mapa Mato Grosso 
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
paleta <- colorBin("YlOrRd", domain = go_internacoes$n_internacoes, bins = bins)

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
  go_internacoes$name_muni, go_internacoes$n_internacoes, go_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
go_mapa <-  map |> 
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
go_mapa



# DISTRITO FEDERAL --------------------------------------------------------------

#Filtrar Base para DISTRITO FEDERAL
df_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "DISTRITO FEDERAL")

#Carregar GEOBR
df_municipios <- geobr::read_municipality("DF", 
                                          year = 2020)

df_municipios <- df_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(df_municipios$code_muni, 1, 
                            nchar(df_municipios$code_muni)-1))


#Mapa Distrito Federal
df_mapa <- ggplot(df_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
df_mapa

#MERGE
df_InternacoesEstrangeiros <- left_join(x = df_municipios,
                                        y = df_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
df_internacoes <- df_InternacoesEstrangeiros |> 
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

df_internacoes <- df_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base DF
write_xlsx(df_internacoes, "~\\COBRADI\\DATASUS\\bases\\df_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(df_internacoes) |> 
  addTiles() 

#Mapa Distrito Federal
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
paleta <- colorBin("YlOrRd", domain = df_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolydfns(
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
  df_internacoes$name_muni, df_internacoes$n_internacoes, df_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
df_mapa <-  map |> 
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
df_mapa
