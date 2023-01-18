######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (NORDESTE) ## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022


# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl)

# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/sih_estrangeiros.xlsx")


# MARANHAO -----------------------------------------------------------------

#Filtrar Base para Maranhão 
ma_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "MARANHÃO")

#Carregar GEOBR
ma_municipios <- geobr::read_municipality("MA", 
                                          year = 2020)

ma_municipios <- ma_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ma_municipios$code_muni, 1, 
                            nchar(ma_municipios$code_muni)-1))


#Mapa Maranhão 
ma_mapa <- ggplot(ma_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ma_mapa

#MERGE
ma_InternacoesEstrangeiros <- left_join(x = ma_municipios,
                                        y = ma_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ma_internacoes <- ma_InternacoesEstrangeiros |> 
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

ma_internacoes <- ma_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(ma_internacoes, "~\\COBRADI\\DATASUS\\bases\\ma_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ma_internacoes) |> 
  addTiles() 

#Mapa Maranhão
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
paleta <- colorBin("YlOrRd", domain = ma_internacoes$n_internacoes, bins = bins)

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
  ma_internacoes$name_muni, ma_internacoes$n_internacoes, ma_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ma_mapa <-  map |> 
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
ma_mapa


#Exportando

save...


# CEARÁ -----------------------------------------------------------------

#Filtrar Base para Ceará
ce_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "CEARÁ")

#Carregar GEOBR
ce_municipios <- geobr::read_municipality("CE", 
                                          year = 2020)

ce_municipios <- ce_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ce_municipios$code_muni, 1, 
                            nchar(ce_municipios$code_muni)-1))


#Mapa Ceará
ce_mapa <- ggplot(ce_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ce_mapa

#MERGE
ce_InternacoesEstrangeiros <- left_join(x = ce_municipios,
                                        y = ce_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ce_internacoes <- ce_InternacoesEstrangeiros |> 
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

ce_internacoes <- ce_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(ce_internacoes, "~\\COBRADI\\DATASUS\\bases\\ce_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ce_internacoes) |> 
  addTiles() 

#Mapa Ceará
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
paleta <- colorBin("YlOrRd", domain = ce_internacoes$n_internacoes, bins = bins)

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
  ce_internacoes$name_muni, ce_internacoes$n_internacoes, ce_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ce_mapa <-  map |> 
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
ce_mapa


#Exportando

save...

# RIO GRANDE DO NORTE ----------------------------------------------------------

#Filtrar Base para Rio Grande do Norte
rn_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "RIO GRANDE DO NORTE")

#Carregar GEOBR
rn_municipios <- geobr::read_municipality("RN", 
                                          year = 2020)

rn_municipios <- rn_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(rn_municipios$code_muni, 1, 
                            nchar(rn_municipios$code_muni)-1))


#Mapa Rio Grande do Norte
rn_mapa <- ggplot(rn_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
rn_mapa

#MERGE
rn_InternacoesEstrangeiros <- left_join(x = rn_municipios,
                                        y = rn_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
rn_internacoes <- rn_InternacoesEstrangeiros |> 
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

rn_internacoes <- rn_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(rn_internacoes, "~\\COBRADI\\DATASUS\\bases\\rn_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(rn_internacoes) |> 
  addTiles() 

#Mapa Rio Grande do Norte
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
paleta <- colorBin("YlOrRd", domain = rn_internacoes$n_internacoes, bins = bins)

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
  rn_internacoes$name_muni, rn_internacoes$n_internacoes, rn_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
rn_mapa <-  map |> 
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
rn_mapa


#Exportando

save...

# PARAIBA -----------------------------------------------------------------

#Filtrar Base para Paraíba
pb_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "PARAÍBA")

#Carregar GEOBR
pb_municipios <- geobr::read_municipality("PB", 
                                          year = 2020)

pb_municipios <- pb_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(pb_municipios$code_muni, 1, 
                            nchar(pb_municipios$code_muni)-1))


#Mapa Paraíba
pb_mapa <- ggplot(pb_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
pb_mapa

#MERGE
pb_InternacoesEstrangeiros <- left_join(x = pb_municipios,
                                        y = pb_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
pb_internacoes <- pb_InternacoesEstrangeiros |> 
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

pb_internacoes <- pb_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(pb_internacoes, "~\\COBRADI\\DATASUS\\bases\\pb_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(pb_internacoes) |> 
  addTiles() 

#Mapa Paraiba
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
paleta <- colorBin("YlOrRd", domain = pb_internacoes$n_internacoes, bins = bins)

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
  pb_internacoes$name_muni, pb_internacoes$n_internacoes, pb_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
pb_mapa <-  map |> 
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
pb_mapa


#Exportando

save...

# PERNAMBUCO -----------------------------------------------------------------

#Filtrar Base para Pernambuco
pe_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "PERNAMBUCO")

#Carregar GEOBR
pe_municipios <- geobr::read_municipality("PE", 
                                          year = 2020)

pe_municipios <- pe_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(pe_municipios$code_muni, 1, 
                            nchar(pe_municipios$code_muni)-1))


#Mapa Pernambuco
pe_mapa <- ggplot(pe_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
pe_mapa

#MERGE
pe_InternacoesEstrangeiros <- left_join(x = pe_municipios,
                                        y = pe_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
pe_internacoes <- pe_InternacoesEstrangeiros |> 
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

pe_internacoes <- pe_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(pe_internacoes, "~\\COBRADI\\DATASUS\\bases\\pe_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(pe_internacoes) |> 
  addTiles() 

#Mapa Pernambuco
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
paleta <- colorBin("YlOrRd", domain = pe_internacoes$n_internacoes, bins = bins)

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
  pe_internacoes$name_muni, pe_internacoes$n_internacoes, pe_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
pe_mapa <-  map |> 
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
pe_mapa


#Exportando

save...


# ALAGOAS -----------------------------------------------------------------

#Filtrar Base para Alagoas
al_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "ALAGOAS")

#Carregar GEOBR
al_municipios <- geobr::read_municipality("AL", 
                                          year = 2020)

al_municipios <- al_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(al_municipios$code_muni, 1, 
                            nchar(al_municipios$code_muni)-1))


#Mapa Alagoas
al_mapa <- ggplot(al_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
al_mapa

#MERGE
al_InternacoesEstrangeiros <- left_join(x = al_municipios,
                                        y = al_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
al_internacoes <- al_InternacoesEstrangeiros |> 
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

al_internacoes <- al_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(al_internacoes, "~\\COBRADI\\DATASUS\\bases\\al_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(al_internacoes) |> 
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
paleta <- colorBin("YlOrRd", domain = al_internacoes$n_internacoes, bins = bins)

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
  al_internacoes$name_muni, al_internacoes$n_internacoes, al_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
al_mapa <-  map |> 
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
al_mapa


#Exportando

save...

# SERGIPE -----------------------------------------------------------------

#Filtrar Base para Sergipe 
se_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "SERGIPE")

#Carregar GEOBR
se_municipios <- geobr::read_municipality("SE", 
                                          year = 2020)

se_municipios <- se_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(se_municipios$code_muni, 1, 
                            nchar(se_municipios$code_muni)-1))


#Mapa Sergipe
se_mapa <- ggplot(se_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
se_mapa

#MERGE
se_InternacoesEstrangeiros <- left_join(x = se_municipios,
                                        y = se_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
se_internacoes <- se_InternacoesEstrangeiros |> 
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

se_internacoes <- se_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(se_internacoes, "~\\COBRADI\\DATASUS\\bases\\se_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(se_internacoes) |> 
  addTiles() 

#Mapa Sergipe
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
paleta <- colorBin("YlOrRd", domain = se_internacoes$n_internacoes, bins = bins)

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
  se_internacoes$name_muni, se_internacoes$n_internacoes, se_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
se_mapa <-  map |> 
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
se_mapa


#Exportando

save...


# BAHIA -----------------------------------------------------------------

#Filtrar Base para Bahia 
ba_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "BAHIA")

#Carregar GEOBR
ba_municipios <- geobr::read_municipality("BA", 
                                          year = 2020)

ba_municipios <- ba_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ba_municipios$code_muni, 1, 
                            nchar(ba_municipios$code_muni)-1))


#Mapa Bahia
ba_mapa <- ggplot(ba_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ba_mapa

#MERGE
ba_InternacoesEstrangeiros <- left_join(x = ba_municipios,
                                        y = ba_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ba_internacoes <- ba_InternacoesEstrangeiros |> 
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

ba_internacoes <- ba_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(ba_internacoes, "~\\COBRADI\\DATASUS\\bases\\ba_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ba_internacoes) |> 
  addTiles() 

#Mapa Bahia
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
paleta <- colorBin("YlOrRd", domain = ba_internacoes$n_internacoes, bins = bins)

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
  ba_internacoes$name_muni, ba_internacoes$n_internacoes, ba_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ba_mapa <-  map |> 
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
ba_mapa


#Exportando

save...

# PIAUÍ -----------------------------------------------------------------

#Filtrar Base para Piauí
pi_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "PIAUÍ")

#Carregar GEOBR
pi_municipios <- geobr::read_municipality("PI", 
                                          year = 2020)

pi_municipios <- pi_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(pi_municipios$code_muni, 1, 
                            nchar(pi_municipios$code_muni)-1))


#Mapa Piauí
pi_mapa <- ggplot(pi_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
pi_mapa

#MERGE
pi_InternacoesEstrangeiros <- left_join(x = pi_municipios,
                                        y = pi_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
pi_internacoes <- pi_InternacoesEstrangeiros |> 
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

pi_internacoes <- pi_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(pi_internacoes, "~\\COBRADI\\DATASUS\\bases\\pi_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(pi_internacoes) |> 
  addTiles() 

#Mapa Pauí
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
paleta <- colorBin("YlOrRd", domain = pi_internacoes$n_internacoes, bins = bins)

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
  pi_internacoes$name_muni, pi_internacoes$n_internacoes, pi_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
pi_mapa <-  map |> 
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
pi_mapa


#Exportando

save...

