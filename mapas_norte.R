######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (NORTE) ## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022


# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl, gt)

# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/SIH-RD/sih_estrangeiros.xlsx")

#Mapas Norte -------------------------------------------------------------------

# RONDÔNIA ---------------------------------------------------------------------

#Filtrar Base para Rondônia
ro_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "RONDÔNIA")

#Carregar GEOBR
ro_municipios <- geobr::read_municipality("RO", 
                                          year = 2020)

ro_municipios <- ro_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ro_municipios$code_muni, 1, 
                            nchar(ro_municipios$code_muni)-1))


#Mapa Mato Grosso do Sul
ro_mapa <- ggplot(ro_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ro_mapa

#MERGE
ro_InternacoesEstrangeiros <- left_join(x = ro_municipios,
                                        y = ro_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ro_internacoes <- ro_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

ro_internacoes <- ro_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base ro 
write_xlsx(ro_internacoes, "~\\COBRADI\\DATASUS\\bases\\ro_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ro_internacoes) |> 
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
paleta <- colorBin("YlOrRd", domain = ro_internacoes$n_internacoes, bins = bins)

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
  ro_internacoes$name_muni, ro_internacoes$n_internacoes, ro_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ro_mapa <- map |> 
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
ro_mapa

#Tabela Gasto por Nacionalidade 
tabela_ro <- ro_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(ro_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Rondônia**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_ro

# ACRE -------------------------------------------------------------------------

#Filtrar Base para ACRE
ac_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "ACRE")

#Carregar GEOBR
ac_municipios <- geobr::read_municipality("AC", 
                                          year = 2020)

ac_municipios <- ac_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ac_municipios$code_muni, 1, 
                            nchar(ac_municipios$code_muni)-1))


#Mapa Mato Gacsso do Sul
ac_mapa <- ggplot(ac_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ac_mapa

#MERGE
ac_Internacoesestrangeiros <- left_join(x = ac_municipios,
                                        y = ac_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ac_internacoes <- ac_Internacoesestrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

ac_internacoes <- ac_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base ac 
write_xlsx(ac_internacoes, "~\\COBRADI\\DATASUS\\bases\\ac_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ac_internacoes) |> 
  addTiles() 

#Mapa Acre
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
paleta <- colorBin("YlOrRd", domain = ac_internacoes$n_internacoes, bins = bins)

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
  "<stacng>%s</stacng></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  ac_internacoes$name_muni, ac_internacoes$n_internacoes, ac_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ac_mapa <-  map |> 
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
ac_mapa

#Tabela Gasto por Nacionalidade 
tabela_ac <- ac_Internacoesestrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(ac_Internacoesestrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade do Acre**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_ac

# AMAZONAS ---------------------------------------------------------------------

#Filtrar Base para Amazonas
am_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "AMAZONAS")

#Carregar GEOBR
am_municipios <- geobr::read_municipality("AM", 
                                          year = 2020)

am_municipios <- am_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(am_municipios$code_muni, 1, 
                            nchar(am_municipios$code_muni)-1))


#Mapa Amazonas
am_mapa <- ggplot(am_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
am_mapa

#MERGE
am_InternacoesEstrangeiros <- left_join(x = am_municipios,
                                        y = am_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
am_internacoes <- am_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

am_internacoes <- am_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base am
write_xlsx(am_internacoes, "~\\COBRADI\\DATASUS\\bases\\am_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(am_internacoes) |> 
  addTiles() 

#Mapa Amazonas
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
paleta <- colorBin("YlOrRd", domain = am_internacoes$n_internacoes, bins = bins)

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
  am_internacoes$name_muni, am_internacoes$n_internacoes, am_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
am_mapa <-  map |> 
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
am_mapa

#Tabela Gasto por Nacionalidade 
tabela_am <- am_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(am_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Amazonas**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_am

# RORAIMA --------------------------------------------------------------

#Filtrar Base para Roraima
rr_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "RORAIMA")

#Carregar GEOBR
rr_municipios <- geobr::read_municipality("RR", 
                                          year = 2020)

rr_municipios <- rr_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(rr_municipios$code_muni, 1, 
                            nchar(rr_municipios$code_muni)-1))


#Mapa Roraima
rr_mapa <- ggplot(rr_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
rr_mapa

#MERGE
rr_InternacoesEstrangeiros <- left_join(x = rr_municipios,
                                        y = rr_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
rr_internacoes <- rr_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

rr_internacoes <- rr_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base RR
write_xlsx(rr_internacoes, "~\\COBRADI\\DATASUS\\bases\\rr_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(rr_internacoes) |> 
  addTiles() 

#Mapa Roraima
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
paleta <- colorBin("YlOrRd", domain = rr_internacoes$n_internacoes, bins = bins)

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
  rr_internacoes$name_muni, rr_internacoes$n_internacoes, rr_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
rr_mapa <-  map |> 
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
rr_mapa

#Tabela Gasto por Nacionalidade 
tabela_rr <- rr_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(rr_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Roraima**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_rr

# PARÁ -------------------------------------------------------------------------

#Filtrar Base para PARÁ
pa_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "PARÁ")

#Carregar GEOBR
pa_municipios <- geobr::read_municipality("PA", 
                                          year = 2020)

pa_municipios <- pa_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(pa_municipios$code_muni, 1, 
                            nchar(pa_municipios$code_muni)-1))


#Mapa Pará
pa_mapa <- ggplot(pa_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
pa_mapa

#MERGE
pa_InternacoesEstrangeiros <- left_join(x = pa_municipios,
                                        y = pa_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
pa_internacoes <- pa_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

pa_internacoes <- pa_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base pa
write_xlsx(pa_internacoes, "~\\COBRADI\\DATASUS\\bases\\pa_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(pa_internacoes) |> 
  addTiles() 

#Mapa Interativo Pará
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
paleta <- colorBin("YlOrRd", domain = pa_internacoes$n_internacoes, bins = bins)

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
  pa_internacoes$name_muni, pa_internacoes$n_internacoes, pa_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
pa_mapa <-  map |> 
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
pa_mapa

#Tabela Gasto por Nacionalidade 
tabela_pa <- pa_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(pa_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Pará**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_pa

# AMAPÁ ------------------------------------------------------------------------

#Filtrar Base para AMAPÁ
ap_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "AMAPÁ")

#Carregar GEOBR
ap_municipios <- geobr::read_municipality("AP", 
                                          year = 2020)

ap_municipios <- ap_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(ap_municipios$code_muni, 1, 
                            nchar(ap_municipios$code_muni)-1))


#Mapa Amapá
ap_mapa <- ggplot(ap_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
ap_mapa

#MERGE
ap_InternacoesEstrangeiros <- left_join(x = ap_municipios,
                                        y = ap_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
ap_internacoes <- ap_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

ap_internacoes <- ap_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base AP
write_xlsx(ap_internacoes, "~\\COBRADI\\DATASUS\\bases\\ap_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(ap_internacoes) |> 
  addTiles() 

#Mapa Amapá
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colors - divisão dos casos que ocorrerap 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = ap_internacoes$n_internacoes, bins = bins)

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
  ap_internacoes$name_muni, ap_internacoes$n_internacoes, ap_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
ap_mapa <-  map |> 
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
ap_mapa

#Tabela Gasto por Nacionalidade 
tabela_ap <- ap_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(ap_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Amapá**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_ap


# TOCANTINS --------------------------------------------------------------------

#Filtrar Base para Tocantins
to_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "TOCANTINS")

#Carregar GEOBR
to_municipios <- geobr::read_municipality("TO", 
                                          year = 2020)

to_municipios <- to_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(to_municipios$code_muni, 1, 
                            nchar(to_municipios$code_muni)-1))


#Mapa Tocantins
to_mapa <- ggplot(to_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
to_mapa

#MERGE
to_InternacoesEstrangeiros <- left_join(x = to_municipios,
                                        y = to_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
to_internacoes <- to_InternacoesEstrangeiros |> 
  group_by(name_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT)) |> 
  mutate(gasto = ifelse(is.na(gasto), "Desconhecido", gasto),
         n_internacoes = ifelse(n_internacoes == 1 & gasto == "Desconhecido", 0, n_internacoes))

#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

to_internacoes <- to_internacoes %>% 
  mutate(gasto = format_real(gasto))


#Salvar base to 
write_xlsx(to_internacoes, "~\\COBRADI\\DATASUS\\bases\\to_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(to_internacoes) |> 
  addTiles() 

#Mapa Mato Gtosso do Sul
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
paleta <- colorBin("YlOrRd", domain = to_internacoes$n_internacoes, bins = bins)

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
  "<sttong>%s</sttong></br>%g Quantidade de Internações</br>%s Gasto em Internações",
  to_internacoes$name_muni, to_internacoes$n_internacoes, to_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
to_mapa <-  map |> 
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
to_mapa

#Tabela Gasto por Nacionalidade 
tabela_to <- to_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(to_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Tocantins**"),
    subtitle = md("**Ano: 2021**")
  ) |> 
  fmt_percent(
    columns = porcentagem, decimals = 2
  ) |> 
  cols_label(
    contagem = md("**Quantidade de Estrangeiros Internados**"), 
    valor_gasto = md("**Valor Gasto em Internações (R$)**"),
    porcentagem = md("**Porcentagem de Internação por Nacionalidade**")
  ) |> 
  opt_align_table_header(align = "left") |> 
  fmt_number(columns = 3) |> 
  cols_width(
    NACIONAL~px(350),
    contagem~px(200),
    valor_gasto~px(200),
    porcentagem~px(200)
  ) |> 
  tab_source_note(source_note = md("*Fonte: Elaboração Própria*")
  ) |> 
  opt_table_font(
    font = google_font("Times New Roman"), 
    weight = 600 
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  )

tabela_to

