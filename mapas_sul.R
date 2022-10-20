######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (SUL)## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022



# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl, gt)



# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/SIH-RD/sih_estrangeiros.xlsx")



# MAPAS SUL --------------------------------------------------------------------

# SANTA CATARINA ---------------------------------------------------------------

#Filtrar Base para Santa Catarina
sc_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "SANTA CATARINA")

#Carregar GEOBR
sc_municipios <- geobr::read_municipality("SC", 
                                          year = 2020)

sc_municipios <- sc_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(sc_municipios$code_muni, 1, 
                            nchar(sc_municipios$code_muni)-1))


#Mapa Santa Catarina
sc_mapa <- ggplot(sc_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
sc_mapa

#MERGE
sc_InternacoesEstrangeiros <- left_join(x = sc_municipios,
                                        y = sc_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
sc_internacoes <- sc_InternacoesEstrangeiros |> 
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

sc_internacoes <- sc_internacoes %>% 
                    mutate(gasto = format_real(gasto))


write_xlsx(sc_internacoes, "~\\COBRADI\\DATASUS\\bases\\sc_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(sc_internacoes) |> 
  addTiles() 

#Mapa Santa Catarina 
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
paleta <- colorBin("YlOrRd", domain = sc_internacoes$n_internacoes, bins = bins)

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
  "<strong>%s</strong></br>%g Internações</br>%s Gasto em Internações",
  sc_internacoes$name_muni, sc_internacoes$n_internacoes, sc_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
sc_mapa <-  map |> 
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
sc_mapa


#Tabela Gasto por Nacionalidade 
tabela_sc <- sc_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(sc_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade de Santa Catarina**"),
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

tabela_sc


# RIO GRANDE DO SUL -------------------------------------------------------------
rs_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "RIO GRANDE DO SUL")

#Carregar GEOBR
rs_municipios <- geobr::read_municipality("RS", 
                                          year = 2020)

rs_municipios <- rs_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(rs_municipios$code_muni, 1, 
                            nchar(rs_municipios$code_muni)-1))


#Mapa Rio Grande do Sul 
rs_mapa <- ggplot(rs_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)

rs_mapa

#Merge 
rs_InternacoesEstrangeiros <- left_join(x = rs_municipios,
                                        y = rs_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
rs_internacoes <- rs_InternacoesEstrangeiros |> 
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


rs_internacoes <- rs_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(rs_internacoes, "~\\COBRADI\\DATASUS\\bases\\rs_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(rs_internacoes) |> 
  addTiles() 

#Mapa Rio Grande do Sul
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
paleta <- colorBin("YlOrRd", domain = rs_internacoes$n_internacoes, bins = bins)

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
  "<strong>%s</strong></br>%g Internações</br>%s Gasto em Internações",
  rs_internacoes$name_muni, rs_internacoes$n_internacoes, rs_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
rs_mapa <-  map |> 
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
rs_mapa

#Tabela Gasto por Nacionalidade 
rs_tabela <- rs_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(rs_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade do Rio Grande do Sul**"),
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

rs_tabela

# PARANA ------------------------------------------------------------------------
pr_sih_estrangeiros <- sih_estrangeiros |> 
  filter(munResUf == "PARANÁ")

#Carregar GEOBR
pr_municipios <- geobr::read_municipality("PR", 
                                          year = 2020)

pr_municipios <- pr_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(pr_municipios$code_muni, 1, 
                            nchar(pr_municipios$code_muni)-1))


#Mapa Parana
mapa_pr <- ggplot(pr_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
mapa_pr

#Merge 
pr_InternacoesEstrangeiros <- left_join(x = pr_municipios,
                                        y = pr_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))


#Gastos e quantidade de internacoes
pr_internacoes <- pr_InternacoesEstrangeiros |> 
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

pr_internacoes <- pr_internacoes %>% 
  mutate(gasto = format_real(gasto))


write_xlsx(pr_internacoes, "~\\COBRADI\\DATASUS\\bases\\pr_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(pr_internacoes) |> 
  addTiles() 

#Mapa Santa Catarina 
map |> 
  addPolygons(
    weight = 1,
    opacity = 0.5,
    color = "blue",
    dashArray = "1",
    fillOpacity = 0
  )

#Bins e Colopr - divisão dos casos que ocorreram 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
paleta <- colorBin("YlOrRd", domain = pr_internacoes$n_internacoes, bins = bins)

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
  "<strong>%s</strong></br>%g Internações</br>%s Gasto em Internações",
  pr_internacoes$name_muni, pr_internacoes$n_internacoes, pr_internacoes$gasto
) |> 
  lapply(htmltools::HTML)


#Legenda 
pr_mapa <-  map |> 
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
pr_mapa

#Tabela Gasto por Nacionalidade 
pr_tabela <- pr_InternacoesEstrangeiros |> 
  st_drop_geometry() |> 
  group_by(NACIONAL) |> 
  dplyr::summarize(contagem = n(), 
                   valor_gasto = sum(VAL_TOT),
                   porcentagem = n()/nrow(pr_InternacoesEstrangeiros))|> 
  arrange(desc(contagem)) |> 
  gt(rowname_col = "Nacionalidade") |>  
  tab_header(
    title = md("**Internação por Nacionalidade do Paraná**"),
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

pr_tabela



