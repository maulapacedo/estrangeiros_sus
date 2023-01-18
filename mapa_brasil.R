######         SIH/DATASUS          ########
## MAPAS COM OS DADOS DE INTERNACOES EM 2021 (BRASIL) ## 
## Autora: Paula Macedo Barros 
## Data criação: 02/09/2022


# Carregar Pacotes -------------------------------------------------------------
pacman::p_load(htmlwidgets, sf, geobr, magrittr, dplyr, colorspace, writexl,
               ggplot2, gganimate, gifski, leaflet, readr, stringr, readxl)

# Carregar Base ----------------------------------------------------------------
sih_estrangeiros <- read_excel("~/COBRADI/DATASUS/bases/sih_estrangeiros.xlsx")

# BRASIL

#Filtrar Base 
br_sih_estrangeiros <- sih_estrangeiros 


#Carregar GEOBR
br_municipios <- geobr::read_municipality(code_muni = "all",
                                          year = 2020)

br_municipios <- br_municipios |> 
  mutate(code_muni = as.character(code_muni)) |> 
  mutate(code_muni = substr(br_municipios$code_muni, 1, 
                            nchar(br_municipios$code_muni)-1)) |> 
  mutate(name_muni = str_to_upper(name_muni)) |> 
  mutate(name_state = str_to_upper(name_state))


#Mapa Brasil
br_mapa <- ggplot(br_municipios) +
  geom_sf(fill = "#2D3E50", color = "#FEBF57", size = .15, 
          show.legend = FALSE)
br_mapa

#MERGE
br_InternacoesEstrangeiros <- left_join(x = br_sih_estrangeiros,
                                        y = br_municipios, 
                                        by = c("munResNome" = "name_muni",
                                               "munResUf" = "name_state"))

br_InternacoesEstrangeiros <-right_join(x = br_municipios,
                                        y = br_sih_estrangeiros, 
                                        by = c("code_muni" = "MUNIC_RES"))



#Gastos e quantidade de internacoes
br_internacoes <-  br_InternacoesEstrangeiros |> 
  select(code_muni, VAL_TOT, munResUf, munResNome) |> 
  group_by(code_muni) |> 
  summarise(n_internacoes = n(),
            gasto = sum(VAL_TOT),
            media_gasto = (sum(VAL_TOT)/n_internacoes), 
            media_gasto = round(media_gasto, digits = 2))



#Formatar o gasto para real 
format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

#Base final
br_internacoes <- br_internacoes %>% 
  mutate(gasto = format_real(gasto),
         media_gasto = format_real(media_gasto))


write_xlsx(br_internacoes, "~\\COBRADI\\DATASUS\\bases\\br_internacoes.xlsx")


#Montando os mapas das internacoes 
map <- leaflet(br_internacoes) |> 
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
bins <- c(0, 1000, 2000, 5000, 10000, 50000, 100000, 200000, 500000, 1000000, Inf)
paleta <- colorBin("YlOrRd", domain = br_internacoes$n_internacoes, bins = bins)

#Adicionar interatividade 
map |> 
  addPolygons(
    fillColor = ~paleta(gasto),
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
  "<strong>%s</strong></br>%g Gasto em Internações</br>%s Quantidade de Internações",
  br_internacoes$code_muni, br_internacoes$gasto, br_internacoes$n_internacoes
) |> 
  lapply(htmltools::HTML)


#Legenda 
br_mapa <-  map |> 
  addPolygons(
    fillColor = ~paleta(gasto),
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
            title = "Gastos em Internações",
            position = "bottomright")
br_mapa


#Exportando

save...
